# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Shiny App: Austin Neighborhood Trends Dashboard
# OPTIMIZED VERSION - Faster loading with parallel processing
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(shiny)
library(tidyverse)
library(tidycensus)
library(tigris)
library(ggplot2)
library(sf)
library(leaflet)
library(scales)
library(tidygeocoder)
library(RSocrata)
library(memoise)  # For caching API calls
library(plotly)
library(waiter)

options(tigris_use_cache = TRUE)
census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE, overwrite = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. CONFIG ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
years_to_pull <- 2009:2023
zcta_years <- 2011:2023  # ZCTA data only available from 2011+

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. ACS VARIABLES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
income_vars <- c(median_income = "B19013_001")
pop_var <- c(total_pop = "B03002_001")
race_vars <- c(
  white_nh = "B03002_003",
  black_nh = "B03002_004",
  asian_nh = "B03002_006",
  hispanic = "B03002_012",
  aian_nh = "B03002_005",
  multiracial_nh = "B03002_009",
  multiracial_h = "B03002_019",
  nhpi_nh = "B03002_007",
  sor_nh = "B03002_008"
)
age_vars <- c(
  # Under 18 (male + female)
  male_under18_1 = "B01001_003",   # Male under 5
  male_under18_2 = "B01001_004",   # Male 5-9
  male_under18_3 = "B01001_005",   # Male 10-14
  male_under18_4 = "B01001_006",   # Male 15-17
  female_under18_1 = "B01001_027", # Female under 5
  female_under18_2 = "B01001_028", # Female 5-9
  female_under18_3 = "B01001_029", # Female 10-14
  female_under18_4 = "B01001_030", # Female 15-17
  
  # 18-34 (male + female)
  male_18_19 = "B01001_007",
  male_20 = "B01001_008",
  male_21 = "B01001_009",
  male_22_24 = "B01001_010",
  male_25_29 = "B01001_011",
  male_30_34 = "B01001_012",
  female_18_19 = "B01001_031",
  female_20 = "B01001_032",
  female_21 = "B01001_033",
  female_22_24 = "B01001_034",
  female_25_29 = "B01001_035",
  female_30_34 = "B01001_036",
  
  # 35-64 (male + female)
  male_35_39 = "B01001_013",
  male_40_44 = "B01001_014",
  male_45_49 = "B01001_015",
  male_50_54 = "B01001_016",
  male_55_59 = "B01001_017",
  male_60_61 = "B01001_018",
  male_62_64 = "B01001_019",
  female_35_39 = "B01001_037",
  female_40_44 = "B01001_038",
  female_45_49 = "B01001_039",
  female_50_54 = "B01001_040",
  female_55_59 = "B01001_041",
  female_60_61 = "B01001_042",
  female_62_64 = "B01001_043",
  
  # 65+ (male + female)
  male_65_66 = "B01001_020",
  male_67_69 = "B01001_021",
  male_70_74 = "B01001_022",
  male_75_79 = "B01001_023",
  male_80_84 = "B01001_024",
  male_85plus = "B01001_025",
  female_65_66 = "B01001_044",
  female_67_69 = "B01001_045",
  female_70_74 = "B01001_046",
  female_75_79 = "B01001_047",
  female_80_84 = "B01001_048",
  female_85plus = "B01001_049"
)

# Combine all variables for single API call
all_vars <- c(income_vars, race_vars, age_vars, pop_var)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. HELPER FUNCTIONS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summarizeRace <- function(df) {
  df %>%
    st_drop_geometry() %>% 
    mutate(
      race_collapsed = case_when(
        variable == "white_nh" ~ "White NH",
        variable == "black_nh" ~ "Black",
        variable == "asian_nh" ~ "Asian",
        variable %in% c("hispanic", "multiracial_h") ~ "Hispanic",
        variable == "aian_nh" ~ "AIAN",
        variable %in% c("multiracial_nh","nhpi_nh","sor_nh") ~ "Other/Multiracial",
        TRUE ~ "Other"
      )
    ) %>% 
    filter(race_collapsed != "Other") %>% 
    # group_by(year, race_collapsed) %>%
    # summarise(total = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
    # group_by(year) %>%
    # mutate(prop = total / sum(total)) %>%
    # ungroup()
  
  group_by(year, race_collapsed) %>%
    summarise(tot_group = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
    group_by(year) %>% 
    mutate(total = sum(tot_group),
           prop = tot_group / total) %>%
    ungroup()
}

summarizeAge <- function(df){
  df %>% 
    st_drop_geometry() %>% 
    mutate(age_collapse = case_when(
      # Under 18
      str_detect(variable, "under18") ~ "Under 18",
      
      # 18-24
      str_detect(variable, "18_19|_20$|_21$|22_24") ~ "18-24",
      
      # 25-39
      str_detect(variable, "25_29|30_34|35_39") ~ "25-39",
      
      # 40-64
      str_detect(variable, "40_44|45_49|50_54|55_59|60_61|62_64") ~ "40-64",
      
      # 65 and over
      str_detect(variable, "65_66|67_69|70_74|75_79|80_84|85plus") ~ "65 and over",
      
      TRUE ~ "Other"
    )) %>%
    filter(age_collapse != "Other") %>%  # Remove any that don't match
    group_by(year, GEOID, age_collapse) %>%
    summarise(tot_group = sum(estimate)) %>%
    mutate(total = sum(tot_group),
           prop = tot_group / total) %>%
    ungroup()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. OPTIMIZED DATA LOADING ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cache get_acs calls across sessions
cached_get_acs <- memoise(get_acs)

# Parallel data fetching function
safe_get_parallel <- function(years, vars, geo, geo_filter = NULL) {
  
  # Check if furrr is available for parallel processing
  has_furrr <- requireNamespace("furrr", quietly = TRUE)
  
  if (has_furrr) {
    cat("Using parallel processing (4 workers)...\n")
    future::plan(future::multisession, workers = 3)
    map_func <- furrr::future_map
  } else {
    cat("Install 'furrr' package for faster loading. Using sequential processing...\n")
    map_func <- purrr::map
  }
  
  results <- map_func(years, function(year) {
    tryCatch({
      if (geo == "tract" & year < 2020) {
        df <- cached_get_acs(
          geography = geo,
          variables = vars,
          year = year,
          survey = "acs5",
          state = "TX",
          county = "Travis",
          geometry = FALSE,
          cache_table = TRUE,
          show_call = FALSE
        )
        if (!is.null(geo_filter)) {
          df <- df %>% filter(GEOID %in% geo_filter)
        }
      } else if (geo == "tract" & year >= 2020){
        df <- get_acs(
          geography = geo,
          variables = vars,
          year = year,
          survey = "acs5",
          state = "TX",
          county = "Travis",
          geometry = FALSE,
          cache_table = FALSE,
          show_call = FALSE
        )
        if (!is.null(geo_filter)) {
          df <- df %>% filter(GEOID %in% geo_filter)
        }
      } else if (geo == "zcta" & year < 2020) {
        df <- cached_get_acs(
          geography = geo,
          variables = vars,
          year = year,
          survey = "acs5",
          state = "TX",
          zcta = geo_filter,
          cache_table = TRUE,
          show_call = FALSE
        )
      } else if (geo == "zcta" & year >= 2020) {
        df <- cached_get_acs(
          geography = geo,
          variables = vars,
          year = year,
          survey = "acs5",
          zcta = geo_filter,
          cache_table = TRUE,
          show_call = FALSE
        )
      }
      df$year <- year
      return(df)
    }, error = function(e) {
      return(NULL)
    })
  }, .progress = FALSE)
  
  if (has_furrr) {
    future::plan(future::sequential)
  }
  
  bind_rows(compact(results))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. LOAD BOUNDARIES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("Loading Austin boundaries...\n")
austin_boundary <- places(state = "TX", year = 2023, class = "sf") %>%
  filter(str_detect(NAME, regex("^Austin$", ignore_case = TRUE))) %>%
  st_transform(4326)

austin_center <- st_centroid(austin_boundary) %>% st_coordinates()

cat("Loading ZCTA shapes...\n")
zcta_austin <- read.socrata("https://data.austintexas.gov/api/odata/v4/49ja-3mqz") %>% 
  st_as_sf(wkt = "the_geom") 
st_crs(zcta_austin) <- 4326

cat("Loading tract boundaries (2023 only)...\n")
# Only get geometry for most recent year
tracts_sf <- get_acs(
  geography = "tract",
  variables = "B01001_001",
  year = 2023,
  state = "48",
  county = "Travis",
  geometry = TRUE,
  cache_table = TRUE,
  show_call = FALSE
) %>% 
  st_transform(4326) %>% 
  st_intersection(austin_boundary) %>% 
  select(GEOID, geometry)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. LOAD TRACT DATA (OPTIMIZED - ALL VARS AT ONCE) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("Loading tract census data for", length(years_to_pull), "years...\n")
geo_ids <- tracts_sf$GEOID

# Fetch all data with parallel processing
tracts_data <- safe_get_parallel(
  years = years_to_pull,
  vars = all_vars,
  geo = "tract",
  geo_filter = geo_ids
)

# Join geometry only once
tracts_yearly <- tracts_data %>%
  left_join(tracts_sf, by = "GEOID") %>%
  st_as_sf()

cat("✓ Loaded", nrow(tracts_data), "tract records\n")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7. LOAD ZCTA DATA (OPTIMIZED) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("Loading ZCTA census data for", length(zcta_years), "years...\n")
zctas <- zcta_austin$zipcode

zcta_data <- safe_get_parallel(
  years = zcta_years,
  vars = all_vars,
  geo = "zcta",
  geo_filter = zctas
)

zcta_clean <- zcta_data %>% 
  mutate(zipcode = str_remove(NAME, "ZCTA5 ") %>% as.numeric())

zcta_all <- zcta_austin %>%
  right_join(zcta_clean, by = "zipcode")

cat("✓ Loaded", nrow(zcta_data), "ZCTA records\n")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 8. PRE-FILTER FOR FASTER RENDERING ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pre-calculate simplified geometry for map
tracts_current <- tracts_yearly %>% 
  filter(year == max(years_to_pull)) %>%
  select(GEOID, NAME, geometry) %>%
  distinct()

cat("✓ Data loading complete!\n\n")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 9. UI -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body, html {
        margin: 0;
        padding: 0;
        overflow: hidden;
      }
      
      #appHeader {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        z-index: 2000;
        background: white;
        padding: 15px 20px;
        font-size: 26px;
        font-weight: bold;
        border-bottom: 2px solid #ddd;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }

      #mainTabs {
        position: fixed;
        top: 62px;
        left: 0;
        right: 0;
        z-index: 1500;
        background: white;
        border-bottom: 1px solid #ccc;
        margin: 0;
      }
      
      #mainTabs .nav-tabs {
        padding-left: 15px;
        margin-bottom: 0;
      }

      #mainTabs .tab-content {
        position: fixed;
        top: 108px;
        bottom: 0;
        left: 0;
        right: 0;
        overflow: hidden;
      }
      
      #mainTabs .tab-pane {
        height: 100%;
        width: 100%;
      }

      .split-container {
        display: flex;
        height: 100%;
        width: 100%;
      }

      .left-column {
        width: 45%;
        height: 100%;
        position: relative;
        overflow: hidden;
      }

      .right-column {
        width: 55%;
        height: 100%;
        overflow-y: auto;
        overflow-x: hidden;
        padding: 20px;
        border-left: 2px solid #ccc;
        background: white;
      }
      
      .leaflet-container {
        height: 100% !important;
      }
      
      .instruction-text {
        color: #666;
        font-style: italic;
        margin-top: 10px;
      }
      
      .placeholder-icon {
        font-size: 48px;
        color: #ccc;
        margin-bottom: 20px;
      }
    "))
  ),
  
  div(id = "appHeader", "Austin Neighborhood Trends (ACS 5-Year)"),
  
  div(
    id = "mainTabs",
    tabsetPanel(
      id = "tabs",
      
      # CENSUS TRACTS TAB
      tabPanel(
        "Census Tracts",
        value = "tracts",
        div(
          class = "split-container",
          
          div(
            class = "left-column",
            leafletOutput("map", width = "100%", height = "100%"),
            absolutePanel(
              top = 20, right = 20, width = 280,
              style = "z-index: 500; background-color: rgba(255,255,255,0.95);
                      padding: 12px; border-radius: 5px; box-shadow: 0 2px 8px rgba(0,0,0,0.2);",
              textInput("address_input", "Search Address:",
                        placeholder = "123 Main St, Austin, TX"),
              actionButton("search_address", "Go", class = "btn-primary btn-sm", 
                           style = "width: 100%;")
            )
          ),
          
          div(
            class = "right-column",
            conditionalPanel(
              condition = "!output.selected_tract",
              div(
                style = "text-align: center; margin-top: 100px;",
                div(class = "placeholder-icon", "📍"),
                h4("Click on a census tract to view trends"),
                p(class = "instruction-text", "Or search for an address using the search box")
              )
            ),
            conditionalPanel(
              condition = "output.selected_tract",
              h4("Selected Census Tract:"),
              textOutput("selected_tract"),
              hr(),
              plotOutput("income_plot", height = "250px"),
              plotOutput("race_plot", height = "300px"),
              plotOutput("age_plot", height = "250px")
            )
          )
        )
      ),
      
      # ZIP CODES TAB
      tabPanel(
        "ZIP Codes",
        value = "zipcodes",
        div(
          class = "split-container",
          
          div(
            class = "left-column",
            leafletOutput("zcta_map", width = "100%", height = "100%"),
            absolutePanel(
              top = 20, right = 20, width = 280,
              style = "z-index: 500; background-color: rgba(255,255,255,0.95);
                      padding: 12px; border-radius: 5px; box-shadow: 0 2px 8px rgba(0,0,0,0.2);",
              textInput("address_input_zcta", "Search Address:",
                        placeholder = "123 Main St, Austin, TX"),
              actionButton("search_address_zcta", "Go", class = "btn-primary btn-sm",
                           style = "width: 100%;")
            )
          ),
          
          div(
            class = "right-column",
            conditionalPanel(
              condition = "!output.selected_zcta",
              div(
                style = "text-align: center; margin-top: 100px;",
                div(class = "placeholder-icon", "📍"),
                h4("Click on a ZIP code to view trends"),
                p(class = "instruction-text", "Or search for an address using the search box")
              )
            ),
            conditionalPanel(
              condition = "output.selected_zcta",
              h4("Selected ZIP Code:"),
              textOutput("selected_zcta"),
              hr(),
              plotOutput("zcta_income_plot", height = "250px"),
              plotOutput("zcta_race_plot", height = "300px"),
              plotOutput("zcta_age_plot", height = "250px")
            )
          )
        )
      )
    )
  )
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 10. SERVER ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
maptiler_key <- "0EIw3h7cY50u6m4tFURH"

# # Color palettes for variables 
# race_colors <- c(
#   "White NH" = "#4E79A7",
#   "Black" = "#F28E2B",
#   "Asian" = "#E15759",
#   "Hispanic" = "#76B7B2",
#   "AIAN" = "#59A14F",
#   "Other/Multiracial" = "#EDC948"
# )
# 
# age_colors <- c(
#   "Under 18" = "#ffffcc",
#   "18-24" = "#a1dab4",
#   "25-39" = "#41b6c4",
#   "40-64" = "#2c7fb8",
#   "65 and over" = "#253494"
# )
# 
# server <- function(input, output, session) {
#   
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # ADDRESS SEARCH - TRACTS ----
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   observeEvent(input$search_address, {
#     req(input$address_input)
#     
#     addr_df <- tibble(address = input$address_input)
#     geo <- tryCatch(
#       tidygeocoder::geocode(addr_df, address = address, method = "osm", 
#                             lat = latitude, long = longitude),
#       error = function(e) NULL
#     )
#     
#     if (is.null(geo) || nrow(geo) == 0 || any(is.na(geo$latitude)) || any(is.na(geo$longitude))) {
#       showNotification("Address not found. Try a full street address.", type = "error")
#       return()
#     }
#     
#     pt <- st_as_sf(geo, coords = c("longitude", "latitude"), crs = 4326)
#     
#     tract_hit <- st_join(pt, tracts_current, join = st_within)
#     if (!is.na(tract_hit$GEOID[1])) {
#       selected_tract(tract_hit$GEOID[1])
#       showNotification(paste("Census tract:", tract_hit$GEOID[1]), type = "message")
#     } else {
#       showNotification("Address outside Austin tract dataset.", type = "warning")
#     }
#     
#     leafletProxy("map") %>%
#       clearMarkers() %>%
#       addCircleMarkers(
#         lng = geo$longitude, lat = geo$latitude,
#         radius = 8, stroke = TRUE, weight = 2,
#         color = "red", fillOpacity = 0.8
#       ) %>%
#       setView(lng = geo$longitude, lat = geo$latitude, zoom = 14)
#   })
#   
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # ADDRESS SEARCH - ZCTA ----
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   observeEvent(input$search_address_zcta, {
#     req(input$address_input_zcta)
#     
#     addr_df <- tibble(address = input$address_input_zcta)
#     geo <- tryCatch(
#       tidygeocoder::geocode(addr_df, address = address, method = "osm", 
#                             lat = latitude, long = longitude),
#       error = function(e) NULL
#     )
#     
#     if (is.null(geo) || nrow(geo) == 0 || any(is.na(geo$latitude)) || any(is.na(geo$longitude))) {
#       showNotification("Address not found. Try a full street address.", type = "error")
#       return()
#     }
#     
#     pt <- st_as_sf(geo, coords = c("longitude", "latitude"), crs = 4326)
#     
#     zcta_hit <- st_join(pt, zcta_austin, join = st_within)
#     if (!is.na(zcta_hit$zipcode[1])) {
#       selected_zcta(zcta_hit$zipcode[1])
#       showNotification(paste("ZIP code:", zcta_hit$zipcode[1]), type = "message")
#     } else {
#       showNotification("Address outside known Austin zip code", type = "warning")
#     }
#     
#     leafletProxy("zcta_map") %>%
#       clearMarkers() %>%
#       addCircleMarkers(
#         lng = geo$longitude, lat = geo$latitude,
#         radius = 8, stroke = TRUE, weight = 2,
#         color = "red", fillOpacity = 0.8
#       ) %>%
#       setView(lng = geo$longitude, lat = geo$latitude, zoom = 14)
#   })
#   
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # TRACT MAP & PLOTS ----
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   output$map <- renderLeaflet({
#     leaflet() %>%
#       addTiles(
#         urlTemplate = paste0(
#           "https://api.maptiler.com/maps/streets-v4/256/{z}/{x}/{y}.png?key=",
#           maptiler_key),
#         attribution = '© MapTiler © OpenStreetMap contributors'
#       ) %>%
#       setView(lng = austin_center[1], lat = austin_center[2], zoom = 11) %>%
#       addPolygons(
#         data = tracts_current,
#         layerId = ~GEOID,
#         color = "#444444",
#         weight = 1,
#         fillOpacity = 0.15,
#         highlightOptions = highlightOptions(color = "yellow", weight = 4)
#       )
#   })
#   
#   selected_tract <- reactiveVal(NULL)
#   
#   observeEvent(input$map_shape_click, {
#     selected_tract(input$map_shape_click$id)
#   })
#   
#   output$selected_tract <- renderText({
#     req(selected_tract())
#     tract_name <- tracts_current %>%
#       filter(GEOID == selected_tract()) %>%
#       pull(NAME) %>% 
#       str_remove("Census Tract ") %>% 
#       str_remove(", Travis County, Texas")
#     paste("Tract #:", tract_name)
#   })
#   
#   # Reactive data filtering - filter once, use three times
#   tract_data_filtered <- reactive({
#     req(selected_tract())
#     tracts_yearly %>%
#       filter(GEOID == selected_tract())
#   })
#   
#   output$income_plot <- renderPlot({
#     data <- tract_data_filtered()
#     data %>%
#       filter(variable %in% names(income_vars)) %>%
#       ggplot(aes(x = year, y = estimate)) +
#       geom_line(linewidth = 1, color = "#2E86AB") + 
#       geom_point(size = 2, color = "#2E86AB") +
#       scale_y_continuous(labels = scales::dollar_format()) + 
#       labs(title = "Median Income Trend", y = "Median income ($)", x = "Year") +
#       theme_minimal(base_size = 13)
#   })
#   
#   output$race_plot <- renderPlot({
#     data <- tract_data_filtered() 
#     data %>%
#       filter(variable %in% names(race_vars)) %>%
#       filter(year %in% c(2009, 2012, 2015, 2018, 2021, 2023)) %>% 
#       summarizeRace() %>%
#       # mutate(race_fact = as.factor(race_collapsed)) %>% 
#       ggplot(aes(x = year, y = prop, 
#                  alluvium = race_collapsed,
#                  stratum = race_collapsed)) +  # Add stratum aesthetic here
#       geom_alluvium(aes(fill = race_collapsed), 
#                     color = "gray25", alpha = 0.7, decreasing = FALSE) +
#       scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
#       scale_x_continuous(breaks = seq(min(years_to_pull), max(years_to_pull), by = 2)) +
#       scale_fill_manual(values = race_colors) +
#       labs(title = "Race/Ethnicity Composition Over Time", 
#            x = "Year", 
#            y = "Share of population",
#            fill = "Racial /\nethnic group") +
#       guides(fill = guide_legend(ncol = 1)) + 
#       theme_minimal(base_size = 13) +
#       theme(legend.position = "right",
#             panel.grid.minor = element_blank(),
#             legend.text = element_text(size = 9),
#             legend.title = element_text(size = 11))
#   })
#   
#   output$age_plot <- renderPlot({
#     data <- tract_data_filtered()
#     data %>%
#       filter(variable %in% names(age_vars)) %>%
#       filter(year %in% c(2009, 2012, 2015, 2018, 2021, 2023)) %>% 
#       summarizeAge() %>% 
#       ggplot(aes(x = year, y = prop, 
#                  alluvium = age_collapse,
#                  stratum = age_collapse)) +  # Add stratum aesthetic here
#       geom_alluvium(aes(fill = age_collapse), 
#                     color = "gray25", alpha = 0.7, decreasing = FALSE) + 
#       scale_y_continuous(labels = scales::percent_format()) +
#       scale_x_continuous(breaks = seq(min(years_to_pull), max(years_to_pull), by = 2)) +
#       scale_fill_manual(values = age_colors) +
#       labs(title = "Age Distribution", y = "Share of population", 
#            x = "Year", color = "Age group") +
#       theme_minimal(base_size = 13) + 
#       theme(legend.position = "right",
#             panel.grid.minor = element_blank(),
#             legend.text = element_text(size = 9),
#             legend.title = element_text(size = 11))
#   })
#   
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # ZCTA MAP & PLOTS ----
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   output$zcta_map <- renderLeaflet({
#     leaflet() %>%
#       addTiles(
#         urlTemplate = paste0(
#           "https://api.maptiler.com/maps/streets-v4/256/{z}/{x}/{y}.png?key=",
#           maptiler_key),
#         attribution = '© MapTiler © OpenStreetMap contributors'
#       ) %>%
#       setView(lng = austin_center[1], lat = austin_center[2], zoom = 11) %>%
#       addPolygons(
#         data = zcta_austin,
#         layerId = ~zipcode,
#         color = "#444444",
#         weight = 1,
#         fillOpacity = 0.15,
#         highlightOptions = highlightOptions(color = "yellow", weight = 4)
#       )
#   })
#   
#   selected_zcta <- reactiveVal(NULL)
#   
#   observeEvent(input$zcta_map_shape_click, {
#     selected_zcta(input$zcta_map_shape_click$id)
#   })
#   
#   output$selected_zcta <- renderText({
#     req(selected_zcta())
#     paste("Zip code:", selected_zcta())
#   })
#   
#   # Reactive data filtering - filter once, use three times
#   zcta_data_filtered <- reactive({
#     req(selected_zcta())
#     zcta_all %>%
#       filter(zipcode == selected_zcta())
#   })
#   
#   output$zcta_income_plot <- renderPlot({
#     data <- zcta_data_filtered()
#     data %>%
#       filter(variable %in% names(income_vars)) %>%
#       ggplot(aes(x = year, y = estimate)) +
#       geom_line(linewidth = 1, color = "#2E86AB") + 
#       geom_point(size = 2, color = "#2E86AB") +
#       scale_y_continuous(labels = scales::dollar_format()) + 
#       labs(title = "Median Income Trend", y = "Median income ($)", x = "Year") +
#       theme_minimal(base_size = 13)
#   })
#   
#   output$zcta_race_plot <- renderPlot({
#     data <- zcta_data_filtered()
#     data %>%
#       filter(variable %in% names(race_vars)) %>%
#       # filter(year %in% c(2009, 2012, 2015, 2018, 2021, 2023)) %>% 
#       summarizeRace() %>%
#       mutate(race_fact = as.factor(race_collapsed)) %>%
#       ggplot(aes(x = year, y = prop, 
#                  alluvium = race_collapsed,
#                  stratum = race_collapsed)) +  # Add stratum aesthetic here
#       geom_alluvium(aes(fill = race_collapsed), 
#                     color = "gray25", alpha = 0.7, decreasing = FALSE) +
#       geom_col(aes(fill = fct_reorder(race_collapsed, -prop)), alpha = 0, width = 1) +
#       scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
#       scale_x_continuous(breaks = seq(min(years_to_pull), max(years_to_pull), by = 2)) +
#       scale_fill_manual(values = race_colors) +
#       labs(title = "Race/Ethnicity Composition Over Time", 
#            x = "Year", 
#            y = "Share of population",
#            fill = "Racial /\nethnic group") +
#       guides(fill = guide_legend(nrow = 1)) + 
#       theme_minimal(base_size = 13) +
#       theme(legend.position = "bottom",
#             panel.grid.minor = element_blank(),
#             legend.text = element_text(size = 9),
#             legend.title = element_text(size = 11))
#   })
#   
#   output$zcta_age_plot <- renderPlot({
#     data <- zcta_data_filtered()
#     data %>%
#       filter(variable %in% names(age_vars)) %>%
#       filter(year %in% c(2009, 2012, 2015, 2018, 2021, 2023)) %>% 
#       summarizeAge() %>% 
#       mutate(age_fact = factor(age_collapse, 
#                                levels = c("Under 18", "18-24", "25-39", "40-64", "65 and over"),
#                                labels = c("Under 18", "18-24", "25-39", "40-64", "65 and over"))) %>% 
#       ggplot(aes(x = year, y = prop, 
#                  alluvium = age_fact,
#                  stratum = age_fact)) +  # Add stratum aesthetic here
#       geom_alluvium(aes(fill = age_fact), 
#                     color = "gray25", alpha = 0.7, decreasing = FALSE) + 
#       scale_y_continuous(labels = scales::percent_format()) +
#       scale_x_continuous(breaks = seq(min(years_to_pull), max(years_to_pull), by = 2)) +
#       scale_fill_manual(values = age_colors) +
#       labs(title = "Age Distribution", y = "Share of population", 
#            x = "Year", fill = "Age group") +
#       theme_minimal(base_size = 13) + 
#       theme(legend.position = "right",
#             panel.grid.minor = element_blank(),
#             legend.text = element_text(size = 9),
#             legend.title = element_text(size = 11))
#   })
#   
#   # Make outputs available to conditionalPanel
#   outputOptions(output, "selected_tract", suspendWhenHidden = FALSE)
#   outputOptions(output, "selected_zcta", suspendWhenHidden = FALSE)
# }
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # 11. RUN APP ----
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# shinyApp(ui, server)
# 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOTLY VERSION ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Add plotly to your libraries at the top


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UI ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(
  # Waiting page 
  # use_waiter(),    
  # waiter_show_on_load( 
  #   html = tagList(
  #     h2("Loading data…"),
  #     spin_ellipsis()
  #   ),
  #   color = "#FFFFFF"
  # ),
  
  tags$head(
    tags$style(HTML("
      body, html {
        margin: 0;
        padding: 0;
        overflow: hidden;
      }
      
      #appHeader {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        z-index: 2000;
        background: white;
        padding: 15px 20px;
        font-size: 26px;
        font-weight: bold;
        border-bottom: 2px solid #ddd;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }

      #mainTabs {
        position: fixed;
        top: 62px;
        left: 0;
        right: 0;
        z-index: 1500;
        background: white;
        border-bottom: 1px solid #ccc;
        margin: 0;
      }
      
      #mainTabs .nav-tabs {
        padding-left: 15px;
        margin-bottom: 0;
      }

      #mainTabs .tab-content {
        position: fixed;
        top: 108px;
        bottom: 0;
        left: 0;
        right: 0;
        overflow: hidden;
      }
      
      #mainTabs .tab-pane {
        height: 100%;
        width: 100%;
      }

      .split-container {
        display: flex;
        height: 100%;
        width: 100%;
      }

      .left-column {
        width: 45%;
        height: 100%;
        position: relative;
        overflow: hidden;
      }

      .right-column {
        width: 55%;
        height: 100%;
        overflow-y: auto;
        overflow-x: hidden;
        padding: 20px;
        border-left: 2px solid #ccc;
        background: white;
      }
      
      .leaflet-container {
        height: 100% !important;
      }
      
      .instruction-text {
        color: #666;
        font-style: italic;
        margin-top: 10px;
      }
      
      .placeholder-icon {
        font-size: 48px;
        color: #ccc;
        margin-bottom: 20px;
      }
    "))
  ),
  
  div(id = "appHeader", "Austin Neighborhood Trends (ACS 5-Year)"),
  
  div(
    id = "mainTabs",
    tabsetPanel(
      id = "tabs",
      
      # CENSUS TRACTS TAB
      tabPanel(
        "Census Tracts",
        value = "tracts",
        div(
          class = "split-container",
          
          div(
            class = "left-column",
            leafletOutput("map", width = "100%", height = "100%"),
            absolutePanel(
              top = 20, right = 20, width = 280,
              style = "z-index: 500; background-color: rgba(255,255,255,0.95);
                      padding: 12px; border-radius: 5px; box-shadow: 0 2px 8px rgba(0,0,0,0.2);",
              textInput("address_input", "Search Address:",
                        placeholder = "123 Main St, Austin, TX"),
              actionButton("search_address", "Go", class = "btn-primary btn-sm", 
                           style = "width: 100%;")
            )
          ),
          
          div(
            class = "right-column",
            conditionalPanel(
              condition = "!output.selected_tract",
              div(
                style = "text-align: center; margin-top: 100px;",
                div(class = "placeholder-icon", "📍"),
                h4("Click on a census tract to view trends"),
                p(class = "instruction-text", "Or search for an address using the search box")
              )
            ),
            conditionalPanel(
              condition = "output.selected_tract",
              h4("Selected Census Tract:"),
              textOutput("selected_tract"),
              hr(),
              plotlyOutput("income_plot", height = "250px"),
              plotlyOutput("race_plot", height = "350px"),
              plotlyOutput("age_plot", height = "350px")
            )
          )
        )
      ),
      
      # ZIP CODES TAB
      tabPanel(
        "ZIP Codes",
        value = "zipcodes",
        div(
          class = "split-container",
          
          div(
            class = "left-column",
            leafletOutput("zcta_map", width = "100%", height = "100%"),
            absolutePanel(
              top = 20, right = 20, width = 280,
              style = "z-index: 500; background-color: rgba(255,255,255,0.95);
                      padding: 12px; border-radius: 5px; box-shadow: 0 2px 8px rgba(0,0,0,0.2);",
              textInput("address_input_zcta", "Search Address:",
                        placeholder = "123 Main St, Austin, TX"),
              actionButton("search_address_zcta", "Go", class = "btn-primary btn-sm",
                           style = "width: 100%;")
            )
          ),
          
          div(
            class = "right-column",
            conditionalPanel(
              condition = "!output.selected_zcta",
              div(
                style = "text-align: center; margin-top: 100px;",
                div(class = "placeholder-icon", "📍"),
                h4("Click on a ZIP code to view trends"),
                p(class = "instruction-text", "Or search for an address using the search box")
              )
            ),
            conditionalPanel(
              condition = "output.selected_zcta",
              h4("Selected ZIP Code:"),
              textOutput("selected_zcta"),
              hr(),
              plotlyOutput("zcta_income_plot", height = "250px"),
              plotlyOutput("zcta_race_plot", height = "350px"),
              plotlyOutput("zcta_age_plot", height = "350px")
            )
          )
        )
      )
    )
  )
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SERVER ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
maptiler_key <- "0EIw3h7cY50u6m4tFURH"

# Color palettes for variables 
race_colors <- c(
  "White NH" = "#4E79A7",
  "Black" = "#F28E2B",
  "Asian" = "#E15759",
  "Hispanic" = "#76B7B2",
  "AIAN" = "#59A14F",
  "Other/Multiracial" = "#EDC948"
)

age_colors <- c(
  "Under 18" = "#ffffcc",
  "18-24" = "#a1dab4",
  "25-39" = "#41b6c4",
  "40-64" = "#2c7fb8",
  "65 and over" = "#253494"
)

# Adding alpha for plot_ly fills 
add_alpha <- function(hex, alpha = 0.6) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], alpha)
}
race_colors_alpha <- sapply(race_colors, add_alpha, alpha = 0.6)
age_colors_alpha <- sapply(age_colors, add_alpha, alpha = 0.6)


server <- function(input, output, session) {
  # Hides waiter when data is ready 
  # waiter_hide()
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ADDRESS SEARCH - TRACTS ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$search_address, {
    req(input$address_input)
    
    addr_df <- tibble(address = input$address_input)
    geo <- tryCatch(
      tidygeocoder::geocode(addr_df, address = address, method = "osm", 
                            lat = latitude, long = longitude),
      error = function(e) NULL
    )
    
    if (is.null(geo) || nrow(geo) == 0 || any(is.na(geo$latitude)) || any(is.na(geo$longitude))) {
      showNotification("Address not found. Try a full street address.", type = "error")
      return()
    }
    
    pt <- st_as_sf(geo, coords = c("longitude", "latitude"), crs = 4326)
    
    tract_hit <- st_join(pt, tracts_current, join = st_within)
    if (!is.na(tract_hit$GEOID[1])) {
      selected_tract(tract_hit$GEOID[1])
      showNotification(paste("Census tract:", tract_hit$GEOID[1]), type = "message")
    } else {
      showNotification("Address outside Austin tract dataset.", type = "warning")
    }
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = geo$longitude, lat = geo$latitude,
        radius = 8, stroke = TRUE, weight = 2,
        color = "red", fillOpacity = 0.8
      ) %>%
      setView(lng = geo$longitude, lat = geo$latitude, zoom = 14)
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ADDRESS SEARCH - ZCTA ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$search_address_zcta, {
    req(input$address_input_zcta)
    
    addr_df <- tibble(address = input$address_input_zcta)
    geo <- tryCatch(
      tidygeocoder::geocode(addr_df, address = address, method = "osm", 
                            lat = latitude, long = longitude),
      error = function(e) NULL
    )
    
    if (is.null(geo) || nrow(geo) == 0 || any(is.na(geo$latitude)) || any(is.na(geo$longitude))) {
      showNotification("Address not found. Try a full street address.", type = "error")
      return()
    }
    
    pt <- st_as_sf(geo, coords = c("longitude", "latitude"), crs = 4326)
    
    zcta_hit <- st_join(pt, zcta_austin, join = st_within)
    if (!is.na(zcta_hit$zipcode[1])) {
      selected_zcta(zcta_hit$zipcode[1])
      showNotification(paste("ZIP code:", zcta_hit$zipcode[1]), type = "message")
    } else {
      showNotification("Address outside known Austin zip code", type = "warning")
    }
    
    leafletProxy("zcta_map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = geo$longitude, lat = geo$latitude,
        radius = 8, stroke = TRUE, weight = 2,
        color = "red", fillOpacity = 0.8
      ) %>%
      setView(lng = geo$longitude, lat = geo$latitude, zoom = 14)
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # TRACT MAP & PLOTS ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = paste0(
          "https://api.maptiler.com/maps/streets-v4/256/{z}/{x}/{y}.png?key=",
          maptiler_key),
        attribution = '© MapTiler © OpenStreetMap contributors'
      ) %>%
      setView(lng = austin_center[1], lat = austin_center[2], zoom = 11) %>%
      addPolygons(
        data = tracts_current,
        layerId = ~GEOID,
        color = "#444444",
        weight = 1,
        fillOpacity = 0.15,
        highlightOptions = highlightOptions(color = "yellow", weight = 4)
      )
  })
  
  selected_tract <- reactiveVal(NULL)
  
  observeEvent(input$map_shape_click, {
    selected_tract(input$map_shape_click$id)
  })
  
  output$selected_tract <- renderText({
    req(selected_tract())
    tract_name <- tracts_current %>%
      filter(GEOID == selected_tract()) %>%
      pull(NAME) %>% 
      str_remove("Census Tract ") %>% 
      str_remove(", Travis County, Texas")
    paste("Tract #:", tract_name)
  })
  
  # Reactive data filtering
  tract_data_filtered <- reactive({
    req(selected_tract())
    tracts_yearly %>%
      filter(GEOID == selected_tract())
  })
  
  # INCOME PLOT - Interactive line chart
  output$income_plot <- renderPlotly({
    data <- tract_data_filtered()
    plot_data <- data %>%
      filter(variable %in% names(income_vars)) %>% 
      st_drop_geometry()
    
    plot_ly(plot_data, x = ~year, y = ~estimate, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#2E86AB', width = 3),
            marker = list(color = '#2E86AB', size = 8),
            hovertemplate = paste0(
              "<b>Year:</b> %{x}<br>",
              "<b>Median Income:</b> $%{y:,.0f}<br>",
              "<extra></extra>"
            )) %>%
      layout(
        title = list(text = "Median Income Trend", font = list(size = 14)),
        xaxis = list(title = "Year",
                     tickformat = "%Y",
                     category = "date"),
        yaxis = list(title = "Median income ($)", tickformat = "$,.0f"),
        hovermode = "closest",
        margin = list(l = 60, r = 20, t = 40, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # RACE PLOT - Interactive stacked area chart (TRACTS)
  output$race_plot <- renderPlotly({
    data <- tract_data_filtered()
    plot_data <- data %>%
      filter(variable %in% names(race_vars)) %>%
      summarizeRace() %>%
      # mutate(race_collapsed = factor(race_collapsed, 
      #                                levels = c("AIAN", "Asian", "Black", "Hispanic", "Other/Multiracial", "White NH"))
      #        ) %>%
      arrange(year, race_collapsed)
    
    fct_merge <- plot_data %>% 
      filter(year == 2023) %>% 
      mutate(race_fact = fct_reorder(race_collapsed, prop)) %>% 
      select(race_fact, race_collapsed) %>% 
      right_join(plot_data, by = "race_collapsed") %>% 
      arrange(rev(race_fact))
    
    plot_ly(fct_merge, x = ~year, y = ~prop, color = ~race_fact,
            type = 'scatter', mode = 'lines', stackgroup = 'one',
            colors = race_colors,
            text = ~race_fact,
            hoveron = 'points',
            hovertemplate = paste0(
              "<b>%{text}</b><br>",
              "Year: %{x}<br>",
              "Share: %{y:.1%}<br>",
              "<extra></extra>"
            )) %>%
      layout(
        title = list(text = "Race/Ethnicity Composition Over Time", font = list(size = 14)),
        xaxis = list(title = "Year",
                     tickformat = "%Y",
                     category = "date"),
        yaxis = list(title = "Share of population", tickformat = ".0%"),
        hovermode = "closest",
        legend = list(title = list(text = "Racial/ethnic group"), 
                      orientation = "v", x = 1.02, y = 0.5),
        margin = list(l = 60, r = 120, t = 40, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # AGE PLOT - Interactive stacked area chart
  output$age_plot <- renderPlotly({
    data <- tract_data_filtered()
    plot_data <- data %>%
      filter(variable %in% names(age_vars)) %>%
      summarizeAge() %>%
      mutate(age_fact = factor(age_collapse,
                               levels = c("Under 18", "18-24", "25-39", "40-64", "65 and over"))) %>% 
      arrange(year, age_fact)
    
    plot_ly(plot_data, x = ~year, y = ~prop, color = ~age_fact,
            type = 'scatter', mode = 'lines', stackgroup = 'one',
            colors = age_colors,
            text = ~age_fact,
            hoveron = 'points',
            hovertemplate = paste0(
              "<b>%{text}</b><br>",
              "Year: %{x}<br>",
              "Share: %{y:.1%}<br>",
              "<extra></extra>"
            )) %>%
      layout(
        title = list(text = "Age Distribution", font = list(size = 14)),
        xaxis = list(title = "Year",
                     tickformat = "%Y",
                     category = "date"),
        yaxis = list(title = "Share of population", tickformat = ".0%"),
        hovermode = "closest",
        legend = list(title = list(text = "Age group"), 
                      orientation = "v", x = 1.02, y = 0.5),
        margin = list(l = 60, r = 120, t = 40, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ZCTA MAP & PLOTS ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$zcta_map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = paste0(
          "https://api.maptiler.com/maps/streets-v4/256/{z}/{x}/{y}.png?key=",
          maptiler_key),
        attribution = '© MapTiler © OpenStreetMap contributors'
      ) %>%
      setView(lng = austin_center[1], lat = austin_center[2], zoom = 11) %>%
      addPolygons(
        data = zcta_austin,
        layerId = ~zipcode,
        color = "#444444",
        weight = 1,
        fillOpacity = 0.15,
        highlightOptions = highlightOptions(color = "yellow", weight = 4)
      )
  })
  
  selected_zcta <- reactiveVal(NULL)
  
  observeEvent(input$zcta_map_shape_click, {
    selected_zcta(input$zcta_map_shape_click$id)
  })
  
  output$selected_zcta <- renderText({
    req(selected_zcta())
    paste("Zip code:", selected_zcta())
  })
  
  # Reactive data filtering
  zcta_data_filtered <- reactive({
    req(selected_zcta())
    zcta_all %>%
      filter(zipcode == selected_zcta())
  })
  
  # ZCTA INCOME PLOT
  output$zcta_income_plot <- renderPlotly({
    data <- zcta_data_filtered()
    plot_data <- data %>%
      filter(variable %in% names(income_vars)) %>% 
      st_drop_geometry()
    
    plot_ly(plot_data, x = ~year, y = ~estimate, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#2E86AB', width = 3),
            marker = list(color = '#2E86AB', size = 8),
            hovertemplate = paste0(
              "<b>Year:</b> %{x}<br>",
              "<b>Median Income:</b> $%{y:,.0f}<br>",
              "<extra></extra>"
            )) %>%
      layout(
        title = list(text = "Median Income Trend", font = list(size = 14)),
        xaxis = list(title = "Year",
                     tickformat = "%Y",
                     category = "date"),
        yaxis = list(title = "Median income ($)", tickformat = "$,.0f"),
        hovermode = "closest",
        margin = list(l = 60, r = 20, t = 40, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ZCTA RACE PLOT - Interactive stacked area chart
  output$zcta_race_plot <- renderPlotly({
    data <- zcta_data_filtered()
    plot_data <- data %>%
      filter(variable %in% names(race_vars)) %>%
      summarizeRace() %>%
      # mutate(race_collapsed = factor(race_collapsed, 
      #                                levels = c("AIAN", "Asian", "Black", "Hispanic", "Other/Multiracial", "White NH"))
      #        ) %>%
      arrange(year, race_collapsed)
    
    fct_merge <- plot_data %>% 
      filter(year == 2023) %>% 
      mutate(race_fact = fct_reorder(race_collapsed, prop)) %>% 
      select(race_fact, race_collapsed) %>% 
      right_join(plot_data, by = "race_collapsed") %>% 
      arrange(rev(race_fact))
    
    plot_ly(fct_merge, x = ~year, y = ~prop, color = ~race_fact,
            type = 'scatter', mode = 'lines', stackgroup = 'one',
            colors = race_colors,
            text = ~race_fact,
            hoveron = 'points',
            hovertemplate = paste0(
              "<b>%{text}</b><br>",
              "Year: %{x}<br>",
              "Share: %{y:.1%}<br>",
              "<extra></extra>"
            )) %>%
      layout(
        title = list(text = "Race/Ethnicity Composition Over Time", font = list(size = 14)),
        xaxis = list(title = "Year",
                     tickformat = "%Y",
                     category = "date"),
        yaxis = list(title = "Share of population", tickformat = ".0%"),
        hovermode = "closest",
        legend = list(title = list(text = "Racial/ethnic group"), 
                      orientation = "v", x = 1.02, y = 0.5),
        margin = list(l = 60, r = 120, t = 40, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ZCTA AGE PLOT
  output$zcta_age_plot <- renderPlotly({
    data <- zcta_data_filtered()
    plot_data <- data %>%
      filter(variable %in% names(age_vars)) %>%
      summarizeAge() %>%
      mutate(age_fact = factor(age_collapse,
                               levels = c("Under 18", "18-24", "25-39", "40-64", "65 and over"))) %>% 
      arrange(year, age_fact)
    
    plot_ly(plot_data, x = ~year, y = ~prop, color = ~age_fact,
            type = 'scatter', mode = 'lines', stackgroup = 'one',
            colors = age_colors,
            text = ~age_fact,
            hoveron = 'points',
            hovertemplate = paste0(
              "<b>%{text}</b><br>",
              "Year: %{x}<br>",
              "Share: %{y:.1%}<br>",
              "<extra></extra>"
            )) %>%
      layout(
        title = list(text = "Age Distribution", font = list(size = 14)),
        xaxis = list(title = "Year",
                     tickformat = "%Y",
                     category = "date"),
        yaxis = list(title = "Share of population", tickformat = ".0%"),
        hovermode = "closest",
        legend = list(title = list(text = "Age group"), 
                      orientation = "v", x = 1.02, y = 0.5),
        margin = list(l = 60, r = 120, t = 40, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Make outputs available to conditionalPanel
  outputOptions(output, "selected_tract", suspendWhenHidden = FALSE)
  outputOptions(output, "selected_zcta", suspendWhenHidden = FALSE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RUN APP ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shinyApp(ui, server)

