# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: austin_census_download.R
# Purpose: download ACS data and save in RDS
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
# Directories 
root <- "/Users/nathanchesterman/Documents/GitHub/public/"
out_path <- file.path(root, "20_Datasets", "austin_census_viz_app")

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
# 3. OPTIMIZED DATA LOADING ----
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
# 4. LOAD BOUNDARIES ----
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
tracts_austin <- get_acs(
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
  select(GEOID, NAME, geometry)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. LOAD TRACT DATA (OPTIMIZED - ALL VARS AT ONCE) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("Loading tract census data for", length(years_to_pull), "years...\n")
geo_ids <- tracts_austin$GEOID

# Fetch all data with parallel processing
tracts_data <- safe_get_parallel(
  years = years_to_pull,
  vars = all_vars,
  geo = "tract",
  geo_filter = geo_ids
)

# Join geometry only once
# tracts_yearly <- tracts_data %>%
#   left_join(tracts_sf, by = "GEOID") %>%
#   st_as_sf()

cat("✓ Loaded", nrow(tracts_data), "tract records\n")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. LOAD ZCTA DATA (OPTIMIZED) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("Loading ZCTA census data for", length(zcta_years), "years...\n")
zctas <- zcta_austin$zipcode

zcta_data <- safe_get_parallel(
  years = zcta_years,
  vars = all_vars,
  geo = "zcta",
  geo_filter = zctas
) %>% 
  mutate(zipcode = str_remove(NAME, "ZCTA5 ") %>% as.numeric())

cat("✓ Loaded", nrow(zcta_data), "ZCTA records\n")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7. PRE-FILTER FOR FASTER RENDERING ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pre-calculate simplified geometry for map
# tracts_current <- tracts_yearly %>% 
#   filter(year == max(years_to_pull)) %>%
#   select(GEOID, NAME, geometry) %>%
#   distinct()

# cat("✓ Data loading complete!\n\n")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7. SAVE DATA TO RDS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_list <- list("tracts_data" = tracts_data,
                "tracts_austin" = tracts_austin,
                "zcta_data" = zcta_data,
                "zcta_austin" = zcta_austin)


saveRDS(df_list,
     file = file.path(out_path, "austin_census_viz_data.RDS"))
