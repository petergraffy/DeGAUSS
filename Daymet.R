# Daymet weather variables include daily minimum and maximum temperature, precipitation,
# vapor pressure, shortwave radiation, snow water equivalent, and day length produced on
# a 1 km x 1 km gridded surface over continental North America and Hawaii from 1980 and
# over Puerto Rico from 1950 through the end of the most recent full calendar year.

# Daymet data documentation: https://daac.ornl.gov/DAYMET/guides/Daymet_Daily_V4.html

# Note: The Daymet calendar is based on a standard calendar year. All Daymet years, including
# leap years, have 1–365 days. For leap years, the Daymet data include leap day (February 29)
# and December 31 is discarded from leap years to maintain a 365-day year.

# Loading necessary packages
library(daymetr)
library(tidyverse)
library(terra)

# Writing functions
# Creating function to link the Daymet data coordinates to the patient address coordinates for a specified patient and specified date
daymet_select <- function(id_var, date_var, .time_dict = time_dict, .daymet_data = daymet_data, .proj_coords = proj_coords, .dm_var = dm_var, .main_dataset = main_dataset) {
  # Taking care of leap years, per Daymet conventions (12/31 is switched to 12/30)
  date_var <- as_date(date_var)
  if (leap_year(date_var) & month(date_var) == 12 & day(date_var) == 31) {
    date_var <- date_var - 1
  }
  # Looking up the number for the specified date in the time dictionary
  date_num <- .time_dict %>%
    filter(date == date_var) %>%
    select(number) %>%
    pull
  # Extracting the Daymet data for the specified patient and specified date
  daymet_linked <- terra::extract(subset(.daymet_data, date_num),
                                  subset(.proj_coords, .proj_coords[[names(.proj_coords)]] == id_var),
                                  bind = TRUE)
  daymet_variable_df <- as.data.frame(daymet_linked)
  # Renaming the Daymet data that was just added with the specified date
  date_name <- .time_dict %>%
    filter(number == date_num) %>%
    select(date) %>%
    pull
  daymet_variable_name <- paste0(.dm_var, "_", date_name)
  rename_variable_name <- daymet_variable_df %>%
    select(last_col()) %>%
    names()
  daymet_variable_df <- daymet_variable_df %>%
    rename(!!daymet_variable_name := !!rename_variable_name)
  # Linking the Daymet data into the main dataset
  if (daymet_variable_name %in% colnames(.main_dataset)) {
    .main_dataset <- rows_update(.main_dataset, daymet_variable_df)
  } else {
    .main_dataset <- full_join(.main_dataset, daymet_variable_df)
  }
}

# Reading in the patient address data
addresses <- read_csv("sample_addresses.csv")

# Creating a main dataset to contain all linked Daymet data - these will all be merged on the first column (the ID column)
main_dataset <- addresses %>%
  select(1)

# Converting the patient addresses to a SpatVector
coords <- vect(addresses, geom = c("lon", "lat"), crs = "+proj=longlat +ellips=WGS84")

# Finding the min and max longitude and latitude out of all the patient address coordinates
min_lon <- min(addresses$lon)
max_lon <- max(addresses$lon)
min_lat <- min(addresses$lat)
max_lat <- max(addresses$lat)

# In order to preserve patient privacy, adding a random amount of noise to the bounding box of patient addresses
# Each bounding box point (e.g., maximum latitude) will be extended by an additional 1-22 kilometers
noise <- runif(4, min = 0.01, max = 0.2)
min_lon <- min_lon - noise[1]
max_lon <- max_lon + noise[2]
min_lat <- min_lat - noise[3]
max_lat <- max_lat + noise[4]

# Downloading the Daymet NetCDF data defined by the coordinate bounding box: One file per variable per year
# This procedure extracts the bounding box from the patient coordinates + noise, but we'll also want to add an option that lets the user specify their own bounding box
year_start <- 2020 # Start year of Daymet NetCDF data download
year_end <- 2021 # End year of Daymet NetCDF data download
daymet_variables <- "tmax,tmin" # Comma-separated string of Daymet variables: tmax,tmin,srad,vp,swe,prcp,dayl
daymet_variables <- str_remove(daymet_variables, " ")
daymet_variables <- str_split(daymet_variables, ",", simplify = TRUE)
for (variable in daymet_variables) {
  download_daymet_ncss(location = c(max_lat, min_lon, min_lat, max_lon), # Bounding box defined as top left / bottom right pair c(lat, lon, lat, lon)
                       start = year_start,
                       end = year_end,
                       param = variable,
                       frequency = "daily",
                       mosaic = "na",
                       silent = FALSE,
                       force = TRUE,
                       path = getwd())
}

# WILL NEED TO BUILD THIS OUT SO IT LOOPS THROUGH A SEQUENCE FROM YEAR_START TO YEAR_END
# WITHIN EACH YEAR LOOP, WILL NEED A NESTED LOOP THAT WORKS THROUGH EACH REQUESTED DAYMET VARIABLE
year_sequence <- year_start:year_end
yr <- year_sequence[1] # INCREMENT THIS IN LOOP
dm_var <- daymet_variables[1] # INCREMENT THIS IN LOOP

# Creating a dictionary to link numbers 1–365 to a date in a year
time_dict <- tibble(number = 1:365)
origin <- as_date(paste0(yr, "-01-01")) - 1 # Numbers count days since origin
time_dict <- time_dict %>%
  mutate(date = as_date(number, origin = origin))

# Loading the NetCDF file downloaded from Daymet as a SpatRaster
file_name <- paste0(dm_var, "_daily_", yr, "_ncss.nc")
daymet_data <- rast(file_name)

# Changing the coordinate reference system of the patient addresses so they match that of Daymet
new_crs <- crs(daymet_data, proj = TRUE)
proj_coords <- project(coords, new_crs)

# Linking the Daymet data coordinates to the patient address coordinates for a specified patient and specified date
id_var <- "patid1" # Specified patient
date_var <- "2020-01-01" # Specified date
main_dataset <- daymet_select(id_var, date_var)

# Run Daymet_delete.R to delete the NetCDF files that were downloaded from disk

# NEXT STEPS:
# - CREATE MULTIPLE VARIABLES LOOP
# - CREATE MULTIPLE YEARS LOOP
# - BUILD OUT THE EVENT DATE DETECTOR (EVENT DATES TO LIST, EXPAND WITH SPECIFIED LAG, SORT AND REMOVE DUPLICATES, SELECT FROM LIST IF DATE IN YEAR)
# - ADD IN CODE FOR EXTRA OPTIONS (DAYMETR OPTIONS, BOUNDING BOX OPTIONS)