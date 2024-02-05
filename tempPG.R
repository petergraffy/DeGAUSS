# Loading necessary packages
library(daymetr)
library(terra)
library(dplyr)

# Downloading the Daymet NetCDF
download_daymet_ncss(location = c(42.154247, -88.263390, 41.470117, -87.525706), # Bounding box defined as top left / bottom right pair c(lat, lon, lat, lon)
                     start = 2020,
                     end = 2021,
                     param = "tmax",
                     frequency = "daily",
                     mosaic = "na",
                     silent = FALSE,
                     force = TRUE,
                     path = getwd())

# Importing the Daymet NetCDF as a raster
daymet_data <- rast("tmax_daily_2020_ncss.nc")

# Number of cells in Daymet data
ncell(daymet_data)

# Create fake patient coordinates
addresses <- data.frame(
  id = c('1', '2', '3'),
  lat = c(41.882601, 41.879146, 41.948175),
  lon = c(-87.623325, -87.636187, -87.655558)
)
coords <- vect(addresses, geom = c("lon", "lat"), crs = "+proj=longlat +ellips=WGS84")

# Changing the coordinate reference system of the input addresses so they match that of Daymet
new_crs <- crs(daymet_data, proj = TRUE)
proj_coords <- project(coords, new_crs)

# Finding the Daymet cell numbers that match the patient coordinates
addresses <- addresses %>%
  mutate(cell = unname(cells(daymet_data, proj_coords))[, 2])

# Converting the Daymet raster to a dataframe, with cell numbers
daymet_data_df <- as.data.frame(daymet_data, cells = TRUE)

# Merging the Daymet dataframe with the patient data
daymet_merged <- inner_join(addresses, daymet_data_df, by = "cell")



#####



year_start <- as.Date("2020-01-01")
year_end <- as.Date("2021-12-31")  # made these manually but should be mapped to the actual function vars

# make new df template with cell repeated 1:nrow of daymet_df and date repeated from year_start to year_end

n <- nrow(daymet_data_df)

new_df <- data.frame(
  cell = rep(1:n, each = 365)
) %>%
  mutate(
    date = rep(seq(year_start, year_end, by = "day"), length.out = nrow(.))      # this is just to make an empty template df that can be used if we need it that has cell repeated 365 times for the study period (will need to be adjusted for leap years)
  )

# pivot daymet_df to long with columns cell, date, and tmax

long_day <- daymet_data_df %>%
  pivot_longer(cols = starts_with("tmax_"),                # would be adjusted for any function variable depending on where you put it to reflect the daymet variable selected followed by "_"
               names_to = "day",
               values_to = "tmax") %>%                     
  mutate(day = as.integer(str_remove(day, "tmax_"))) %>%   # this version does not account for leap years
  mutate(date = year_start + day - 1) %>%
  filter(date <= year_end) %>%
  select(cell, date, tmax)

str(long_day)


# to account for leap years we can do something like this

# function to check if a year is a leap year
is_leap_year <- function(year) {
  year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)        # checks to see if year is divisible by 4, 100, and 400. if divisible by all then leap year. if divisible by 4 and 100 but not 400 then not leap year. if divisible by 4 but not 100 then leap year. not divisible by 4 then not leap year.
}

# then add it to long_day. adds a day if found to be a leap year.

long_day <- daymet_data_df %>%
  pivot_longer(cols = starts_with("tmax_"),                                               # select columns to pivot as any starting with tmax_
               names_to = "day",                                                          # dummy column to contain the names of original columns
               values_to = "tmax") %>%                                                    # name of new column in the long df
  mutate(day = as.integer(str_remove(day, "tmax_"))) %>%                                  # removes tmax_ and converts column to integer for numeric day values
  mutate(year = year(year_start)) %>%                                                     # starts at year_start
  mutate(date = year_start + day - 1) %>%                                                 # calculates date from day 1 of year_start
  mutate(date = ifelse(is_leap_year(year(date)) & month(date) == 2 & day(date) == 29,     # accommodate leap years
                       year(date) + 1, date)) %>%
  filter(date <= year_end) %>%                                                            # cuts off any dates after year_end just in case
  mutate(date = as.Date(date)) %>%
  select(cell, date, tmax)                                                                # convert to date and drop the day var to keep what we want





