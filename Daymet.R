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
# NOTE - I'M USING THE DAYMETR PACKAGE, SPECIFICALLY DEVELOPED TO DOWNLOAD DAYMET DATA INTO R.
# THIS RUNS THE RISK OF THE PACKAGE BECOMING OUTDATED THOUGH, MAYBE WE WANT TO SCRAPE DATA OFF DAYMET A DIFFERENT WAY.
# WE WILL ALSO NEED TO MANUALLY CONVERT EVENT DATES INTO SEQUENTIAL YEAR DAYS (OR VICE-VERSA).
# THIS PACKAGE ONLY ALLOWS YOU TO SPECIFY START AND END DATES ON THE YEAR LEVEL.

# Downloading Daymet data for a specified longitude/latitude and time period
daymet_data <- download_daymet(lat = 36.0133,
                               lon = -84.2625,
                               start = 2020,
                               end = 2023,
                               force = TRUE,
                               internal = TRUE,
                               silent = TRUE)
daymet_data <- daymet_data$data
# year = year of weather measure
# yday = sequential day within that year
# dayl..s. = day length (seconds/day)
# prcp..mm.day. = precipitation (mm/day)
# srad..W.m.2. = shortwave radiation (W/m^2)
# swe..kg.m.2. = snow water equivalent (kg/m^2)
# tmax..deg.c. = maximum temperature (deg c)
# tmin..deg.c. = minimum temperature (deg c)
# vp..Pa. = vapor pressure (Pa)

# Downloading Daymet data given a CSV of site name, latitude, longitude
daymet_data <- download_daymet_batch(file_location = 'sample_addresses.csv',
                                     start = 2020,
                                     end = 2023,
                                     force = TRUE,
                                     internal = TRUE)
# Extracting the Daymet data for the first patient
assign(daymet_data[[1]]$site, data.frame(daymet_data[[1]]$data))


# Let's try getting Daymet data a different way, downloading it directly from the web
# Here's some documentation: https://daymet.ornl.gov/web_services#single
# Loading necessary packages
library(curl)
library(tidyverse)
library(janitor)

# Setting a URL - this can be customized with Latitude, Longitude, Variables of Interest, Start Date, and End Date
url <- 'https://daymet.ornl.gov/single-pixel/api/data?lat=35.9621&lon=-84.2916&vars=dayl,prcp,srad,swe,tmax,tmin,vp&start=2020-01-01&end=2020-01-01'

# Downloading the file directly and saving to disk
download.file(url, destfile = "Daymet Data.csv")
# Importing this file back into R, only keeping the rows that I want
daymet_data <- read_csv("Daymet Data.csv", skip = 7)

# Alternatively, streaming the data directly into R
# Opening a streaming connection
connection <- curl(url)
open(connection)
# Streaming the data as text
output <- readLines(connection)
# Closing the streaming connection
close(connection)
# Pulling out the data of interest
output_length <- length(output)
data_string <- str_detect(output, "year,yday")
data_position <- match(TRUE, data_string)
tail_position <- output_length - data_position + 1
daymet_data <- tail(output, tail_position)
# Splitting the data strings
daymet_data <- str_split(daymet_data, ",", simplify = TRUE)
# Putting data into tibble
daymet_data <- as_tibble(daymet_data)
daymet_data <- row_to_names(daymet_data, row_number = 1)
# Converting data to numeric
daymet_data <- daymet_data |>
  mutate_if(is.character, as.numeric)


# Writing a function to stream Daymet data from a URL
# Function arguments:
# Latitude as string
lat <- ""
# Longitude as string
lon <- ""
# Comma-separated Daymet variables of interest as string, a selection of: dayl,prcp,srad,swe,tmax,tmin,vp
vars <- ""
# Start Date as string, in format YYYY-MM-DD
start <- ""
# End Date as string, in format YYYY-MM-DD
end <- ""

daymet_download <- function(lat, lon, vars, start, end){
  # Building URL
  url <- str_glue('https://daymet.ornl.gov/single-pixel/api/data?',
                  'lat=', lat,
                  '&lon=', lon,
                  '&vars=', vars,
                  '&start=', start,
                  '&end=', end,
                  sep = "")
  # Opening a streaming connection
  connection <- curl(url)
  open(connection)
  # Streaming the data as text
  output <- readLines(connection)
  # Closing the streaming connection
  close(connection)
  # Pulling out the data of interest
  output_length <- length(output)
  data_string <- str_detect(output, "year,yday")
  data_position <- match(TRUE, data_string)
  tail_position <- output_length - data_position + 1
  daymet_data <- tail(output, tail_position)
  # Splitting the data strings
  daymet_data <- str_split(daymet_data, ",", simplify = TRUE)
  # Putting data into tibble
  daymet_data <- as_tibble(daymet_data, .name_repair = 'unique')
  daymet_data <- row_to_names(daymet_data, row_number = 1)
  # Converting data to numeric
  daymet_data <- daymet_data |>
    mutate_if(is.character, as.numeric)
}

# Running the function with specified arguments
lat <- "35.9621"
lon <- "-84.2916"
vars <- "tmax"
start <- "2020-01-01"
end <- "2020-01-02"
daymet_data <- daymet_download(lat, lon, vars, start, end)
