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
library(ncdf4)
library(tidyverse)
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


#########
# ON SECOND THOUGHT, I'M NOT SURE IF THE DAYMET API IS HIPAA COMPLIANT. I REACHED OUT TO THEM TO ASK.
# EVERYTHING ABOVE USES THE DAYMET API (FOR SINGLE PIXEL EXTRACTION).
# IF IT IS COMPLIANT, THEN GREAT. IF IT IS NOT, THEN WE WILL NEED TO DOWNLOAD DAYMET DATA THAT SPANS CHICAGO (AND WHEREVER ELSE PATIENTS MIGHT BE).
# WE COULD PROBABLY USE THE DAYMETR PACKAGE FOR THAT SINCE LOCATIONS AREN'T SPECIFIC, OR BUILD OUR OWN TOOL. AGAIN, DAYMETR WOULD BE DOWNLOADING DATA FOR AN ENTIRE YEAR AT MINIMUM.
# THEN WE'D NEED TO EXTRACT DATA FROM THE NETCDF, AND LINK IN THE PATIENT LON/LAT COORDINATES TO THE DAYMET TILES, ALL INTERNALLY.
#########
# Trying the NetCDF route #
# Reading in the patient address data
sample_addresses <- read_csv("sample_addresses.csv")

# Finding the min and max longitude and latitude out of all the patient address coordinates
min_lon <- min(sample_addresses$lon)
max_lon <- max(sample_addresses$lon)
min_lat <- min(sample_addresses$lat)
max_lat <- max(sample_addresses$lat)

# In order to preserve patient privacy, adding a random amount of noise to the bounding box of patient addresses
# Each bounding box point (e.g., maximum latitude) will be extended by an additional 1-22 kilometers
noise <- runif(4, min = 0.01, max = 0.2)
min_lon <- min_lon - noise[1]
max_lon <- max_lon + noise[2]
min_lat <- min_lat - noise[3]
max_lat <- max_lat + noise[4]

# Downloading the Daymet NetCDF data defined by the coordinate bounding box
download_daymet_ncss(location = c(max_lat, min_lon, min_lat, max_lon), # Bounding box defined as top left / bottom right pair c(lat, lon, lat, lon)
                     start = 2022,
                     end = 2022,
                     param = "tmax",
                     mosaic = "na",
                     silent = FALSE,
                     path = getwd())

# Loading the NetCDF file downloaded from Daymet
daymet_data <- nc_open("tmax_daily_2022_ncss.nc")

# Checking the variables in the file
attributes(daymet_data$var)
# Checking the dimensions in the file
attributes(daymet_data$dim)

# Extracting latitude and longitude
lat <- ncvar_get(daymet_data, "lat")
lon <- ncvar_get(daymet_data, "lon")

# Extracting time (365 sequential days since 1950-01-01 00:00:00)
ncatt_get(daymet_data, "time")
time <- ncvar_get(daymet_data, "time")

# Extracting tmax as "matrix slices" (3rd dimension is time)
tmax <- ncvar_get(daymet_data, "tmax")
dim(tmax) # tmax for every lat/lon, and every day of the year
# Replacing FillValues with NA
fillvalue <- ncatt_get(daymet_data, "tmax", "_FillValue")
fillvalue <- fillvalue$value
tmax[tmax == fillvalue] <- NA

# Converting time to calendar dates
time <- as_date(time, origin = "1950-01-01")

# Creating dataframe of coordinates, time, and tmax
# Just doing first day for now
lonlattime <- as.matrix(expand.grid(lon, lat, time[1])) # FIX THIS
tmax_vector <- as.vector(tmax[, , 1])
tmax_df <- data.frame(cbind(lonlattime, tmax_vector))
tmax_df <- tmax_df |>
  rename(longitude = Var1,
         latitude = Var2,
         date = Var3,
         tmax = tmax_vector)

PLOT THE ABOVE, CHANGE
CONFIRM THAT ABOVE WORKS FOR LEAP YEARS
UNANSWERED QUESTION: HOW TO LINK A PATIENT LON/LAT TO A DAYMET LOCATION LON/LAT