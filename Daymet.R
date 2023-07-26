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
# This procedure extracts the bounding box from the patient coordinates + noise, but we'll also want to add an option that lets the user specify their own bounding box
year_start <- 2020
year_end <- 2021
daymet_variables <- "tmax,tmin" # Comma-separated string of Daymet variables: tmax,tmin,srad,vp,swe,prcp,dayl
daymet_variables <- str_remove(daymet_variables, " ")
daymet_variables <- str_split(daymet_variables, ",", simplify = TRUE)
for (variable in daymet_variables){
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
# Loading the NetCDF file downloaded from Daymet
daymet_data <- nc_open("tmax_daily_2021_ncss.nc")

# Extracting latitude and longitude
lat <- ncvar_get(daymet_data, "lat")
lon <- ncvar_get(daymet_data, "lon")

# Keeping the NetCDF latitude and longitude points that overlap and are closest to the requested bounding box
first_index <- 1
last_index <- ncol(lat)
max_dif <- lat[, first_index] - max_lat
min_dif <-  min_lat - lat[, last_index]
dif <- data.frame(max_dif, min_dif)
dif$dif <- abs(dif$max_dif - dif$min_dif)
dif <- subset(dif, min_dif > 0 & max_dif > 0)
dif <- subset(dif, dif == min(dif))
selected <- as.numeric(rownames(dif))
lat <- lat[selected ,]

first_index <- 1
last_index <- nrow(lon)
max_dif <- lon[last_index ,] - max_lon
min_dif <-  min_lon - lon[first_index ,]
dif <- data.frame(max_dif, min_dif)
dif$dif <- abs(dif$max_dif - dif$min_dif)
dif <- subset(dif, min_dif > 0 & max_dif > 0)
dif <- subset(dif, dif == min(dif))
selected <- as.numeric(rownames(dif))
lon <- lon[, selected]

# Extracting time (365 sequential days since 1950-01-01 00:00:00)
time_units <- ncatt_get(daymet_data, "time")$units
time_units <- str_extract(time_units, "[0-9]{4}-[0-9]{2}-[0-9]{2}")
time <- ncvar_get(daymet_data, "time")

# Extracting the Daymet variable as "matrix slices" (3rd dimension is time)
daymet_variable <- ncvar_get(daymet_data, "tmax")
# Replacing FillValues with NA
fillvalue <- ncatt_get(daymet_data, "tmax", "_FillValue")
fillvalue <- fillvalue$value
daymet_variable[daymet_variable == fillvalue] <- NA

# Converting time to calendar dates
time <- as_date(time, origin = time_units)

# Creating dataframe of coordinates, time, and the Daymet variable
# Just doing first day for now
lonlattime <- as.matrix(expand.grid(lon, lat, time[1]))
daymet_variable_vector <- as.vector(daymet_variable[, , 1])
daymet_variable_df <- data.frame(cbind(lonlattime, daymet_variable_vector))
daymet_variable_df <- daymet_variable_df %>%
  rename(longitude = Var1,
         latitude = Var2,
         date = Var3,
         tmax = daymet_variable_vector) %>%
  mutate_at(c('longitude', 'latitude', 'tmax'), as.numeric)  %>%
  mutate_at('date', as_date)

# Plotting the data
ggplot(daymet_variable_df, aes(x = longitude, y = latitude, fill = tmax)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#010891", "#3ce4f0", "#f0f037", "#e0901f", "#910401"))
# AT THE END OF PROCESSING, DELETE THE NETCDF FILE THAT WAS JUST WORKED THROUGH FROM R MEMORY AND DISK

NEXT STEP: LINK THE VARIABLE MATCHED WITH DATE INTO THE COORDS (ONLY HAVE TO EXTRACT COORDS ONCE)
NEXT NEXT STEP: CREATE A SELECTOR SO I CAN PICK OUT WHAT DATE OF DATA I WANT
NEXT NEXT NEXT STEP: MATCH THE DAYMET LOCATION LON/LAT TO THE PATIENT LON/LAT
NEXT NEXT NEXT NEXT STEP: CREATE MULTIPLE VARIABLES LOOP
NEXT NEXT NEXT NEXT NEXT STEP: CREATE MULTIPLE YEARS LOOP