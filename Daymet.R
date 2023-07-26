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
download_daymet_ncss(location = c(max_lat, min_lon, min_lat, max_lon), # Bounding box defined as top left / bottom right pair c(lat, lon, lat, lon)
                     start = 2020,
                     end = 2020,
                     param = "tmax",
                     frequency = "daily",
                     mosaic = "na",
                     silent = FALSE,
                     force = TRUE,
                     path = getwd())

# Loading the NetCDF file downloaded from Daymet
daymet_data <- nc_open("tmax_daily_2020_ncss.nc")

# Checking the variables in the file
attributes(daymet_data$var)
# Checking the dimensions in the file
attributes(daymet_data$dim)

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
lonlattime <- as.matrix(expand.grid(lon, lat, time[1]))
tmax_vector <- as.vector(tmax[, , 1])
tmax_df <- data.frame(cbind(lonlattime, tmax_vector))
tmax_df <- tmax_df %>%
  rename(longitude = Var1,
         latitude = Var2,
         date = Var3,
         tmax = tmax_vector) %>%
  mutate_at(c('longitude', 'latitude', 'tmax'), as.numeric)  %>%
  mutate_at('date', as_date)

# Plotting the data
ggplot(tmax_df, aes(x = longitude, y = latitude, fill = tmax)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#010891", "#3ce4f0", "#f0f037", "#e0901f", "#910401"))


NEXT STEP: LINK THE VARIABLE MATCHED WITH YEAR INTO THE COORDS (ONLY HAVE TO EXTRACT COORDS ONCE)
NEXT NEXT STEP: CREATE A SELECTOR SO I CAN PICK OUT WHAT DATE OF DATA I WANT
NEXT NEXT NEXT STEP: PULL IN TMIN AND TMAX
UNANSWERED QUESTION: HOW TO LINK A PATIENT LON/LAT TO A DAYMET LOCATION LON/LAT