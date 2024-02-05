# Loading necessary packages
library(daymetr)
library(terra)
library(dplyr)

# Downloading the Daymet NetCDF
download_daymet_ncss(location = c(42.154247, -88.263390, 41.470117, -87.525706), # Bounding box defined as top left / bottom right pair c(lat, lon, lat, lon)
                     start = 2020,
                     end = 2020,
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
