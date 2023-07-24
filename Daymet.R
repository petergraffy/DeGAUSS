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
# WE WILL ALSO NEED TO MANUALLY CONVERT EVENT DATES INTO SEQUENTIAL YEAR DAYS (OR VICE-VERSA)

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


# Let's try downloading Daymet data a different way, scraping it directly from the web
