# Specifying user-customized options
csv_filename <- "sample_addresses_dates.csv" # Input CSV file, containing columns of ID variable, lat, lon, and optionally event dates
year_start <- 2009 # Start year of Daymet NetCDF data download
year_end <- 2022 # End year of Daymet NetCDF data download
daymet_variables <- "tmax,tmin" # Comma-separated string of Daymet variables: tmax,tmin,srad,vp,swe,prcp,dayl
lag <- 7 # Lag in days for Daymet data to be pulled (days prior to event dates - event dates will be included as well)
#### DO NOT CHANGE ANYTHING AFTER THIS ####

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
library(gtools)
library(data.table)
library(dht)

# Greeting users
greeting()

# Writing functions
# Creating function to import and process the input addresses and optionally event dates
import_data <- function(.csv_filename = csv_filename, .year_start = year_start, .year_end = year_end) {
  # Checking that the input data is a CSV file
  if (!str_detect(.csv_filename, ".csv$")) {
    stop(call. = FALSE, 'Input file must be a CSV.')
  }
  # Reading in the input data
  input_data <- read_csv(.csv_filename)
  # Assigning the very first column in the input data as the ID column
  id <- input_data %>%
    select(1) %>%
    colnames()
  # Ensuring that numeric lat and lon are in the input data, and quitting if not
  tryCatch({
    check_for_column(input_data, column_name = "lat")
    check_for_column(input_data, column_name = "lon")
    check_for_column(input_data, column_name = "lat", column = input_data$lat, column_type = "numeric")
    check_for_column(input_data, column_name = "lon", column = input_data$lon, column_type = "numeric")
  }, error = function(e) {
    print(e)
    stop(call. = FALSE)
  }, warning = function(w) {
    print(w)
    stop(call. = FALSE)
  })
  # Filtering out rows in the input data where lat or lon are missing
  input_data <- input_data %>%
    filter(!is.na(lat) & !is.na(lon))
  # Separating the ID and address coordinates out into their own dataset
  addresses <- input_data %>%
    select(!!id, lat, lon)
  # Separating the ID and any other columns that aren't coordinates (presumed to be event dates) out into their own dataset
  event_dates <- input_data %>%
    select(-lat, -lon)
  # If the ID column in addresses is equal to lat or lon (no ID column was supplied by the user) then creating an ID column in addresses and event_dates that is just the row number
  if (sum(addresses %>% select(!!id) == addresses %>% select(lat), na.rm = TRUE) > 0 | sum(addresses %>% select(!!id) == addresses %>% select(lon), na.rm = TRUE) > 0) {
    addresses <- addresses %>%
      mutate(id = as.character(1:nrow(addresses))) %>%
      relocate(id)
    event_dates <- event_dates %>%
      mutate(id = as.character(1:nrow(event_dates))) %>%
      relocate(id)
    id <- addresses %>%
      select(id) %>%
      colnames()
  }
  # Ensuring that the ID column is a character
  tryCatch({
    addresses <- addresses %>%
      mutate(across(as.name(id), as.character))
    event_dates <- event_dates %>%
      mutate(across(as.name(id), as.character))
  }, error = function(e) {
    stop(call. = FALSE, 'Could not format ID variable as a string.')
  }, warning = function(w) {
    stop(call. = FALSE, 'Could not format ID variable as a string.')
  })
  # If any columns that are not the ID column are characters, then converting them to dates
  tryCatch({
    event_dates <- event_dates %>%
      mutate(across(where(is.character) & !as.name(id), mdy))
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    stop(call. = FALSE, paste('Could not format user-supplied event dates as dates.',
                              'Please provide event dates as YYYY-MM-DD or MM/DD/YYYY.'))
  })
  # If the only column in the event_dates dataset is the ID column (no event dates were supplied by the user), then filling in every single day between year_start and year_end
  if (.year_start > .year_end) {
    stop(call. = FALSE, 'Please ensure that year_start is less than or equal to year_end.')
  }
  tryCatch({
    year_start_date <- ymd(.year_start, truncated = 2L)
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    stop(call. = FALSE, 'Please provide year_start as a numeric four-digit year: YYYY.')
  })
  tryCatch({
    year_end_date <- ymd(.year_end, truncated = 2L)
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    stop(call. = FALSE, 'Please provide year_end as a numeric four-digit year: YYYY.')
  })
  if (leap_year(year_end_date)) {
    year_end_date <- year_end_date + 365
  } else {
    year_end_date <- year_end_date + 364
  }
  if (!FALSE %in% (colnames(event_dates) == id)) {
    date_range <- as.data.frame(matrix(rep(year_start_date:year_end_date, nrow(event_dates)), ncol = length(year_start_date:year_end_date), byrow = TRUE))
    names(date_range) <- c(paste0("date_", 1:ncol(date_range)))
    date_range <- date_range %>%
      mutate_all(as_date)
    event_dates <- cbind(event_dates, date_range)
  }
  # Converting the input addresses to a SpatVector
  coords <- vect(addresses, geom = c("lon", "lat"), crs = "+proj=longlat +ellips=WGS84")
  # Returning a list of objects needed later
  out <- list("id" = id, "addresses" = addresses, "event_dates" = event_dates, "year_start_date" = year_start_date, "year_end_date" = year_end_date, "coords" = coords)
  return(out)
}

# Creating function to link the Daymet data coordinates to the input address coordinates for a specified ID and specified date
daymet_select <- function(id_var, date_var, silent = FALSE, .template = template, .year_start_date = year_start_date, .year_end_date = year_end_date, .id = id, .time_dict = time_dict, .layer_dict = layer_dict, .daymet_data = daymet_data, .proj_coords = proj_coords) {
  # Resetting the data to append with the output data template
  to_append <- .template
  # If the specified date is beyond the Daymet data available, then breaking out of the function
  tryCatch({
    date_var <- as_date(date_var)
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    stop(call. = FALSE, 'Ensure that input date is formatted as YYYY-MM-DD.')
  })
  if (date_var < .year_start_date | date_var > .year_end_date) {
    return(NA)
  }
  # Taking care of leap years, per Daymet conventions (12/31 is switched to 12/30)
  if (leap_year(date_var) & month(date_var) == 12 & day(date_var) == 31) {
    date_var <- date_var - 1
  }
  # Filling in the data to append with the ID and date
  tryCatch({
    id_var <- as.character(id_var)
  }, error = function(e) {
    stop(call. = FALSE, 'Could not format ID variable as a string.')
  }, warning = function(w) {
    stop(call. = FALSE, 'Could not format ID variable as a string.')
  })
  to_append <- to_append %>%
    mutate(!!.id := !!id_var,
           date := !!date_var)
  # Looking up the number for the specified date in the time dictionary
  time_dict_col <- paste0("date_", year(date_var))
  date_num <- .time_dict %>%
    filter(get(time_dict_col) == date_var) %>%
    select(number) %>%
    pull
  # Extracting the Daymet data for the specified ID and specified date
  layers <- .layer_dict %>%
    filter(yr == year(date_var))
  daymet_extract <- function(dm_var, multiplier) {
    date_num_subset <- date_num + (365 * multiplier)
    daymet_linked <- terra::extract(subset(.daymet_data, date_num_subset),
                                    subset(.proj_coords, .proj_coords[[names(.proj_coords)]] == id_var),
                                    bind = TRUE)
    daymet_variable_df <- as.data.frame(daymet_linked)
    # Renaming the Daymet data that was just added and joining it to the data to append
    rename_variable_name <- paste0(dm_var, "_", date_num)
    daymet_variable_df <- daymet_variable_df %>%
      rename(!!dm_var := !!rename_variable_name)
    return(daymet_variable_df)
  }
  daymet_extract_output <- map2_df(layers$dm_var, layers$multiplier, daymet_extract)
  daymet_extract_output <- daymet_extract_output %>%
    fill(-!!.id, .direction = "downup") %>%
    distinct()
  tryCatch({
    if (silent == TRUE) {
      to_append <- suppressMessages(rows_update(to_append, daymet_extract_output))
    } else {
      to_append <- rows_update(to_append, daymet_extract_output)
    }
  }, error = function(e) {
    stop(call. = FALSE, paste('Could not complete Daymet data merge, user-supplied IDs must be unique.',
                              id_var, 'is not unique.'))
  }, warning = function(w) {
    print(w)
  })
  return(to_append)
}

# Importing and processing the input data
import_data_out <- import_data()
id <- import_data_out$id
addresses <- import_data_out$addresses
event_dates <- import_data_out$event_dates
year_start_date <- import_data_out$year_start_date
year_end_date <- import_data_out$year_end_date
coords <- import_data_out$coords

# Finding the min and max longitude and latitude out of all the input address coordinates
min_lon <- min(addresses$lon)
max_lon <- max(addresses$lon)
min_lat <- min(addresses$lat)
max_lat <- max(addresses$lat)

# In order to preserve privacy, adding a random amount of noise to the bounding box of input addresses
# Each bounding box point (e.g., maximum latitude) will be extended by an additional 1-22 kilometers
noise <- runif(4, min = 0.01, max = 0.2)
min_lon <- min_lon - noise[1]
max_lon <- max_lon + noise[2]
min_lat <- min_lat - noise[3]
max_lat <- max_lat + noise[4]

# Downloading the Daymet NetCDF data defined by the coordinate bounding box: One file per variable per year
# THIS PROCEDURE EXTRACTS THE BOUNDING BOX FROM THE INPUT COORDINATES + NOISE, BUT WE'LL ALSO WANT TO ADD AN OPTION THAT LETS THE USER SPECIFY THEIR OWN BOUNDING BOX
# Additionally creating a template to link all Daymet data to - new observations will all be appended (vertical dataset)
template_colnames <- c(id, "date")
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
  template_colnames[[length(template_colnames) + 1]] <- variable
}
template <- as.data.table(matrix(nrow = 0, ncol = length(template_colnames)))
colnames(template) <- template_colnames
template <- rbindlist(list(template, list(NA)), fill = TRUE)
template <- template %>%
  mutate_if(is.logical, as.numeric)

# Loading the NetCDF files downloaded from Daymet as a SpatRaster raster stack
netcdf_list <- list.files(pattern = "_ncss.nc$")
# Checking for extra NetCDF files
if (length(netcdf_list) > (length(seq(year_start, year_end)) * length(daymet_variables))) {
  stop(call. = FALSE, 'Ensure that there are not extra NetCDF files in folder where Daymet data was downloaded to.')
}
time_dict <- tibble(number = 1:365)
layer_dict_colnames <- c("dm_var", "yr", "multiplier")
layer_dict <- as_tibble(matrix(nrow = length(netcdf_list), ncol = length(layer_dict_colnames)), .name_repair = ~ layer_dict_colnames)
for (i in 1:length(netcdf_list)) {
  daymet_load <- rast(netcdf_list[i])
  # Extracting the year and Daymet variable from the file loaded in
  yr <- str_extract(netcdf_list[i], "[0-9]{4}")
  dm_var <- unlist(str_split(netcdf_list[i], "_"))[1]
  # Creating a dictionary to link numbers 1–365 to a date in a year
  origin <- as_date(paste0(yr, "-01-01")) - 1 # Numbers count days since origin
  new_date_col <- paste0("date_", yr)
  if (!new_date_col %in% colnames(time_dict)) {
    time_dict <- time_dict %>%
      mutate(!!new_date_col := as_date(number, origin = origin))
  }
  # Creating a dictionary to assign a multiplier for identifying different layers in the raster stack
  layer_dict$dm_var[i] <- dm_var
  layer_dict$yr[i] <- yr
  layer_dict$multiplier[i] <- i - 1
  # Stacking the Daymet data rasters
  if (i == 1) {
    daymet_data <- daymet_load
  } else {
    daymet_data <- c(daymet_data, daymet_load)
  }
}

# Changing the coordinate reference system of the input addresses so they match that of Daymet
new_crs <- crs(daymet_data, proj = TRUE)
proj_coords <- project(coords, new_crs)

# Filtering out any rows in event_dates where the event dates are all missing
event_dates <- event_dates %>%
  filter(if_any(-!!id, ~ !is.na(.)))

# Combining all the event dates into a list
event_dates <- event_dates %>%
  mutate(event_date_list = pmap(select(., -!!id), c)) %>%
  select(!!id, event_date_list)
event_dates$event_date_list <- map(event_dates$event_date_list, unname)
event_dates$event_date_list <- map(event_dates$event_date_list, na.omit)
tryCatch({
  event_dates$event_date_list <- map(event_dates$event_date_list, as_date)
}, error = function(e) {
  print(e)
}, warning = function(w) {
  stop(call. = FALSE, 'Ensure that input date is formatted as YYYY-MM-DD.')
})

# For each event date list, adding in new dates that correspond to the specified lag
tryCatch({
  if (!(lag %% 1 == 0) | is.logical(lag)) {
    stop(call. = FALSE)
  }
}, error = function(e) {
  stop(call. = FALSE, 'Ensure that user-supplied lag is a numeric whole number.')
}, warning = function(w) {
  print(w)
})
if (lag > 0) {
  for (lag_step in 1:lag) {
    new_dates <- map(event_dates$event_date_list, ~ .x - lag_step)
    event_dates$new_dates <- new_dates
    if (lag_step == 1) {
      event_dates$event_date_list_lag <- map2(event_dates$event_date_list, event_dates$new_dates, c)
    } else {
      event_dates$event_date_list_lag <- map2(event_dates$event_date_list_lag, event_dates$new_dates, c)
    }
    event_dates <- event_dates %>%
      select(-new_dates)
  }
  event_dates <- event_dates %>%
    select(-event_date_list) %>%
    rename(event_date_list = event_date_list_lag)
}

# Sorting each event date list, and removing duplicates
event_dates$event_date_list <- map(event_dates$event_date_list, sort)
event_dates$event_date_list <- map(event_dates$event_date_list, unique)

# Extending each ID to be a list of length equal to the event date list
event_dates$id_list <- map(event_dates$event_date_list, as.character)
tryCatch({
  event_dates$id_list <- map2(event_dates$id_list, unlist(unname(as.vector(event_dates[as.character(id)]))), str_replace, pattern = ".*")
}, error = function(e) {
  stop(call. = FALSE, 'User-supplied IDs are entirely missing.')
}, warning = function(w) {
  print(w)
})

# Linking the Daymet data coordinates to the input address coordinates across all event dates
daymet_select_output <- map2(unlist(event_dates$id_list), unlist(event_dates$event_date_list), daymet_select, silent = TRUE)
daymet_select_output <- daymet_select_output[!is.na(daymet_select_output)]
main_dataset <- rbindlist(daymet_select_output)

# Handling cases where none of the event dates were within the date range of loaded Daymet data
if (nrow(main_dataset) == 0 & ncol(main_dataset) == 0) {
  main_dataset <- template
}

# Handling cases where an address was outside of the bounding box of loaded Daymet data
main_dataset <- main_dataset %>%
  filter(if_any(-c(!!id, date), ~ !is.na(.)))

# Sorting and de-duplicating the final results (duplicates could have resulted from leap years)
get_id <- id
main_dataset <- main_dataset %>%
  mutate(sort1 = factor(get(get_id), ordered = TRUE, levels = unique(mixedsort(get(get_id)))),
         sort2 = factor(date, ordered = TRUE, levels = unique(mixedsort(date)))) %>%
  arrange(sort1, sort2) %>%
  select(-c(sort1, sort2)) %>%
  distinct()

# Writing the results out as a CSV file
csv_out <- paste0(unlist(str_split(csv_filename, ".csv"))[1], "_daymet", ".csv")
write.csv(main_dataset, csv_out, na = "", row.names = FALSE)

# Deleting the NetCDF files that were downloaded from disk
rm(list = ls(all.names = TRUE))
unlink(list.files(pattern = "_ncss.nc$"), force = TRUE)

# NEXT STEPS:
# - ADD IN CODE FOR EXTRA OPTIONS (DAYMETR OPTIONS, BOUNDING BOX OPTIONS)
# - TEST WITH CAPRICORN DAYMET