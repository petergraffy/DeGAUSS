# Creating function to specify user-customized options
set_options <- function(csv_filename, year_start, year_end, daymet_variables, lag, min_lon = 0, max_lon = 0, min_lat = 0, max_lat = 0, region = "na", id_column = NA, extra_columns = NA) {
  # csv_filename: Input CSV file, containing columns of lat, lon, and optionally event dates, a user-supplied ID column, and/or extra columns.
  # year_start: Start year of Daymet NetCDF data download.
  # year_end: End year of Daymet NetCDF data download.
  # daymet_variables: Comma-separated string of Daymet variables: "tmax,tmin,srad,vp,swe,prcp,dayl".
  # lag: Lag in days for Daymet data to be pulled (number of days prior to event dates - event dates will be included as well).
  # min_lon: [Optional] Minimum longitude (in decimal degrees) of bounding box for Daymet data download. Default is to infer bounding box from address coordinates.
  # max_lon: [Optional] Maximum longitude (in decimal degrees) of bounding box for Daymet data download. Default is to infer bounding box from address coordinates.
  # min_lat: [Optional] Minimum latitude (in decimal degrees) of bounding box for Daymet data download. Default is to infer bounding box from address coordinates.
  # max_lat: [Optional] Maximum latitude (in decimal degrees) of bounding box for Daymet data download. Default is to infer bounding box from address coordinates.
  # region: [Optional] Daymet spatial region ("na" for continental North America, "hi" for Hawaii, or "pr" for Puerto Rico). Default is continental North America.
  # id_column: [Optional] Name (as string) of user-supplied ID column (cannot be named "id"). Useful for later merging Daymet data with another dataset by the user-supplied ID column. Can contain repeats. Default is no user-supplied ID column included. In this case, the ID variable assigned is the row number of the input data.
  # extra_columns: [Optional] Comma-separated string of column names for any extra columns in the dataset that are not lat, lon, event dates, or a user-supplied ID column. Extra columns cannot be named "id", and the column names cannot contain a comma. Default is no extra columns included.
  
  # Returning a list of objects needed later
  out <- list("csv_filename" = csv_filename, "year_start" = year_start, "year_end" = year_end, "daymet_variables" = daymet_variables, "lag" = lag, "min_lon" = min_lon, "max_lon" = max_lon, "min_lat" = min_lat, "max_lat" = max_lat, "region" = region, "id_column" = id_column, "extra_columns" = extra_columns)
  return(out)  
}

# Specifying user-customized options
#### DO NOT CHANGE ANYTHING BEFORE THIS ####
set_options_out <- set_options(csv_filename = "loyalty_geocoded.csv",
                               year_start = 2010,
                               year_end = 2022,
                               daymet_variables = "tmax,tmin",
                               lag = 7,
                               min_lon = -89.570714,
                               max_lon = -85.952434,
                               min_lat = 39.669027,
                               max_lat = 43.095030,
                               id_column = "patid",
                               extra_columns = "enc_id,address_number")
#### DO NOT CHANGE ANYTHING AFTER THIS ####
csv_filename <- set_options_out$csv_filename
year_start <- set_options_out$year_start
year_end <- set_options_out$year_end
daymet_variables <- set_options_out$daymet_variables
lag <- set_options_out$lag
min_lon <- set_options_out$min_lon
max_lon <- set_options_out$max_lon
min_lat <- set_options_out$min_lat
max_lat <- set_options_out$max_lat
region <- set_options_out$region
id_column <- set_options_out$id_column
extra_columns <- set_options_out$extra_columns

# Daymet weather variables include daily minimum and maximum temperature, precipitation,
# vapor pressure, shortwave radiation, snow water equivalent, and day length produced on
# a 1 km x 1 km gridded surface over continental North America and Hawaii from 1980 and
# over Puerto Rico from 1950 through the end of the most recent full calendar year.

# Daymet data documentation: https://daac.ornl.gov/DAYMET/guides/Daymet_Daily_V4.html

# Note: The Daymet calendar is based on a standard calendar year. All Daymet years, including
# leap years, have 1–365 days. For leap years, the Daymet data include leap day (February 29)
# and December 31 is discarded from leap years to maintain a 365-day year.

#### IF YOU HAVEN'T PREVIOUSLY INSTALLED ANY OF THE PACKAGES BELOW, THEN UN-COMMENT AND RUN THE RELEVANT CODE LINES ####
#install.packages('daymetr')
#install.packages('tidyverse')
#install.packages('terra')
#install.packages('gtools')
#install.packages('data.table')
#install.packages('remotes')
#remotes::install_github('degauss-org/dht')

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
# Creating function to import and process the input addresses and optionally event dates, a user-supplied ID column, and/or extra columns
import_data <- function(.csv_filename = csv_filename, .year_start = year_start, .year_end = year_end, .id_column = id_column, .extra_columns = extra_columns) {
  # Checking that the input data is a CSV file
  if (!str_detect(.csv_filename, ".csv$")) {
    stop(call. = FALSE, 'Input file must be a CSV.')
  }
  # Reading in the input data
  input_data <- fread(.csv_filename, header = TRUE, sep = ",")
  input_data <- as_tibble(input_data)
  # Creating an ID variable in the input data that is just the row number
  if ("id" %in% colnames(input_data)) {
    stop(call. = FALSE, 'Input file cannot contain a column named "id".')
  }
  input_data <- input_data %>%
    mutate(id = as.character(1:nrow(input_data))) %>%
    relocate(id)
  # Assigning the ID variable in the input data as the ID variable object
  id <- input_data %>%
    select(id) %>%
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
  # Separating the ID variable and user-supplied ID column out into their own dataset (will just be the ID variable if no user-supplied ID column was given)
  if (!(!!.id_column %in% colnames(input_data)) & !is.na(.id_column)) {
    stop(call. = FALSE, 'User-supplied ID column not in input file.')
  }
  if (!is.na(.id_column)) {
    id_column_df <- input_data %>%
      select(!!id, !!.id_column)
    input_data <- input_data %>%
      select(-!!.id_column)
  } else {
    id_column_df <- input_data %>%
      select(!!id)
  }
  # Separating the ID variable and extra columns out into their own dataset (will just be the ID variable if no extra columns were given)
  if (!is.na(.extra_columns)) {
    .extra_columns <- str_remove(.extra_columns, " ")
    .extra_columns <- str_split(.extra_columns, ",", simplify = TRUE)
    for (column in .extra_columns) {
      if (!(column %in% colnames(input_data))) {
        stop(call. = FALSE, paste0('Extra column ', column, ' not in input file.'))
      }
    }
    extra_columns_df <- input_data %>%
      select(!!id, matches(.extra_columns))
    input_data <- input_data %>%
      select(-matches(.extra_columns))
  } else {
    extra_columns_df <- input_data %>%
      select(!!id)
  }
  # Separating the ID variable and address coordinates out into their own dataset
  addresses <- input_data %>%
    select(!!id, lat, lon)
  # Separating the ID variable and any other columns that aren't coordinates (event dates) out into their own dataset
  event_dates <- input_data %>%
    select(-lat, -lon)
  # If any columns that are not the ID variable are characters, then removing anything after a space (any times included in dates)
  event_dates <- event_dates %>%
    mutate(across(where(is.character) & !as.name(id), ~str_remove_all(., " .*")))
  # If any columns that are not the ID variable are characters, then converting them to dates
  tryCatch({
    event_dates <- event_dates %>%
      mutate(across(where(is.character) & !as.name(id) & where(~any(str_detect(., "/"))), mdy))
    event_dates <- event_dates %>%
      mutate(across(where(is.character) & !as.name(id) & where(~any(str_detect(., "-"))), ymd))
  }, error = function(e) {
    print(e)
  }, warning = function(w) {
    stop(call. = FALSE, paste('Could not format user-supplied event dates as dates.',
                              'Please provide event dates as YYYY-MM-DD or MM/DD/YYYY.'))
  })
  # If there is no date column(s) in the event_dates dataset (no event dates were supplied by the user), then filling in every single day between year_start and year_end
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
  if (!"Date" %in% unlist(sapply(event_dates, class))) {
    date_range <- as.data.frame(matrix(rep(year_start_date:year_end_date, nrow(event_dates)), ncol = length(year_start_date:year_end_date), byrow = TRUE))
    names(date_range) <- c(paste0("date_", 1:ncol(date_range)))
    date_range <- date_range %>%
      mutate_all(as_date)
    event_dates <- cbind(event_dates, date_range)
  }
  # Converting the input addresses to a SpatVector
  coords <- vect(addresses, geom = c("lon", "lat"), crs = "+proj=longlat +ellips=WGS84")
  # Returning a list of objects needed later
  out <- list("id" = id, "id_column_df" = id_column_df, "extra_columns_df" = extra_columns_df, "addresses" = addresses, "event_dates" = event_dates, "year_start_date" = year_start_date, "year_end_date" = year_end_date, "coords" = coords)
  return(out)
}

# Creating function to download and load the Daymet NetCDF data
daymet_download_load <- function(.min_lon = min_lon, .max_lon = max_lon, .min_lat = min_lat, .max_lat = max_lat, .id = id, .daymet_variables = daymet_variables, .year_start = year_start, .year_end = year_end, .region = region) {
  # If the Daymet data bounding box was not supplied by the user, then inferring the bounding box from the input address coordinates
  if (.min_lon == 0 & .max_lon == 0 & .min_lat == 0 & .max_lat == 0) {
    # Finding the min and max longitude and latitude out of all the input address coordinates
    .min_lon <- min(addresses$lon)
    .max_lon <- max(addresses$lon)
    .min_lat <- min(addresses$lat)
    .max_lat <- max(addresses$lat)
    # In order to preserve privacy, adding a random amount of noise to the bounding box of input addresses
    # Each bounding box point (e.g., maximum latitude) will be extended by an additional 1-22 kilometers
    noise <- runif(4, min = 0.01, max = 0.2)
    .min_lon <- .min_lon - noise[1]
    .max_lon <- .max_lon + noise[2]
    .min_lat <- .min_lat - noise[3]
    .max_lat <- .max_lat + noise[4]
  }
  # Otherwise, verifying that the user-supplied bounding box consists of numeric coordinates
  if (!is.numeric(.min_lon) | !is.numeric(.max_lon) | !is.numeric(.min_lat) | !is.numeric(.max_lat)) {
    stop(call. = FALSE, 'Please ensure that user-supplied Daymet bounding box has coordinates in numeric decimal degrees.')
  }
  # Additionally verifying that for user-supplied bounding box coordinates, the minimum coordinates are less than the maximum coordinates
  if (!(.min_lon < .max_lon)) {
    stop(call. = FALSE, paste0('Please ensure that bounding box min_lon: ', .min_lon,
                              ' is less than bounding box max_lon: ', .max_lon, '.'))
  }
  if (!(.min_lat < .max_lat)) {
    stop(call. = FALSE, paste0('Please ensure that bounding box min_lat: ', .min_lat,
                               ' is less than bounding box max_lat: ', .max_lat, '.'))
  }
  # Downloading the Daymet NetCDF data defined by the coordinate bounding box: One file per variable per year
  # Additionally creating a template to link all Daymet data to - new observations will all be appended (vertical dataset)
  template_colnames <- c(.id, "date")
  .daymet_variables <- str_remove(.daymet_variables, " ")
  .daymet_variables <- str_split(.daymet_variables, ",", simplify = TRUE)
  for (variable in .daymet_variables) {
    download_daymet_ncss(location = c(.max_lat, .min_lon, .min_lat, .max_lon), # Bounding box defined as top left / bottom right pair c(lat, lon, lat, lon)
                         start = .year_start,
                         end = .year_end,
                         param = variable,
                         frequency = "daily",
                         mosaic = .region,
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
  if (length(netcdf_list) > (length(seq(.year_start, .year_end)) * length(.daymet_variables))) {
    stop(call. = FALSE, 'Ensure that there are not extra NetCDF files in folder where Daymet data was downloaded to.')
  }
  # Initializing a time and layer dictionary
  time_dict <- tibble(number = 1:365)
  layer_dict_colnames <- c("dm_var", "yr", "multiplier")
  layer_dict <- as_tibble(matrix(nrow = length(netcdf_list), ncol = length(layer_dict_colnames)), .name_repair = ~ layer_dict_colnames)
  for (i in 1:length(netcdf_list)) {
    daymet_load <- rast(netcdf_list[i])
    # Extracting the year and Daymet variable from the file loaded in
    yr <- str_extract(netcdf_list[i], "[0-9]{4}")
    dm_var <- unlist(str_split(netcdf_list[i], "_"))[1]
    # Creating a dictionary to link numbers 1–365 to a date in a year (time dictionary)
    origin <- as_date(paste0(yr, "-01-01")) - 1 # Numbers count days since origin
    new_date_col <- paste0("date_", yr)
    if (!new_date_col %in% colnames(time_dict)) {
      time_dict <- time_dict %>%
        mutate(!!new_date_col := as_date(number, origin = origin))
    }
    # Creating a dictionary to assign a multiplier for identifying different layers in the raster stack (layer dictionary)
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
  # Returning a list of objects needed later
  out <- list("template" = template, "time_dict" = time_dict, "layer_dict" = layer_dict, "daymet_data" = daymet_data)
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
    stop(call. = FALSE, paste('Could not complete Daymet data merge, ID variable must be unique.',
                              id_var, 'is not unique.'))
  }, warning = function(w) {
    print(w)
  })
  return(to_append)
}

# Importing and processing the input data
import_data_out <- import_data()
id <- import_data_out$id
id_column_df <- import_data_out$id_column_df
extra_columns_df <- import_data_out$extra_columns_df
addresses <- import_data_out$addresses
event_dates <- import_data_out$event_dates
year_start_date <- import_data_out$year_start_date
year_end_date <- import_data_out$year_end_date
coords <- import_data_out$coords

# Downloading and loading the Daymet NetCDF data
daymet_download_load_out <- daymet_download_load()
template <- daymet_download_load_out$template
time_dict <- daymet_download_load_out$time_dict
layer_dict <- daymet_download_load_out$layer_dict
daymet_data <- daymet_download_load_out$daymet_data

# Changing the coordinate reference system of the input addresses so they match that of Daymet
new_crs <- crs(daymet_data, proj = TRUE)
proj_coords <- project(coords, new_crs)

# Filtering out any rows in event_dates where the event dates are all missing
event_dates <- event_dates %>%
  filter(if_any(-!!id, ~ !is.na(.)))

# Checking for any columns in event_dates that don't seem to be dates
if (any(c("numeric", "integer", "logical", "complex") %in% unlist(sapply(event_dates, class)))) {
  stop(call. = FALSE, paste('Ensure that input date is formatted as YYYY-MM-DD.',
                            'And/or, be sure to specify a user-supplied ID column, and/or extra columns.'))
}

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
  stop(call. = FALSE, 'ID variable is entirely missing.')
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

# Merging in the extra columns
extra_columns_df <- as.data.table(extra_columns_df)
main_dataset <- extra_columns_df[main_dataset, on = c(id = "id")]

# Merging in the user-supplied ID column
id_column_df <- as.data.table(id_column_df)
main_dataset <- id_column_df[main_dataset, on = c(id = "id")]

# Sorting and de-duplicating the final results (duplicates could have resulted from leap years or repeat event dates)
if (!is.na(id_column)) {
  main_dataset <- main_dataset %>%
    select(-!!id)
  get_id <- id_column
} else {
  get_id <- id
}
main_dataset <- main_dataset %>%
  mutate(sort1 = factor(get(get_id), ordered = TRUE, levels = unique(mixedsort(get(get_id)))),
         sort2 = factor(date, ordered = TRUE, levels = unique(mixedsort(date)))) %>%
  arrange(sort1, sort2) %>%
  select(-c(sort1, sort2)) %>%
  distinct()

# Writing the results out as a CSV file
csv_out <- paste0(unlist(str_split(csv_filename, ".csv"))[1], "_daymet", ".csv")
fwrite(main_dataset, csv_out, na = "", row.names = FALSE)

# Deleting the NetCDF files that were downloaded from disk
rm(list = ls(all.names = TRUE))
unlink(list.files(pattern = "_ncss.nc$"), force = TRUE)