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
                               year_start = 2016,
                               year_end = 2022,
                               daymet_variables = "tmax,tmin",
                               lag = 7,
                               min_lon = -88.263390,
                               max_lon = -87.525706,
                               min_lat = 41.470117,
                               max_lat = 42.154247,
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
rm(set_options_out)

# Daymet weather variables include daily minimum and maximum temperature, precipitation,
# vapor pressure, shortwave radiation, snow water equivalent, and day length produced on
# a 1 km x 1 km gridded surface over continental North America and Hawaii from 1980 and
# over Puerto Rico from 1950 through the end of the most recent full calendar year.

# Daymet data documentation: https://daac.ornl.gov/DAYMET/guides/Daymet_Daily_V4.html

# Note: The Daymet calendar is based on a standard calendar year. All Daymet years, including
# leap years, have 1–365 days. For leap years, the Daymet data include leap day (February 29)
# and December 31 is discarded from leap years to maintain a 365-day year.

#### IF YOU HAVEN'T PREVIOUSLY INSTALLED ANY OF THE PACKAGES BELOW, THEN UN-COMMENT AND RUN THE RELEVANT CODE LINES ####
# install.packages('daymetr')
# install.packages('tidyverse')
# install.packages('terra')
# install.packages('gtools')
# install.packages('data.table')
# install.packages('future')
# install.packages('furrr')
# install.packages('progressr')
# install.packages('remotes')
# remotes::install_github('degauss-org/dht')

# Loading necessary packages
withr::with_message_sink("/dev/null", library(daymetr))
withr::with_message_sink("/dev/null", library(tidyverse))
withr::with_message_sink("/dev/null", library(terra))
withr::with_message_sink("/dev/null", library(gtools))
withr::with_message_sink("/dev/null", library(data.table))
withr::with_message_sink("/dev/null", library(future))
withr::with_message_sink("/dev/null", library(furrr))
withr::with_message_sink("/dev/null", library(progressr))
withr::with_message_sink("/dev/null", library(dht))

# library(daymetr)
# library(tidyverse)
# library(terra)
# library(gtools)
# library(data.table)
# library(future)
# library(furrr)
# library(progressr)
# library(dht)

# Greeting users
greeting()

# Writing functions
# Creating function to import and process the input addresses and optionally event dates, a user-supplied ID column, and/or extra columns
import_data <- function(.csv_filename = csv_filename, .min_lon = min_lon, .max_lon = max_lon, .min_lat = min_lat, .max_lat = max_lat, .year_start = year_start, .year_end = year_end, .lag = lag, .id_column = id_column, .extra_columns = extra_columns) {
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
  # Throwing an error if no observations are remaining
  if (nrow(input_data) == 0) {
    stop(call. = FALSE, 'Zero observations where lat and lon are not missing.')
  }
  # Verifying that if the user supplied bounding box coordinates, they consist of numeric coordinates
  if (!(.min_lon == 0 & .max_lon == 0 & .min_lat == 0 & .max_lat == 0)) {
    if (!is.numeric(.min_lon) | !is.numeric(.max_lon) | !is.numeric(.min_lat) | !is.numeric(.max_lat)) {
      stop(call. = FALSE, 'Please ensure that user-supplied Daymet bounding box has coordinates in numeric decimal degrees.')
    }
  }
  # Verifying that if the user supplied bounding box coordinates, the minimum coordinates are less than the maximum coordinates
  if (!(.min_lon == 0 & .max_lon == 0 & .min_lat == 0 & .max_lat == 0)) {
    if (!(.min_lon < .max_lon)) {
      stop(call. = FALSE, paste0('Please ensure that bounding box min_lon: ', .min_lon,
                                 ' is less than bounding box max_lon: ', .max_lon, '.'))
    }
    if (!(.min_lat < .max_lat)) {
      stop(call. = FALSE, paste0('Please ensure that bounding box min_lat: ', .min_lat,
                                 ' is less than bounding box max_lat: ', .max_lat, '.'))
    }
  }
  # If the user supplied bounding box coordinates, then removing observations where the address is outside of the bounding box of Daymet data
  if (!(.min_lon == 0 & .max_lon == 0 & .min_lat == 0 & .max_lat == 0)) {
    input_data <- input_data %>%
      filter(lat >= .min_lat & lat <= .max_lat & lon >= .min_lon & lon <= .max_lon)
  }
  # Throwing an error if no observations are remaining
  if (nrow(input_data) == 0) {
    stop(call. = FALSE, 'Zero observations where the lat and lon coordinates are within the user-supplied bounding box of Daymet data.')
  }
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
  # Filtering out any rows in event_dates where the event dates are all missing
  event_dates <- event_dates %>%
    filter(if_any(-!!id, ~ !is.na(.)))
  # Throwing an error if no observations are remaining
  if (nrow(event_dates) == 0) {
    stop(call. = FALSE, 'Zero observations where the user-supplied event dates are not entirely missing.')
  }
  # Removing any columns in event_dates where everything is NA
  event_dates <- event_dates %>%
    select_if(~ !all(is.na(.)))
  # If any columns that are not the ID variable are characters, then converting them to dates
  tryCatch({
    event_dates <- event_dates %>%
      mutate(across(where(is.character) & !as.name(id) & where(~any(str_detect(., "/") & !is.na(.))), mdy))
    event_dates <- event_dates %>%
      mutate(across(where(is.character) & !as.name(id) & where(~any(str_detect(., "-") & !is.na(.))), ymd))
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
  # Checking for any columns in event_dates that don't seem to be dates
  if (any(c("numeric", "integer", "logical", "complex") %in% unlist(sapply(event_dates, class)))) {
    stop(call. = FALSE, paste('Please ensure that provided event dates are formatted as YYYY-MM-DD or MM/DD/YYYY.',
                              'And/or, be sure to specify a user-supplied ID column, and/or extra columns, if applicable.'))
  }
  # Filtering out any rows in event_dates where the event dates are all before the date range of Daymet data to be loaded
  event_dates <- event_dates %>%
    filter(if_any(-!!id, ~ !(. < year_start_date)))
  # Throwing an error if no observations are remaining
  if (nrow(event_dates) == 0) {
    stop(call. = FALSE, 'Zero observations where the event dates are within or after the start year of Daymet data to be loaded.')
  }
  # Filtering out any rows in event_dates where all of the lagged event dates are after the date range of Daymet data to be loaded
  tryCatch({
    if (!(.lag %% 1 == 0) | is.logical(.lag)) {
      stop(call. = FALSE)
    }
  }, error = function(e) {
    stop(call. = FALSE, 'Ensure that user-supplied lag is a numeric whole number.')
  }, warning = function(w) {
    print(w)
  })
  event_dates <- event_dates %>%
    filter(if_any(-!!id, ~ !(as_date(.) - .lag > year_end_date)))
  # Throwing an error if no observations are remaining
  if (nrow(event_dates) == 0) {
    stop(call. = FALSE, 'Zero observations where the lagged event dates are within or before the end year of Daymet data to be loaded.')
  }
  # Removing observations from id_column_df, extra_columns_df, and addresses that were also removed from event_dates
  id_column_df <- id_column_df %>%
    filter(id %in% event_dates$id)
  extra_columns_df <- extra_columns_df %>%
    filter(id %in% event_dates$id)
  addresses <- addresses %>%
    filter(id %in% event_dates$id)
  # Converting the input addresses to a SpatVector
  coords <- vect(addresses, geom = c("lon", "lat"), crs = "+proj=longlat +ellips=WGS84")
  # Returning a list of objects needed later
  out <- list("id" = id, "id_column_df" = id_column_df, "extra_columns_df" = extra_columns_df, "addresses" = addresses, "event_dates" = event_dates, "coords" = coords)
  return(out)
}

# Creating function to download and load the Daymet NetCDF data
daymet_download_load <- function(.min_lon = min_lon, .max_lon = max_lon, .min_lat = min_lat, .max_lat = max_lat, .daymet_variables = daymet_variables, .year_start = year_start, .year_end = year_end, .region = region) {
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
  # Downloading the Daymet NetCDF data defined by the coordinate bounding box: One file per variable per year
  .daymet_variables <- str_remove(.daymet_variables, " ")
  .daymet_variables <- str_split(.daymet_variables, ",", simplify = TRUE)
  for (variable in .daymet_variables) {
    for (year in .year_start:.year_end) {
      download_daymet_ncss(location = c(.max_lat, .min_lon, .min_lat, .max_lon), # Bounding box defined as top left / bottom right pair c(lat, lon, lat, lon)
                           start = year,
                           end = year,
                           param = variable,
                           frequency = "daily",
                           mosaic = .region,
                           silent = FALSE,
                           force = TRUE,
                           path = getwd())
      Sys.sleep(30) # Pausing download of Daymet data for 30 seconds to help avoid placing too many requests at once
    }
  }
  # Loading the NetCDF files downloaded from Daymet as a SpatRaster raster stack
  netcdf_list <- list.files(pattern = "_ncss.nc$")
  # Checking for extra NetCDF files
  if (length(netcdf_list) > (length(seq(.year_start, .year_end)) * length(.daymet_variables))) {
    stop(call. = FALSE, 'Ensure that there are not extra NetCDF files in folder where Daymet data was downloaded to.')
  }
  # Initializing a time dictionary
  time_dict <- tibble(number = 1:365)
  for (i in 1:length(netcdf_list)) {
    # Extracting the year and Daymet variable from the file to be loaded in
    yr <- str_extract(netcdf_list[i], "[0-9]{4}")
    dm_var <- unlist(str_split(netcdf_list[i], "_"))[1]
    # Creating a vector of layer names
    layer_names <- as.character(1:365)
    layer_names <- paste0(dm_var, "_", layer_names, "_", yr)
    # Loading the Daymet data
    daymet_load <- rast(netcdf_list[i])
    names(daymet_load) <- layer_names
    # Creating a dictionary to link numbers 1–365 to a date in a year (time dictionary)
    origin <- as_date(paste0(yr, "-01-01")) - 1 # Numbers count days since origin
    time_dict <- time_dict %>%
      mutate(year = yr,
             date := as_date(number, origin = origin))
    # Stacking the Daymet data rasters and time dictionary
    if (i == 1) {
      daymet_data <- daymet_load
      time_dictionary <- time_dict
    } else {
      daymet_data <- c(daymet_data, daymet_load)
      time_dictionary <- rbind(time_dictionary, time_dict)
    }
  }
  time_dictionary <- time_dictionary %>%
    arrange(number, year) %>%
    distinct()
  time_dictionary <- as.data.table(time_dictionary)
  # Returning a list of objects needed later
  out <- list("time_dictionary" = time_dictionary, "daymet_data" = daymet_data)
  return(out)
}

# Importing and processing the input data
import_data_out <- import_data()
id <- import_data_out$id
id_column_df <- import_data_out$id_column_df
extra_columns_df <- import_data_out$extra_columns_df
addresses <- import_data_out$addresses
event_dates <- import_data_out$event_dates
coords <- import_data_out$coords
rm(import_data_out)

# Downloading and loading the Daymet NetCDF data
daymet_download_load_out <- daymet_download_load()
time_dictionary <- daymet_download_load_out$time_dictionary
daymet_data <- daymet_download_load_out$daymet_data
rm(daymet_download_load_out)

# Setting up parallel processing and progress bar reporting
plan(multisession)
set.seed(1)
handlers("cli")

# Changing the coordinate reference system of the input addresses so they match that of Daymet
new_crs <- crs(daymet_data, proj = TRUE)
proj_coords <- project(coords, new_crs)
rm(coords)

# Finding the Daymet raster cell numbers that match the input address coordinates
addresses <- addresses %>%
  mutate(cell = unname(cells(daymet_data, proj_coords)[, "cell"]))
rm(proj_coords)

# Removing any input address observations where the Daymet cell raster number is missing
addresses <- addresses %>%
  filter(!is.na(cell))

# Throwing an error if no observations are remaining
if (nrow(addresses) == 0) {
  stop(call. = FALSE, 'Zero observations where the input address coordinates fell within Daymet raster cells.')
}

# Combining all the event dates into a list
event_dates <- event_dates %>%
  mutate(event_date_list = future_pmap(select(., -!!id), c)) %>%
  select(!!id, event_date_list)
event_dates$event_date_list <- future_map(event_dates$event_date_list, unname)
event_dates$event_date_list <- future_map(event_dates$event_date_list, na.omit)
tryCatch({
  event_dates$event_date_list <- future_map(event_dates$event_date_list, as_date)
}, error = function(e) {
  print(e)
}, warning = function(w) {
  stop(call. = FALSE, 'Ensure that input date is formatted as YYYY-MM-DD or MM/DD/YYYY.')
})

# For each event date list, adding in new dates that correspond to the specified lag
if (lag > 0) {
  with_progress({
    p <- progressor(steps = lag, message = "Lagging Dates")
    for (lag_step in 1:lag) {
      new_dates <- future_map(event_dates$event_date_list, ~ .x - lag_step)
      event_dates$new_dates <- new_dates
      if (lag_step == 1) {
        event_dates$event_date_list_lag <- future_map2(event_dates$event_date_list, event_dates$new_dates, c)
      } else {
        event_dates$event_date_list_lag <- future_map2(event_dates$event_date_list_lag, event_dates$new_dates, c)
      }
      event_dates <- event_dates %>%
        select(-new_dates)
      p()
    }
  })
  event_dates <- event_dates %>%
    select(-event_date_list) %>%
    rename(event_date_list = event_date_list_lag)
}
rm(new_dates)

# Sorting each event date list, and removing duplicates
event_dates$event_date_list <- future_map(event_dates$event_date_list, sort)
event_dates$event_date_list <- future_map(event_dates$event_date_list, unique)

# Expanding the event date list column to be a separate row for each entry
event_dates <- unnest(event_dates, event_date_list)

# Merging event_dates and addresses
event_dates <- event_dates %>%
  rename(date = event_date_list)
addresses <- addresses %>%
  select(-c(lat, lon))
event_dates <- as.data.table(event_dates)
addresses <- as.data.table(addresses)
addresses <- addresses[event_dates, on = c(id = "id")]
rm(event_dates)

# Taking care of leap years, per Daymet conventions (12/31 is switched to 12/30)
addresses$date <- if_else(leap_year(addresses$date) & month(addresses$date) == 12 & day(addresses$date) == 31,
                          addresses$date - 1,
                          addresses$date)

# Converting the Daymet SpatRaster raster stack to a data table, with cell numbers
daymet_data_dt <- as.data.frame(daymet_data, cells = TRUE)
daymet_data_dt <- as.data.table(daymet_data_dt)
rm(daymet_data)

# Transposing the Daymet data table, one Daymet variable at a time
transpose_daymet <- function(.daymet_data_dt = daymet_data_dt, dm_var) {
  daymet_data_dt_var <- .daymet_data_dt %>%
    select(c("cell", starts_with(dm_var)))
  daymet_data_dt_var <- melt(daymet_data_dt_var, id.vars = "cell", variable.name = "number_year", value.name = dm_var)
  daymet_data_dt_var <- daymet_data_dt_var %>%
    mutate(number_year = str_remove_all(number_year, paste0(dm_var, "_")))
  return(daymet_data_dt_var)
}
daymet_variables <- str_remove(daymet_variables, " ")
daymet_variables <- str_split(daymet_variables, ",", simplify = TRUE)
for (i in 1:length(daymet_variables)) {
  if (i == 1) {
    daymet_data_long <- transpose_daymet(dm_var = daymet_variables[i])
  }
  else {
    daymet_data_long <- daymet_data_long[transpose_daymet(dm_var = daymet_variables[i]), on = c(cell = "cell", number_year = "number_year")]
  }
}
rm(daymet_data_dt)

# Rounding the Daymet variables to two decimal places
daymet_data_long <- daymet_data_long %>%
  mutate(across(where(is.numeric) & matches(daymet_variables), ~ round(., 2)))
rm(daymet_variables)

# Splitting out number_year in daymet_data_long
daymet_data_long <- daymet_data_long %>%
  separate_wider_delim(number_year, delim = "_", names = c("number", "year"), cols_remove = TRUE)

# Matching the Daymet day numbers to the dates they correspond to
daymet_data_long <- daymet_data_long %>%
  mutate(number = as.integer(number),
         year = as.integer(year))
daymet_data_long <- as.data.table(daymet_data_long)
time_dictionary <- time_dictionary[, number := as.integer(number)]
time_dictionary <- time_dictionary[, year := as.integer(year)]
daymet_data_long <- daymet_data_long[time_dictionary, on = c(number = "number", year = "year")]
daymet_data_long <- daymet_data_long[, c("number", "year") := NULL]
rm(time_dictionary)

# Linking the Daymet data cells to the input address coordinate cells across all event dates
main_dataset <- daymet_data_long[addresses, on = c(cell = "cell", date = "date")]
main_dataset <- main_dataset[, "cell" := NULL]
setcolorder(main_dataset, c("id", "date"))
rm(daymet_data_long, addresses)

# Merging in the extra columns
extra_columns_df <- as.data.table(extra_columns_df)
main_dataset <- extra_columns_df[main_dataset, on = c(id = "id")]
rm(extra_columns_df)

# Merging in the user-supplied ID column
id_column_df <- as.data.table(id_column_df)
main_dataset <- id_column_df[main_dataset, on = c(id = "id")]
rm(id_column_df)

# Removing any rows with NA
main_dataset <- main_dataset %>%
  na.omit()

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