#!/usr/local/bin/Rscript

dht::greeting()

## load libraries without messages or warnings
withr::with_message_sink("/dev/null", library(dplyr))
withr::with_message_sink("/dev/null", library(tidyr))
withr::with_message_sink("/dev/null", library(sf))
withr::with_message_sink("/dev/null", library(data.table))
withr::with_message_sink("/dev/null", library(addNarrData))

doc <- '
      Usage:
      narr.R <filename> [<vars>]
      narr.R (-h | --help)

      Options:
      -h --help   Show this screen
      filename  name of csv file
      vars   weather, wind, atmosphere, pratepres, or none (see readme for more info)
      '

opt <- docopt::docopt(doc)

## for interactive testing
## opt <- docopt::docopt(doc, args = 'test/my_address_file_geocoded.csv')

message("reading input file...")
d <- dht::read_lat_lon_csv(opt$filename)
dht::check_for_column(d, "lat", d$lat)
dht::check_for_column(d, "lon", d$lon)

if (is.null(opt$vars)) {
  opt$vars <- "weather"
  cli::cli_alert_warning("Blank argument for NARR variable selection. Will return air.2m and rhum.2m. Please see {.url https://degauss.org/narr/} for more information about the NARR variable argument.")
}

if (! opt$vars %in% c("weather", "wind", "atmosphere", "pratepres", "none")) {
  opt$vars <- "weather"
  cli::cli_alert_warning("Invalid argument for NARR variable selection. Will return air.2m and rhum.2m. Please see {.url https://degauss.org/narr/} for more information about the NARR variable argument.")
}

if (opt$vars == "none") {
  d_out <- addNarrData::get_narr_cell_numbers(d)
} else {
  dht::check_for_column(d, 'start_date', d$start_date)
  dht::check_for_column(d, 'end_date', d$end_date)

  d$start_date <- dht::check_dates(d$start_date)
  d$end_date <- dht::check_dates(d$end_date)

  narr_variables = case_when(
    opt$vars == "weather" ~ c("air.2m", "rhum.2m"),
    opt$vars == "wind" ~ c("uwnd.10m", "vwnd.10m"),
    opt$vars == "atmosphere" ~ c("hpbl", "vis"),
    opt$vars == "pratepres" ~ c("prate", "pres.sfc")
  )

  message('appending NARR variables based on grid cell and date range...')
  d_out <- addNarrData::get_narr_data(d, narr_variables = narr_variables, confirm = F)
  d_out <- dplyr::select(d_out, -.row)
}

## merge back on .row after unnesting .rows into .row
dht::write_geomarker_file(d = d_out,
                          filename = opt$filename,
                          argument = opt$vars)


