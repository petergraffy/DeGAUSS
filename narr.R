#!/usr/local/bin/Rscript

dht::greeting(geomarker_name = 'narr', version = '0.2', description = 'add NARR weather variables to geocoded data')

dht::qlibrary(dplyr)
dht::qlibrary(tidyr)
dht::qlibrary(sf)
dht::qlibrary(data.table)
dht::qlibrary(addNarrData)

doc <- '
      Usage:
      narr.R <filename>
      narr.R <filename> --all
      narr.R (-h | --help)

      Options:
      -h --help   Show this screen
      --all   Returns all narr variables
      '

opt <- docopt::docopt(doc)
## for interactive testing
## opt <- docopt::docopt(doc, args = 'test/my_address_file_geocoded.csv')
## opt <- docopt::docopt(doc, args = 'my_address_file_geocoded.csv')
####### to flag for all narr variables
## opt <- docopt::docopt(doc, args = 'test/my_address_file_geocoded.csv --all')
## opt <- docopt::docopt(doc, args = 'my_address_file_geocoded.csv --all')

d <- dht::read_lat_lon_csv(opt$filename)

dht::check_for_column(d, 'lat', d$lat)
dht::check_for_column(d, 'lon', d$lon)
dht::check_for_column(d, 'start_date', d$start_date)
dht::check_for_column(d, 'end_date', d$end_date)

d$start_date <- dht::check_dates(d$start_date)
d$end_date <- dht::check_dates(d$end_date)

message('appending NARR variables based on grid cell and date range...')
if (!opt$all) {
  d_out <- addNarrData::get_narr_data(d, narr_variables = c("air.2m", "rhum.2m"), confirm = F)
} else {
  cli::cli_alert_warning("Due to their size, the narr data files will be downloaded and processed in 4 groups of 2,
                         which will be reflected in the progress messages below.")
  d_out1 <- addNarrData::get_narr_data(d, narr_variables = c("hpbl", "vis"), confirm = F)
  d_out2 <- addNarrData::get_narr_data(d, narr_variables = c("uwnd.10m", "vwnd.10m"), confirm = F)
  d_out3 <- addNarrData::get_narr_data(d, narr_variables = c("air.2m", "rhum.2m"), confirm = F)
  d_out4 <- addNarrData::get_narr_data(d, narr_variables = c("prate", "pres.sfc"), confirm = F)

  d_out <- bind_rows(d_out1, d_out2, d_out3, d_out4) %>%
    select(-.row)
}

## merge back on .row after unnesting .rows into .row
dht::write_geomarker_file(d = d_out,
                     filename = opt$filename,
                     geomarker_name = 'narr',
                     version = '0.2')


