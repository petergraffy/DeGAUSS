# Deleting the NetCDF files that were downloaded from disk
unlink(list.files(pattern = "_ncss.nc$"), force = TRUE)