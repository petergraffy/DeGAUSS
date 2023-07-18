
remotes::install_github("degauss-org/dht")

library(dplyr, warn.conflicts = FALSE)
library(dht)


addresses <-
  tibble::tibble(
    id = paste0("g_", 1:10),
    address = c(
      "518 Fortune Ave Cincinnati OH 45219",
      "3201 Stanhope Av Apt. 2 Cincinnati, OH 45211",
      "3917 Catherine Av Norwood OH 45212",
      "9960 Carolina Trace Road Harrison Township OH 45030",
      "332 East Sharon Rd Unit #15 Glendale OH 45246",
      "10101 Hamilton Cleves Road Crosby Township OH 45030",
      "6076 Lagrange Ln Green Township, OH 45239",
      "1325 Fuhrman Rd Reading, OH 45215",
      "8831 Wellerstation Drive Montgomery OH 45249",
      "2916 Willow Ridge Dr Colerain Township OH 45251"
    ))

d <-
  addresses |>
  degauss_run("postal", "0.1.1", quiet = TRUE) |>
  select(id = id, address = parsed_address)
