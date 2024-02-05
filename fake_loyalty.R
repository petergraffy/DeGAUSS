


library(dplyr)
library(lubridate)

set.seed(123) 

# random date generator
random_date <- function(n, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  as.Date(start_date + runif(n) * (end_date - start_date))
}

# random number generator
random_number <- function(n, min_val, max_val) {
  min_val + runif(n) * (max_val - min_val)
}


n_obs <- 500000

# dataset creation
data <- tibble(
  patid = sample(1:450000, n_obs, replace = TRUE),
  enc_admit_date = random_date(n_obs, "2010-01-01", "2022-12-31"),
  lat = random_number(n_obs, 39.669, 43.095),
  lon = random_number(n_obs, -89.5707, -85.9524),
  address_number = NA_real_,
  enc_id = NA_integer_
)


data <- data %>%
  group_by(patid) %>%
  mutate(
    enc_id = row_number(),  # set enc id by patid row number
    address_number = seq_len(n())  # change address every encounter
  ) %>%
  ungroup() %>%
  arrange(patid)


#write.csv(data, "loyalty2.csv")
