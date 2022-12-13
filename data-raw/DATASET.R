## code to prepare `DATASET` dataset goes here
library(jsonlite)

# read json as list
# ls <- lapply(readLines("data-raw/data/aimon.json"), fromJSON)

data_pharma <- stream_in(file("data-raw/data/aimon.json"))
data_pharma$`Published-at` <- lubridate::as_date(lubridate::ymd_hms(data_pharma$`Published Date`))
usethis::use_data(data_pharma, overwrite = TRUE)

