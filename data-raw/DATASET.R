## code to prepare `DATASET` dataset goes here
library(jsonlite)

# read json as list
# ls <- lapply(readLines("data-raw/data/aimon.json"), fromJSON)

data_pharma <- stream_in(file("data-raw/data/aimon.json"))
data_pharma$`Published-at` <- lubridate::as_date(
  lubridate::ymd_hms(data_pharma$`Published Date`))
usethis::use_data(data_pharma, overwrite = TRUE)

id_names <- tolower(paste0("id_",
                           gsub("\\.", "_",make.names(names(data_pharma)))))
class_var <- purrr::map(data_pharma, class) |> unlist()
dic_pharma <- dplyr::tibble(id = id_names, label = names(data_pharma), ftype = class_var)
usethis::use_data(dic_pharma, overwrite = TRUE)
