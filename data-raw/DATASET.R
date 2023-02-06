## code to prepare `DATASET` dataset goes here
library(dplyr)
library(jsonlite)
library(tidyr)
library(purrr)
# read json as list
# ls <- lapply(readLines("data-raw/data/aimon.json"), fromJSON)


# read data ---------------------------------------------------------------
googledrive::drive_deauth()
googledrive::drive_download("https://drive.google.com/file/d/1pGGRJV9RH3yBeG4qXdNBL64HsxELd47B/view",
                                         path = "data-raw/data/aimon.json", overwrite = TRUE)
data_pharma <- stream_in(file("data-raw/data/aimon.json"))
data_pharma$`Published-at` <- lubridate::as_date(
  lubridate::ymd_hms(data_pharma$`Published Date`))
data_pharma$`Corruption Case Study`[data_pharma$`Corruption Case Study`] <- "Yes"
data_pharma$`Corruption Case Study`[is.na(data_pharma$`Corruption Case Study`)] <- "No"
data_pharma <- data_pharma |>
  rename( URL = url,
          `Media type` = `media-type`,
          `Source` = `Source Name`,
          `English title` = `Title`,
          `Publication year` = `Year`,
          `Publication date` = `Published Date`,
          `Country / region` = `Country/Region`,
          `Corruption case study` = `Corruption Case Study`,
          `Associated topic` = `topics`,
          `Links to similar articles` = `Similar Stories Links`,
          `Titles of similar articles`= `Similar Stories Titles`,
          `Corruption categories` = `Corruption Categories`,
          `Health categories` = `Health Categories`)



# create dic --------------------------------------------------------------

id_names <- tolower(paste0("id_",
                           gsub("\\.", "_",make.names(names(data_pharma)))))
class_var <- purrr::map(data_pharma, class) |> unlist()
dic_pharma <- dplyr::tibble(id = id_names,
                            label = names(data_pharma),
                            ftype = class_var)


# list as vectors ---------------------------------------------------------

paste_vector <- function(x, collapse = ",") {
  paste0(trimws(unique(x)), collapse = collapse)
}

var_list <- dic_pharma |>
  filter(ftype == "list") |>
  pull(label)
var_list <- var_list[-grep("Other Articles|entityname", var_list)]


unlist_pharma <- map(var_list, function (by) {
  print(by)
  df <- data_pharma |>
    select(`story-id`, {{ by }}) |>
    unnest({{ by }}, keep_empty = TRUE) |>
    group_by(`story-id`) |>
    dplyr::summarise_each(funs(paste_vector))
  df
}) |>  reduce(left_join, by = "story-id")

data_pharma <- data_pharma[-grep(paste0(var_list, collapse = "|"),
                                 names(data_pharma))]

data_pharma <- data_pharma |> left_join(unlist_pharma) |> dplyr::top_n(2)


# save data and dic -------------------------------------------------------

usethis::use_data(data_pharma, overwrite = TRUE)
usethis::use_data(dic_pharma, overwrite = TRUE)
