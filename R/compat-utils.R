filter_dates <- function(data, range_date, by) {
  if (is.null(data)) return()

  min_date <- min(data[[by]], na.rm = TRUE)
  max_date <- max(data[[by]], na.rm = TRUE)

  if (length(range_date) == 2) {
    if (min_date == range_date[1] & max_date == range_date[2]) {
      data_filter <- data
    } else {
      data_filter <- data |>
        dplyr::filter(!!dplyr::sym(by) >= range_date[1] &
                        !!dplyr::sym(by) <= range_date[2])
    }
  } else {
    data_filter <-  data |>
      dplyr::filter(!!dplyr::sym(by) == range_date)
  }
  data_filter

}
