#' @export
var_aggregation <- function(data, dic, ...) {
  if (is.null(data)) return()
  dic_filter <- dic |>
    dplyr::filter(label %in% names(data))

  if ("list" %in% dic_filter$ftype) {
    var <- dic_filter |>
      dplyr::filter(ftype %in% "list") |>
      dplyr::pull(label)
    data <- data |>
      tidyr::separate_rows( {{ var }}, sep = ",")
  }
  data <- data |>
    dplyr::group_by_all() |>
    dplyr::summarise(...)
  data
}
