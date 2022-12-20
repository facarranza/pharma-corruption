#' @export
data_filter <- function(data, dic, var_inputs, .id) {
  if (is.null(data)) return()
  if (is.null(dic)) return()
  df <- data
  if (is.null(var_inputs)) return()
  if (!is.list(var_inputs)) return()
  tem_ls <-
    seq_along(var_inputs) |>
    purrr::map(function(.x) {
      if (!is.null(var_inputs[[.x]])) {
        if (!setequal(var_inputs[[.x]], "")) {
          if (!setequal(var_inputs[[.x]], "All")) {
            name_var <- names(var_inputs)[.x]
            info_var <- dic |>
              dplyr::filter(id %in% name_var)
            filter_var <- var_inputs[[.x]]
            if (info_var$ftype == "Date") {
              df <<- filter_dates(df, range_date = filter_var, by = info_var$label)
            }
            if (info_var$ftype == "list") {
              df <<- filter_list(df, filter_var, info_var$label, .id = .id)
            }
            if (info_var$ftype == "character") {
              df <<- df |>
                dplyr::filter(!!dplyr::sym(info_var$label) %in% filter_var)
            }
            if (info_var$ftype == "numeric") {
              df <<- df |>
                dplyr::filter(!!dplyr::sym(info_var$label) == filter_var)
            }
          }
        }
      }
    })
  rm(tem_ls)
  df
}

