#' @export
variable_selection <- function(data, viz = NULL, ...) {
  if (is.null(data)) return()
  if (!is.null(viz)) {
    path <- paste0(system.file(package = "pharma.corruption"), "/conf/viz.yaml")
    conf_viz <- yaml::read_yaml(path)
    selected <- conf_viz[[viz]]
    if (length(selected) == 1) {
      data <- data |> dplyr::select({{ selected }})
    } else {
    data <- data[, selected]
    }
  } else {
    data <- data |> dplyr::select(...)
  }
  data
}
