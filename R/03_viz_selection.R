#' @export
viz_selection <- function(data, dic, viz) {
  if (is.null(data)) return()
  if (nrow(data) == 0) return()
  if (is.null(dic)) return()
  if (is.null(viz)) return()

  dic_viz <- dic |>
    dplyr::filter(label %in% names(data))
  dic_viz$ftype <- dplyr::recode(dic_viz$ftype,
                                 "character" = "Cat",
                                 "list" = "Cat")
  library <- "hgchmagic"
  pseudonym <- "hgch"

  if (viz == "map") {
    dic_viz$ftype <- dplyr::recode(dic_viz$ftype, "Cat" = "Gnm")
    viz <- "choropleth"
    library <- "lfltmagic"
    pseudonym <- "lflt"
  } else if (viz == "map_bubbles") {
    dic_viz$ftype <- c("Gln", "Glt")
    viz <- "bubbles"
    library <- "lfltmagic"
    pseudonym <- "lflt"
  } else if (viz == "line") {
    dic_viz$ftype <- dplyr::recode(dic_viz$ftype, "Date" = "Yea")
    viz <- "line"
  }  else {
    viz <- viz
  }

  viz_type <- paste0(library, "::",
                     pseudonym, "_",
                     viz, "_",
                     paste0(dic_viz$ftype, collapse = ""),
                     "Num")
  viz_type
}
