#' @export
write_html <- function(data, dic, click, class_title, class_body, id = NULL, ...) {
  if (is.null(data)) return()
  if (is.null(dic)) return()
  if (is.null(click)) return()
  data_click <- data_filter(data = data,
                            dic = dic,
                            var_inputs = click,
                            .id = id)
  data_click <- data_click |> dplyr::select(...)
  info_click <- names(data_click)

  htmltools::HTML(
    paste0(
      purrr::map(unique(data_click[[info_click[1]]]), function(x) {
        df <- data_click |> dplyr::filter(!!dplyr::sym(info_click[1]) %in% x)
        htmltools::HTML(
          paste0(

            div(class = class_title,
                htmltools::HTML(
                  paste0(
                    "<q>", x, "</q>")
                )),
            div(class = class_body,
                htmltools::HTML(
                  paste0(
                    purrr::map(names(df)[-1], function(v) {

                      tx <- paste0("<div class = 'click-p'><div class = 'click-tl'>", v, ":</div> <div class = 'click-subtitle'>",
                                   df[[v]], "</div></div>", collapse = "")
                      if (any( grepl("url|URL", v))) {
                        tx <- paste0("<div style='display:flex;justify-content: space-between;'><div>",
                                     paste0("<a href=", df[[v]]," target='_blank'>Link &#8734;</a>"),
                                     "</div>
                                     <a class='btn report' href='mailto:ti-health@transparency.org?subject=Inquiry about news article'>
                                     Report</a>
                                     </div>")
                      }
                      tx
                    }), collapse = "</br>" )
                )
            )
          )
        )
      }), collapse = "</br>" )
  )
}
