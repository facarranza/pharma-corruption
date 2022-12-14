#' @export
write_html <- function(data, dic, click, class_title, class_body, ...) {
  if (is.null(data)) return()
  if (is.null(dic)) return()
  if (is.null(click)) return()
  data_click <- data_filter(data = data,
                            dic = dic,
                            var_inputs = click)
  data_click <- data_click |> dplyr::select(...)
  info_click <- names(data_click)

  htmltools::HTML(
    paste0(
      purrr::map(unique(data_click[[info_click[1]]]), function(x) {
        df <- data_click |> dplyr::filter(!!dplyr::sym(info_click[1]) %in% x)
        htmltools::HTML(
          paste0(
            div(class = class_title,
                x),
            div(class = class_body,
                htmltools::HTML(
                  paste0(
                    purrr::map(names(df)[-1], function(v) {
                      name_text <- ""
                      if (v == "url") {
                        name_text <- paste0("<a href=", v," target='_blank'>Link</a>")
                      } else {
                        name_text <- paste0("<p class = 'click-p'>", v, "</p>")
                      }
                      htmltools::HTML(
                        paste0(name_text, ": ", df[[v]], collapse = "<br/>")
                      )
                    })
                  )
                )
            )
          )
        )
      }), collapse = "</br>" )
  )
}
