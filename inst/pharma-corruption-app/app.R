library(dsmodules)
library(hgchmagic)
library(lfltmagic)
library(parmesan)
library(pharma.corruption)
library(shiny)
library(shinypanels)

ui <- panelsPage(
  includeCSS("www/custom.css"),
  panel(title = "Filters",
        id = "pharma-panel",
        can_collapse = TRUE,
        width = 350,
        body =  div(
          uiOutput("controls")
        )
  ),
  panel(title = "Visualization",
        id = "pharma-panel",
        can_collapse = FALSE,
        header_right = div(
          class = "head-viz",
          div(class = "viz-style",
              uiOutput("viz_icons")),
          uiOutput("downloads")
        ),
        body =  div(
          uiOutput("viz_view")
        )
  ),
  panel(title = "Detail",
        id = "pharma-panel",
        can_collapse = TRUE,
        width = 300,
        color = "chardonnay",
        body =  div(
          shinycustomloader::withLoader(
            uiOutput("click_info"),
            type = "html", loader = "loader4"
          )
        ),
        footer =  div(class = "footer-logos",
                      tags$a(
                        href="https://www.datasketch.co", target="blank",
                        img(src= 'img/logos/logo_ds.svg',
                            align = "left", width = 130, height = 70)),
                      img(src= 'img/logos/logo_ins.svg',
                          width = 150, height = 150)
        )
  )
)


server <- function(input, output, session) {

  # Renderizar graficos ------------------------------------------

  actual_but <- reactiveValues(active = NULL)

  observe({
    if (is.null(input$viz_selection)) return()
    viz_rec <- c("map_bubbles", "map", "line", "bar", "treemap", "table")
    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }
  })

  output$viz_icons <- renderUI({
    viz <- c("map_bubbles", "map", "line", "bar", "treemap", "table")
    viz_label <- c("Points", "Map", "Line", "Bar", "Treemap", "Table")

    suppressWarnings(
      shinyinvoer::buttonImageInput("viz_selection",
                                    " ",
                                    images = viz,
                                    tooltips = viz_label,
                                    path = "img/viz_icons/",
                                    active = actual_but$active,
                                    imageStyle = list(shadow = TRUE,
                                                      borderColor = "#ffffff",
                                                      padding = "3px")
      )
    )
  })

  # Options to parmesan -----------------------------------------------------

  title_opts <- reactive({
    unique(data_pharma$Title)
  })
  title_holder <- reactive({
    #HTML("<i class='icon-search'></i>")
    "Search"
  })

  countries_opts <- reactive({
    unique(data_pharma$`Country/Region`) |>
      setdiff("NA")
  })

  range_dates <- reactive({
    c(min(data_pharma$`Published-at`, na.rm = TRUE),
      max(data_pharma$`Published-at`, na.rm = TRUE))
  })
  min_date <- reactive({
    req(range_dates())
    range_dates()[1]
  })
  max_date <- reactive({
    req(range_dates())
    range_dates()[2]
  })

  study_label <- reactive({
    HTML("<div class='control-label'>Corruption Case Detection</div>")
  })

  health_opts <- reactive({
    strsplit(data_pharma$`Health Categories`, split = ",") |>
      unlist() |>
      unique() |>
      setdiff("NA")
  })

  corruption_opts <- reactive({
    strsplit(data_pharma$`Corruption Categories`, split = ",") |>
      unlist() |>
      unique() |>
      setdiff("NA")
  })



  # Renderizar inputs con parmesan ------------------------------------------

  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)

  output_parmesan("controls",
                  input = input, output = output, session = session,
                  env = environment())



  # Data filter -------------------------------------------------------------

  data_down <- reactive({
    ls <- parmesan_input()
    df <- data_filter(data = data_pharma,
                      dic = dic_pharma,
                      var_inputs = ls,
                      .id = "story-id") |>
      dplyr::select(-`Other Articles`, -entityname)
    df
  })



  # data to viz -------------------------------------------------------------

  data_viz <- reactive({
    req(actual_but$active)
    if (actual_but$active == "table") return()
    req(data_down())
    if (nrow(data_down()) == 0) return()
    data_down() |>
      variable_selection(viz = actual_but$active) |>
      var_aggregation(dic_pharma, Total = dplyr::n())
  })


  # viz styles --------------------------------------------------------------

  viz_opts <- reactive({
    req(data_viz())
    req(actual_but$active)

    myFunc <- NULL
    if (actual_but$active %in% c("bar", "treemap")) {
      myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {id:event.point.name, timestamp: new Date().getTime()});}")
    }
    if (actual_but$active %in% c("line")) {
      myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")
    }

    opts <- list(
      data = data_viz(),
      orientation = "hor",
      ver_title = " ",
      hor_title = " ",
      label_wrap_legend = 100,
      label_wrap = 40,
      background_color = "#ffffff",
      axis_line_y_size = 1,
      axis_line_color = "#dbd9d9",
      grid_y_color = "#dbd9d9",
      grid_x_color = "#fafafa",
      cursor = "pointer",
      map_tiles = "OpenStreetMap",
      legend_position = "bottomleft",
      border_weight = 0.3
    )
    if (actual_but$active == "map_bubbles") {
      opts$legend_show <- FALSE
      opts$map_min_size <- 2
      opts$map_max_size <- 3
      opts$na_color <- "transparent"
    }
    if (actual_but$active == "map") {
      opts$na_color <- "transparent"
      opts$palette_colors <- rev(c("#ef4e00", "#f66a02", "#fb8412", "#fd9d29",
                                   "#ffb446", "#ffca6b", "#ffdf98"))
    } else {
      opts$clickFunction <- htmlwidgets::JS(myFunc)
      opts$palette_colors <- "#ef4e00"
      if (actual_but$active == "line") {
        opts$marker_enabled <- FALSE
        opts$palette_colors <- c("#ef4e00", "#ffe700", "#6fcbff", "#62ce00",
                                 "#ffeea8", "#da3592","#0000ff")
      }
    }

    if (actual_but$active == "treemap") {
      opts$dataLabels_align <- "middle"
      opts$dataLabels_inside <- TRUE
      opts$dataLabels_show <- TRUE
      opts$legend_show <- FALSE
    }

    opts
  })


  # Render Viz --------------------------------------------------------------

  viz_down <- reactive({
    req(data_viz())
    viz <- viz_selection(data_viz(), dic_pharma, actual_but$active)
    suppressWarnings(do.call(eval(parse(text=viz)),viz_opts()))
  })

  output$hgch_viz <- highcharter::renderHighchart({
    req(actual_but$active)
    req(data_viz())
    if (actual_but$active %in% c("table", "map", "map_bubbles")) return()
    viz_down()
  })

  output$lflt_viz <- leaflet::renderLeaflet({
    req(actual_but$active)
    req(data_viz())
    if (!actual_but$active %in% c("map", "map_bubbles")) return()
    viz_down()
  })

  output$dt_viz <- DT::renderDataTable({
    req(actual_but$active)
    if (actual_but$active != "table") return()
    req(data_down())
    df <- data_down()
    dtable <- DT::datatable(df,
                            rownames = F,
                            selection = 'none',
                            options = list(
                              scrollX = T,
                              fixedColumns = TRUE,
                              fixedHeader = TRUE,
                              scrollY = "500px"
                            ))

    dtable
  })

  output$viz_view <- renderUI({
    req(actual_but$active)
    if (is.null(data_viz())) return("No information available")

    viz <- actual_but$active
    if (viz %in% c("map", "map_bubbles")) {
      shinycustomloader::withLoader(
        leaflet::leafletOutput("lflt_viz", height = 600),
        type = "html", loader = "loader4"
      )
    } else if (viz == "table") {
      shinycustomloader::withLoader(
        DT::dataTableOutput("dt_viz", height = 600, width = 600 ),
        type = "html", loader = "loader4"
      )
    } else {
      #shinycustomloader::withLoader(
        highcharter::highchartOutput("hgch_viz", height = 600)#,
      #   type = "html", loader = "loader4"
      # )
    }
  })


  # Captura click -----------------------------------------------------------

  click_viz <- reactiveValues(info = NULL)

  observe({
    req(actual_but$active)
    if (actual_but$active == "map_bubbles") {
      click <- input$lflt_viz_marker_click
      if (!is.null(click)) {
        click_viz$info <- list("id_location_lat" = click$lat,
                               "id_location_lon" = click$lng)
      }
    }
    if (actual_but$active == "map") {
      click <- input$lflt_viz_shape_click
      if (!is.null(click)) {
        click_viz$info <- list("id_country_region" = click$id)
      }
    }
    if (actual_but$active == "line") {
      click <- input$hcClicked
      if (!is.null(click)) {
        click_viz$info <- list("id_health_categories" = click$cat,
                               "id_published_at" = click$id)
      }
    }
    if (actual_but$active %in% c("bar", "treemap")) {
      click <- input$hcClicked
      if (!is.null(click)) {
        click_viz$info <- list("id_corruption_categories" = click$id)
      }
    }
  })


  # Click Info --------------------------------------------------------------

  output$click_info <- renderUI({
    tx <- HTML("<div class = 'click'>
               <img src='img/click/click.svg' class = 'click-img'/><br/>
               <b>Click</b> on the visualization to see more information.")
    if (is.null(click_viz$info)) return(tx)
    if (is.null(data_viz())) return("No information available")
    tx <- write_html(data = data_down(),
                     dic = dic_pharma,
                     click = click_viz$info,
                     class_title = "click-title",
                     class_body = "click-text",
                     id = "story-id",
                     "Title",
                     "Country/Region",
                     "Published-at",
                     "Health Categories",
                     "Corruption Categories",
                     "url")
    tx
  })

  # downloads ---------------------------------------------------------------

  output$downloads <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      dsmodules::downloadImageUI("download_viz",
                                 dropdownLabel ="Download",
                                 formats = c("jpeg", "pdf", "png", "html"),
                                 display = "dropdown",
                                 text = "Descargar")
    } else {
      dsmodules::downloadTableUI("dropdown_table",
                                 dropdownLabel = "Download",
                                 formats = c("csv", "xlsx", "json"),
                                 display = "dropdown", text = "Descargar")
    }
  })

  observe({
    dsmodules::downloadTableServer("dropdown_table",
                                   element = data_down(),
                                   formats = c("csv", "xlsx", "json"))
    dsmodules::downloadImageServer("download_viz",
                                   element = viz_down(),
                                   lib = "highcharter",
                                   formats = c("jpeg", "pdf", "png", "html"),
                                   file_prefix = "plot")
  })

}

shinyApp(ui, server)



