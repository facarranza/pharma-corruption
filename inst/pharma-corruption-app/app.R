library(dsmodules)
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
        color = "chardonnay",
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
          verbatimTextOutput("test")
        )
  ),
  panel(title = "Detail",
        id = "pharma-panel",
        can_collapse = TRUE,
        width = 300,
        color = "chardonnay",
        body =  div(
          "panel 3"
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

  data_filter <- reactive({
    ls <- parmesan_input()
    pharma.corruption::data_filter(data = data_pharma,
                                   dic = dic_pharma,
                                   var_inputs = ls,
                                   .id = "story-id")
  })



  output$test <- renderPrint({
    parmesan_input()
    # data_filter()
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



