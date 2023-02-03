webshot::install_phantomjs(force = FALSE)
library(dsmodules)
library(hgchmagic)
library(lfltmagic)
library(parmesan)
library(pharma.corruption)
library(shiny)
library(shinypanels)

ui <- panelsPage(
  includeCSS("www/custom.css"),
  includeScript("www/handlers.js"),
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
        id = "pharma-detail",
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
    unique(data_pharma$`English title`)
  })
  title_holder <- reactive({
    #HTML("<i class='icon-search'></i>")
    "Search Titles"
  })

  data_titles <- reactive({

    if (is.null(input$id_english_title)) return()
    df <- NULL
    if (input$id_english_title == "") {
      df <- data_pharma
    } else {
      df <- data_pharma |> dplyr::filter(`English title` %in% input$id_english_title)
    }

    df

  })

  countries_opts <- reactive({
    req(actual_but$active)
    req(data_titles())
    df <- data_titles()
    cp <- unique(c("All", df$`Country / region`)) |>
      setdiff("NA")
    if (actual_but$active %in% c("map_bubbles", "map")) {
      cp <- setdiff(cp, "No Location")
    }
    cp
  })



  range_dates <- reactive({
    req(data_titles())
    df <- data_titles()
    if (!is.null(input$id_country___region)) {
      if (!any(input$id_country___region %in% c("All", ""))) {
        df <- df |> dplyr::filter(`Country / region` %in% input$id_country___region)
      }
    }
    c(min(df$`Published-at`, na.rm = TRUE),
      max(df$`Published-at`, na.rm = TRUE))
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
    req(data_titles())
    df <- data_titles()
    if (!is.null(input$id_country___region)) {
      if (!any(input$id_country___region %in% c("All", ""))) {
        df <- df |> dplyr::filter(`Country / region` %in% input$id_country___region)
      }
    }
    strsplit(c("All", df$`Health categories`), split = ",") |>
      unlist() |>
      unique() |>
      setdiff("NA")
  })

  corruption_opts <- reactive({
    req(data_titles())
    df <- data_titles()
    if (!is.null(input$id_country___region)) {
      if (!any(input$id_country___region %in% c("All", ""))) {
        df <- df |> dplyr::filter(`Country / region` %in% input$id_country___region)
      }
    }
    strsplit(c("All", df$`Corruption categories`), split = ",") |>
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



  # updates all -------------------------------------------------------------

  observe({
    if ("All" %in% input$id_country___region) {
      updateSelectizeInput(session, inputId = "id_country___region", selected = "All")
    }
    if ("All" %in% input$id_health_categories) {
      updateSelectizeInput(session, inputId = "id_health_categories", selected = "All")
    }
    if ("All" %in% input$id_corruption_categories) {
      updateSelectizeInput(session, inputId = "id_corruption_categories", selected = "All")
    }
  })



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

    dv <- data_down() |>
      variable_selection(viz = actual_but$active)
    if (actual_but$active %in% c("bar", "treemap")) {
      id_r <- input$id_country___region
      if (is.null(id_r)) id_r <- "All"
      if (any(grepl("All", id_r))) {
        dv <- dv |> dplyr::select(-`Country / region`)
      }
    }

    dv |> var_aggregation(dic_pharma, Total = dplyr::n())

  })


  # viz styles --------------------------------------------------------------

  viz_opts <- reactive({
    req(data_viz())
    req(actual_but$active)

    myFunc <- NULL
    dv <- dplyr::as_tibble(data_viz())

    verLabel <- names(dv)[2]
    horLabel <- names(dv)[1]

    if (actual_but$active %in% c("line", "bar")) {
      myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")
    }
    if (actual_but$active %in% c("treemap")) {
      myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:event.point.node, id:event.point.name, timestamp: new Date().getTime()});}")
    }
    if (actual_but$active %in% c("bar", "treemap")) {
      dv[[1]][dv[[1]] == "NA"] <- NA
      if (ncol(data_viz()) == 2) {
        verLabel <- names(dv)[1]
        horLabel <- names(dv)[2]
        myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {id:event.point.name, timestamp: new Date().getTime()});}")
      }
      if (ncol(dv) == 3) {
        verLabel <- names(dv)[2]
        horLabel <- names(dv)[3]
        dv[[2]][dv[[2]] == "NA"] <- NA
      }
    }

    if (actual_but$active %in% c("line")) {
      verLabel <- names(dv)[2]
      horLabel <- names(dv)[3]
    }

    if (actual_but$active %in% c("map_bubbles")) {
      if (all(is.na(dv[[1]]))) dv <- NULL
    }
    if (actual_but$active %in% c("map")) {
      if (unique(dv[[1]])[1] == "No Location") dv <- NULL
    }

    #print(unique(dv$`Country / region`))

    opts <- list(
      data = dv,
      orientation = "hor",
      ver_title = verLabel,
      hor_title = horLabel,
      label_wrap_legend = 100,
      label_wrap = 40,
      background_color = "#ffffff",
      axis_line_y_size = 1,
      axis_line_color = "#dbd9d9",
      grid_y_color = "#dbd9d9",
      grid_x_color = "#fafafa",
      cursor = "pointer",
      map_zoom_snap = 0.25,
      map_zoom_delta = 0.25,
      legend_position = "bottomleft",
      border_weight = 0.3,
      format_sample_num = "1,234.",
      drop_na = TRUE,
      map_provider_tile = "url",
      map_extra_layout = "https://maps.geoapify.com/v1/tile/osm-bright-smooth/{z}/{x}/{y}.png?apiKey=3ccf9d5f19894b32b502485362c99163",
      map_name_layout = "osm-brigh"

    )

    if (actual_but$active == "map") {
      opts$na_color <- "transparent"
      opts$palette_colors <- c("#FFF6FF", "#da3592")
    } else {
      opts$clickFunction <- htmlwidgets::JS(myFunc)
      opts$palette_colors <- c("#ef4e00", "#ffe700", "#6fcbff", "#62ce00",
                                        "#ffeea8", "#da3592","#0000ff")
                                        if (actual_but$active == "line") {
                                          opts$marker_enabled <- FALSE
                                          # opts$palette_colors <- c("#ef4e00", "#ffe700", "#6fcbff", "#62ce00",
                                          #                                   "#ffeea8", "#da3592","#0000ff")
                                        }
    }

    if (actual_but$active == "map_bubbles") {
      opts$legend_show <- FALSE
      opts$map_min_size <- 3
      opts$map_max_size <- 5
      opts$na_color <- "transparent"
      opts$tooltip <- "<b>Total: </b>{Total}"
      opts$palette_colors <- "#ef4e00"
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
    viz_down() |>
      leaflet::setView(lng = 0, lat = -5, 1.3)
  })

  output$dt_viz <- DT::renderDataTable({
    req(actual_but$active)
    if (actual_but$active != "table") return()
    req(data_down())
    df <- data_down()
    df <- dplyr::as_tibble(data_down())
    df$URL <- paste0("<a href='",df$URL,"'  target='_blank'>","link to view","</a>")
    df$`Links to similar articles` <- paste0("<a href='", df$`Links to similar articles`, "'  target='_blank'>","link to view","</a>")
    dtable <- DT::datatable(df,
                            rownames = F,
                            selection = 'none',
                            escape = FALSE,
                            options = list(
                              scrollX = T,
                              fixedColumns = TRUE,
                              fixedHeader = TRUE,
                              autoWidth = TRUE,
                              scrollY = "500px",
                              columnDefs = list(#list(width = '200px', targets = c("URL")),
                                #list(width = '500px', targets = c("Links to similar articles")),
                                list(width = '700px', targets = c("Titles of similar articles")))
                            ))
    dtable
  })

  output$viz_view <- renderUI({
    req(actual_but$active)

    heigh_viz <- 600
    width_viz <- 500
    if (!is.null(input$dimension)) {
      heigh_viz <- input$dimension[2] - 150
      width_viz <- input$dimension[1] - 600
    }

    if (actual_but$active != "table") {
      if (is.null(data_viz())) return(
        HTML("Unfortunately, there are no search results for your requested filters.<br/>
             Please try again with different filters"))
    }

    viz <- actual_but$active
    if (viz %in% c("map", "map_bubbles")) {
      shinycustomloader::withLoader(
        leaflet::leafletOutput("lflt_viz", height = heigh_viz),
        type = "html", loader = "loader4"
      )
    } else if (viz == "table") {
      #shinycustomloader::withLoader(
      DT::dataTableOutput("dt_viz", height = heigh_viz, width = width_viz)#,
      # type = "html", loader = "loader4"
      #)
    } else {
      #shinycustomloader::withLoader(
      highcharter::highchartOutput("hgch_viz", height = heigh_viz)#,
      #   type = "html", loader = "loader4"
      # )
    }
  })


  # Captura click -----------------------------------------------------------

  click_viz <- reactiveValues(info = NULL)

  observeEvent(input$lflt_viz_marker_click, {
    if (is.null(data_viz())) return()
    if (!"location.lat" %in% names(data_viz())) return()
    req(actual_but$active)
    if (actual_but$active != "map_bubbles") return()
    click <- input$lflt_viz_marker_click
    if (!is.null(click)) {
      click_viz$info <- list("id_location_lat" = click$lat,
                             "id_location_lon" = click$lng)
    }

  })

  observeEvent(input$lflt_viz_shape_click, {
    if (is.null(data_viz())) return()
    if (!"Country / region" %in% names(data_viz())) return()
    req(actual_but$active)
    if (actual_but$active != "map") return()
    click <- input$lflt_viz_shape_click
    if (!is.null(click)) {
      click_viz$info <- list("id_country___region" = click$id)
    }

  })

  observeEvent(input$hcClicked, {
    if (is.null(data_viz())) return()

    if (actual_but$active == "line") {
      if (!"Published-at" %in% names(data_viz())) return()
      click <- input$hcClicked
      if (!is.null(click)) {
        click_viz$info <- list("id_health_categories" = click$cat,
                               "id_published_at" = click$id)
      }
    }
    if (actual_but$active %in% c("bar", "treemap")) {
      if (!"Corruption categories" %in% names(data_viz())) return()
      click <- input$hcClicked

      if (!is.null(click)) {
        click_viz$info <- list("id_corruption_categories" = click$id)
        if ("Country / region" %in% names(data_viz())) {
          click_viz$info <- list("id_corruption_categories" = click$id,
                                 "id_country___region" = click$cat)
        }
      }


    }
  })


  # Click Info --------------------------------------------------------------

  output$click_info <- renderUI({
    tx <- HTML("<div class = 'click'>
               <img src='img/click/click.svg' class = 'click-img'/><br/>
               <b>Click</b> on the visualization to see more information.")
    if (is.null(click_viz$info)) return(tx)
    if (is.null(data_viz()))  return(HTML("Unfortunately, there are no search results for your requested filters.<br/>
             Please try again with different filters"))

    if (nrow(data_viz()) == 0) return( HTML("Unfortunately, there are no search results for your requested filters.<br/>
             Please try again with different filters"))

    tx <- write_html(data = data_down(),
                     dic = dic_pharma,
                     click = click_viz$info,
                     class_title = "click-title",
                     class_body = "click-text",
                     id = "story-id",
                     "English title",
                     "Country / region",
                     "Published-at",
                     "Health categories",
                     "Corruption categories",
                     "URL")
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
                                 text = "Download")
    } else {
      dsmodules::downloadTableUI("dropdown_table",
                                 dropdownLabel = "Download",
                                 formats = c("csv", "xlsx", "json"),
                                 display = "dropdown", text = "Download")
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



