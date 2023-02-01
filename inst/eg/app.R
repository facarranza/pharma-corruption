library(shiny)
library(pharma.corruption)

ui <- fluidPage(
  uiOutput("html_test")
)

# Server logic
server <- function(input, output) {

  output$html_test <- renderUI({
    write_html(data = data_pharma,
               dic = dic_pharma,
               click = list("id_country_region" = "Spain"),
               class_title = "click-title",
               class_body = "click-text",
               "Title",
               "Country / region",
               "Published-at",
               "Health categories",
               "Corruption categories",
               "url")
  })

}

# Complete app with UI and server components
shinyApp(ui, server)

