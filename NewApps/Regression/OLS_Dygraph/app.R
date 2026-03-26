library(shiny)
library(dygraphs)

ui <- fluidPage(
  dygraphOutput("myDygraph")
)

server <- function(input, output) {
  output$myDygraph <- renderDygraph({
    dygraph(nhtemp, main = "New Haven Temperatures") %>%
      dyRangeSelector()
  })
}

shinyApp(ui, server)
