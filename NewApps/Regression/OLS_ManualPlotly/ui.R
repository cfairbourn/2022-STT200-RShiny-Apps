library(shiny)
library(ggplot2)
library(plotly)

fluidPage(
  titlePanel("Custom Ordinary Least Squares"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("random", "Randomize", value = FALSE),
      conditionalPanel(
        condition = "!input['random']",
        numericInput("seed", "Seed", min = 0, max = 100000, value = 2024)
      ),
      actionButton("generate", "Generate New"),
      sliderInput("dots", "Number of Dots", min = 2, max = 10, step = 1, value = 4),
      checkboxInput("advanced", "Advanced Options"),
      conditionalPanel(
        condition = "input['advanced']",
        sliderInput("b0_true", "True b0", min = -7.5, max = 7.5, step = 0.1, value = 0),
        sliderInput("b1_true", "True b1", min = -2.5, max = 2.5, step = 0.1, value = 1),
        sliderInput("error_sd", "Error SD", min = 0, max = 2, step = 0.1, value = 1)
      )
    ),
    mainPanel(
      plotlyOutput("out_plot"),
      verbatimTextOutput("rmse"),
      verbatimTextOutput("true"),
      verbatimTextOutput("fitted")
    )
  )
)
