





library(shiny)
library(ggplot2)
library(tidyverse)



fluidPage(
  titlePanel("Custom Ordinary Least Squares"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput(
        "random","randomize",value = F
      ),
      conditionalPanel(
        condition = "!input['random']",
        numericInput(
          "seed","seed",
          min = 0, max = 100000, value = 2024
        ),
      ),
      actionButton("generate","generate new"),
      sliderInput(
        "dots","number of dots",
        min = 2, max = 10, step = 1, value = 4
      ),
      checkboxInput(
        "advanced","advanced options"
      ),
      conditionalPanel(
        condition = "input['advanced']",
        sliderInput(
          "b0_true","true b0",
          min = -7.5, max = 7.5, step = 0.1, value = 0
        ),
        sliderInput(
          "b1_true","true b1",
          min = -2.5, max = 2.5, step = 0.1, value = 1 
        ),
        sliderInput(
          "error_sd","error sd",
          min = 0, max = 2, step = 0.1, value = 1 
        )
      )
    ),
    mainPanel(
      plotOutput("out_plot"),
      textOutput(
        "rmse"
      ),
      # interactive values
      fluidRow(
        column(
          6,
          numericInput(
            "b0","intercept",
            min = -10, max = 10, step = 0.1, value = 0
          )
        ),
        column(
          6,
          numericInput(
            "b1","slope",
            min = -5, max = 5, step = 0.1, value = 0
            
          )
        )
      ),
      # true values
      fluidRow(
        column(
          6,
          checkboxInput(
            "show_true","show true"
          ),  
        ),
        column(
          6,
          conditionalPanel(
            condition = "input['show_true']",
            textOutput("true")
          )
        )
      ),
      fluidRow(
        column(
          6,
          checkboxInput(
            "show_fitted","show fitted"
          )
        ),
        column(
          6,
          conditionalPanel(
            condition = "input['show_fitted']",
            textOutput("fitted")
          )
        )
      )
    )
  )
)
