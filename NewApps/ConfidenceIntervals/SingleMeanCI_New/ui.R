


library(shiny)
library(ggplot2)
library(dplyr)
library(openintro)
library(rhandsontable)
library(data.table)
library(shinyhelper)

source("www/SingleMeanSource.R")

fluidPage(
  titlePanel("Bootstrap Confidence Interval for a Single Mean"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Controls",
          numericInput("n_sim", "Number of Simulations", value = 500),
          
          actionButton("generate","Generate"),
          actionButton("reset","Reset"),
          
          selectInput("plot_type","Plot Type", selected = "Dotplot",
                      choices = c("Dotplot","Histogram")),
          
          checkboxInput("advanced","Advanced Controls"),
          conditionalPanel(
            "input.advanced & output.show_samples",
            sliderInput("n_cols","Number of Columns per Bin",
                        min = .5, max = 2.5, value = 1, step = 0.1),
            checkboxInput("no_colors","Remove Color from Distribution"),

            tags$div(
              class = "header", checked = NA,

              tags$p("Warning: Performance issues occur with higher values")
            ),
            sliderInput("n_bins_scalar", "Number of Bins (Scalar)",
                        value = 1, # <- Change Default Here *****************
                        min = 1, max = 4, step = 0.1),

          )
          
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data",
        ),
        tabPanel(
          "Visuals",
          plotOutput("distribution"),
          sliderInput("conf_level", 
                      min = 0.8, max = 0.99, value = 0.95, step = 0.01),
        )
      )
    )
  )
)

