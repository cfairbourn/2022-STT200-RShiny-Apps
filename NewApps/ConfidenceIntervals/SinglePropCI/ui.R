# ------------------------------------------------------------------------------
# File: SinglePropCI-app.R
# Authors: Camille Fairbourn, Colin Fairbourn, Scott Manski
# Date: 08/22/2022 
# Desc: This app calculates a bootstrap confidence interval 
#       for a single proportion.
# Published Location: 
# Email: fairbour@msu.edu, fairbou2@msu.edu, manskisc@msu.edu
#
# For questions or concerns, please email the authors. This work is licensed 
# under a Creative Commons Attribution-ShareAlike 4.0 International License 
# (https://creativecommons.org/licenses/by-sa/4.0/).
# ------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(latex2exp)

source("www/SinglePropSource.R")

# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Bootstrap Confidence Interval for a Single Proportion"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Controls",
          # Input
          selectInput(
            "plot_type", "Plot Type", choices = c("Dotplot", "Histogram")),
          selectInput(
            "preset", "Presets", choices = names(Presets),selected = "Custom"),
          selectInput(
            "input_type","Input Type", choices = c("Proportion", "Counts"),
            selected = "Counts"),
          # proportion
          conditionalPanel(
            condition = "input['input_type'] == 'Proportion'",
            numericInput(
              "p_success","Proportion of Success in Original Sample", 
              value = 0.5, max = 1, min = 0, step = 0.01)
          ),
          # counts
          conditionalPanel(
            condition = "input['input_type'] == 'Counts'",
            numericInput(
              "c_success","Number of Successes in Original Sample",
              value = 50, min = 0)
          ),
          # sample size
          numericInput("sample_size", "Sample Size", value = 100,
                       min = 0, max = 2000),
          # number of samples upon pressing generate
          numericInput("n_sims", "Number of Simulations", value = 1000),
          # display number of simulations
          textOutput("length"),
          # buttons to create and erase samples
          actionButton("generate", "Generate"),
          actionButton("reset","Reset"),
          # advanced controls
          checkboxInput("color_switch","Switch Colors",
                        value = T), # Default switch here <-------------------
          checkboxInput("outer_percentages","Show Outer Percentages",
                        value = T) # Default switch here <--------------------
          
          
        ),
        tabPanel(
          "Instructions",
          tags$div(
            class = "header", checked = NA,
            
            hr(),
            tags$p("Choose either a Dotplot or a Histogram for 
                   your graph type."), # line of text
            tags$p("Choose to enter your sample proportion as 
                   either counts (x out of n) or as a sample proportion, 
                   and then enter those values where specified."), 
            tags$p("Select your confidence level using the slider, and then
                   press Generate Bootstrap Proportions to create the
                   interval and its visualization."), 
            hr(), # this is a line break
            tags$p("This app was written by Colin Fairbourn  
                          for the Statistical Methods course 
                          at Michigan State University"),
            tags$p("This work is licensed under a "),
            tags$a(href="http://creativecommons.org/licenses/by-sa/4.0/", 
                   "Creative Commons Attribution-ShareAlike 4.0 
                          International License"),
            hr(),
            tags$p("The previous version of this app is archived at"),
            tags$a(href="https://shiny.stt.msu.edu/fairbour/Archive/TwoProportion/",
                   "Archived Two Proportion Test"),
            
          )
        )
      )
    ),
      mainPanel(
        plotOutput("distribution"),
        fluidRow(
          column(
            6,
            checkboxInput("lines","Show Interval Boundaries",value = T),
            # confidence interval slider
            sliderInput("conf_level", "Confidence Level", ticks = FALSE,
                        min = 0.8, max = .99, step = 0.01, value = 0.95),
            textOutput("bounds"),
            tags$head(
              tags$style(
                "#bounds{font-size: 20px;
                   font-style: bold;
                   }"
              ))
          ),
          column(
            6,
            conditionalPanel(
              condition = "output.show_samples",
              checkboxInput("sample_position", "Show Sample Position"),
              sliderInput("select_sample","Select Sample",
                          min = 1, max = 1, step = 1, value = 1)
            )
          )
        ),
        fluidRow(
          column(2),
          column(
            10,
            withMathJax(),
            uiOutput("sample_info")
          )
        ),
        # tableOutput("debug")
      )
    )
  )