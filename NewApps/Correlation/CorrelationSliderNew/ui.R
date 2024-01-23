# <<<<<<< HEAD
# ------------------------------------------------------------------------------
# File: CorrelationSlider/app.R
# Authors: Camille Fairbourn, Colin Fairbourn
# Date: 07/04/2023 
# Desc: This app creates a scatter plot for a given correlation value.
# Published Location: 
# Email: fairbour@msu.edu
#
# For questions or concerns, please email the authors. This work is licensed 
# under a Creative Commons Attribution-ShareAlike 4.0 International License 
# (https://creativecommons.org/licenses/by-sa/4.0/).
# ------------------------------------------------------------------------------
# =======
# 
# 
# 
# >>>>>>> 42b4f704d5821eead8f916263b15b2aefd8bb23d


#############################
# Correlation Slider App UI #
#############################

library(shiny)
require(MASS)
library(ggplot2)

source("www/CorrelationAnimationSource.R")


fluidPage(
  titlePanel("Correlation"),
  
  sidebarLayout(
    sidebarPanel(
      # Seed
      radioButtons("seed_choice", "Seed Choice",
                   choices = c("Set","Random")),
      conditionalPanel(
        condition = "input['seed_choice'] == 'Set'",
        numericInput("seed","Seed", value = 2004)
      ),
      # Sample Size
      numericInput("sample_size", "Sample size", min = 0, value = 50),
      radioButtons("r_choice","Slider or Text",
                   choices = c("Text","Slider"), inline = TRUE),
      # Slider Functionality
      conditionalPanel(
        condition = "input['r_choice'] == 'Slider'",
        sliderInput("r_value_slider",
                    "Correlation Coefficient",
                    min = -1,max = 1, step = 0.05, value = 0)
      ),
      # Numerical Functionality
      conditionalPanel(
        condition = "input['r_choice'] == 'Text'",
        numericInput("r_value_number",
                     "Correlation Coefficient",
                     min = -1, max = 1, step = 0.05, value = 0)
      )
    ),
    # Output
    mainPanel(
      plotOutput("distPlot")
    )
  )
)