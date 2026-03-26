# ------------------------------------------------------------------------------
# File: MosaicPlotGenerator/ui.R
# Authors: Camille Fairbourn, Colin Fairbourn
# Date: 02/13/2026
# Desc: This app performs a two proportion test via randomization. The 
#       resampling test mimics Fisher's Exact Test.
# Published Location: 
# Email: fairbour@msu.edu, fairbou2@msu.edu
#
# For questions or concerns, please email the authors. This work is licensed 
# under a Creative Commons Attribution-ShareAlike 4.0 International License 
# (https://creativecommons.org/licenses/by-sa/4.0/).
# ------------------------------------------------------------------------------


library(shiny)
library(rhandsontable)

# Define UI for application that draws a mosaicplot based on inputs
fluidPage(
  
  # Application title
  titlePanel("Mosaic Plot Generator"),
  
  fluidRow(
    column(
      4,
      # numerical inputs for nrows and ncols
      numericInput(
        "nrows",
        "Number of Rows:",
        min = 2,
        max = 20,
        value = 2,
        step = 1
      ),
      numericInput(
        "ncols",
        "Number of Columns:",
        min = 2,
        max = 20,
        value = 2,
        step = 1
      ),
      
      textInput(
        "title",
        "Plot Title:",
        value = "Mosaic Plot"
      ),
      textInput(
        "row_lab",
        "Row Label:",
        value = "Rows"
      ),
      textInput(
        "col_lab",
        "Column Label:",
        value = "Columns"
      ),
      
      checkboxInput(
        "transpose",
        "transpose",
        value = FALSE
      ),
      
    ), 
    # the editable table
    column(
      8, 
      rHandsontableOutput("mosaicTable")
    )
  ),
  
  fluidRow(
    column(3,),
    column(
      6,
      # Show a plot of the generated plot
      plotOutput("mosaicPlot")
      
    ),
    column(3,)
  )
)
