# ------------------------------------------------------------------------------
# File: MosaicPlotGenerator/server.R
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
library(tidyverse)

# Define server logic required to draw a histogram
function(input, output, session) {

  alpha <- reactiveValues()
  
  # intialize table and resize when dimensions change
  observeEvent(c(input$nrows, input$ncols), {
    
    table_data <- data.frame(
      Label = c("Label", paste0("Row", seq_len(input$nrows))),
      matrix(
        "",
        nrow = input$nrows + 1,
        ncol = input$ncols),
      check.names = FALSE
    )
    
    # fill column label row
    table_data[1, -1] <- paste0("Col", seq_len(input$ncols))
    
    # fill data rows with zeros
    table_data[-1, -1] <- rnorm(input$nrows * input$ncols, mean = 20, sd = 5)^2 %>%
      round()
    
    alpha$table_data <- table_data
  }, ignoreInit = FALSE)
  
  
  # Render the editable table
  output$mosaicTable <- renderRHandsontable({
    req(alpha$table_data)
    rhandsontable(
      alpha$table_data, 
      rowHeaders = FALSE
    ) %>% 
      hot_cols(type = "text")
  })
  
  
  # Update reactive table data when table is edited
  observeEvent(input$mosaicTable, {
    
    df <- hot_to_r(input$mosaicTable)
    alpha$table_data <- df
  })
  
  
  
  # Generating the plot
  output$mosaicPlot <- renderPlot({
    
    
    df <- alpha$table_data
    req(df)
    
    # extract labels
    col_labels <- as.character(unlist(df[1,-1]))
    row_labels <- as.character(df[-1, 1])
    
    # extract counts
    mat <- apply(df[-1, -1], 2, as.numeric)
    
    
    validate(
      need(!anyNA(mat), "All count cells must be numeric."),
      need(all(mat >= 0), "Counts must be nonnegative."),
      need(sum(mat) > 0, "At least one count must be positive."),
      need(!anyDuplicated(row_labels), "Row names must be unique."),
      need(!anyDuplicated(col_labels), "Column names must be unique.")
    )
    
    rownames(mat) <- row_labels
    colnames(mat) <- col_labels
    
    if (!input$transpose) {
      mat <- t(mat)
    }
    
    mosaicplot(
      mat,
      main = "",
      color = TRUE,
      las = 1,
      cex.axis = 1.25
    )
    mtext(input$row_lab, cex = 1.75, side = 2, adj = 0.5, las = 3, line = 0.5) 
    mtext(input$col_lab, cex = 1.75, side = 3, adj = 0.5, las = 1, line = 0.5)
    
  })
  

}
