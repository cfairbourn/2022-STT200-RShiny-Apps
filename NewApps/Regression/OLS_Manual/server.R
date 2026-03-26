#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(ggplot2)

source("www/OLS_Manual-Source.R")

# Define server logic required to draw a histogram
function(input, output, session) {
  
  alpha = reactiveValues(data = c(), true = c(0,1), 
                         bool = F, mse = 0)
  
  # generate samples
  observeEvent(input$generate, {
    seed = 0
    n = 5
    # check if seed works
    if(is.numeric(input$seed) & !input$random) {
      seed = input$seed 
      set.seed(seed)
    } 
    if(is.integer(input$dots)) {
      n = input$dots
    }
    
    b0 = runif(1, -7.5, 7.5)
    b1 = runif(1, -2.5, 2.5)
    error_sd = rnorm(n, 0, 1)
    if(input$advanced) {
      b0 = input$b0_true
      b1 = input$b1_true
      error_sd = rnorm(n, 0, input$error_sd)
    }
    
    alpha$true = c(b0, b1)
    
    xvals = rnorm(n, 0, 1)
    yvals = b1*xvals + b0 + error_sd
    
    if(!alpha$bool | input$random) {
      updateNumericInput(
        session, "b0",
        value = round(mean(yvals), 4)
      )
      updateNumericInput(
        session, "b1",
        value = 0
      )
    }
    
    
    df = data.frame("X" = xvals, "Y" = yvals)
    
    # save dataframe
    alpha$data = df
    alpha$bool = T
    
  })
  
  output$out_plot = renderPlot({
    if(alpha$bool) {
      df = alpha$data
      df = as.data.frame(df)
      colnames(df) = c("X","Y")
      # sanitizing inputs
      b0 = 0
      b1 = 1

      if(is.numeric(input$b0)) {
        b0 = input$b0
      }
      if (is.numeric(input$b1)) {
        b1 = input$b1
      }

      # getting yhat, calculating rmse
      yhat = b1*df$X + b0
      errors = df$Y - yhat
      errors = errors^2
      alpha$mse = mean(errors)
      
      # plotting points
      plot = ggplot() +
        geom_point(aes(df$X, df$Y), size = 3) +
        axistheme + plaintheme + theme(aspect.ratio = 1)
      
      # getting lines
      xrange = range(df$X)
      xval = c(min(df$X) - .1*xrange, max(df$X) + .1*xrange)
      yval = b1*xval + b0
      df2 = data.frame(matrix(c(xval,yval),ncol=2))
      colnames(df2) = c('X','Y')
      # plotting lines
      plot = plot + geom_line(aes(df2$X, df2$Y), 
                              linewidth = 2, linetype = 'dashed') +
        scale_x_continuous(
          breaks = as.integer(seq(min(xval), max(xval), by = 1))) +
        scale_y_continuous(
          breaks = as.integer(seq(min(yval), max(yval), by = 1)))
      
      xmin = c()
      xmax = c()
      ymin = c()
      ymax = c()
      
      # plotting boxes
      for(i in 1:nrow(df)) {
        # calculate residual
        x = df[i,1]
        y = df[i,2]
        y_new = yhat[i]
        residual = y - y_new
        
        # find which direction square will go
        if(b1 * residual > 0) {
          # positive slope, positive error
          # negative slope, negative error
          # square to the left
          x_new = x - residual

        } else {
          # positive slope, negative error
          # negative slope, positive error
          # square to the right
          x_new = x + residual
        }
        
        xmin = c(xmin, min(x, x_new))
        xmax = c(xmax, max(x, x_new))
        ymin = c(ymin, min(y, y_new))
        ymax = c(ymax, max(y, y_new))
       
        
      }
      
      
      # Plot rectangle with geom_rect
      plot = plot +
        geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  alpha = 0.25, color = 'black', fill = 'green')
      
      plot

    }
  })
  
  
  
  output$true = renderText({
    b0 = alpha$true[1]
    b1 = alpha$true[2]
    out_str = sprintf("yhat = %sx + %s",
                      round(b1, 4), round(b0, 4))
    
    paste(out_str)
  })
  
  output$fitted = renderText({
    df = as.data.frame(alpha$data)
    model = lm (Y ~ X, data = df)
    b0 = coef(model)[1]
    b1 = coef(model)[2]
    
    out_str = sprintf("yhat = %sx + %s", round(b1, 4), round(b0, 4))
    
    paste(out_str)
  })

  output$rmse = renderText({
    if(alpha$bool) {
      paste("mse:", alpha$mse)  
    }
  })
}
