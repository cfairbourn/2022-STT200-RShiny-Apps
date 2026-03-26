library(shiny)
library(ggplot2)
library(plotly)

function(input, output, session) {
  
  alpha <- reactiveValues(data = NULL, true = c(0, 1), bool = FALSE, mse = 0)
  
  observeEvent(input$generate, {
    seed <- ifelse(is.numeric(input$seed) & !input$random, input$seed, 2024)
    set.seed(seed)
    
    n <- input$dots
    b0 <- ifelse(input$advanced, input$b0_true, runif(1, -7.5, 7.5))
    b1 <- ifelse(input$advanced, input$b1_true, runif(1, -2.5, 2.5))
    error_sd <- ifelse(input$advanced, input$error_sd, 1)
    
    xvals <- rnorm(n, 0, 1)
    yvals <- b1 * xvals + b0 + rnorm(n, 0, error_sd)
    
    updateNumericInput(session, "b0", value = round(mean(yvals), 4))
    updateNumericInput(session, "b1", value = 0)
    
    df <- data.frame(X = xvals, Y = yvals)
    alpha$data <- df
    alpha$bool <- TRUE
  })
  
  output$out_plot <- renderPlotly({
    if (alpha$bool) {
      df <- alpha$data
      model <- lm(Y ~ X, data = df)
      
      plot_ly(data = df, x = ~X, y = ~Y, type = 'scatter', mode = 'markers') %>%
        add_lines(x = ~X, y = predict(model), type = 'scatter', mode = 'lines', line = list(color = 'red')) %>%
        layout(showlegend = FALSE) %>%
        config(edits = list(shapePosition = TRUE))
    }
  })
  
  output$true <- renderPrint({
    b0 <- alpha$true[1]
    b1 <- alpha$true[2]
    paste("True Model: yhat = ", round(b1, 4), "x +", round(b0, 4))
  })
  
  output$fitted <- renderPrint({
    if (alpha$bool) {
      df <- alpha$data
      model <- lm(Y ~ X, data = df)
      b0 <- coef(model)[1]
      b1 <- coef(model)[2]
      paste("Fitted Model: yhat = ", round(b1, 4), "x +", round(b0, 4))
    }
  })
  
  output$rmse <- renderPrint({
    if (alpha$bool) {
      df <- alpha$data
      model <- lm(Y ~ X, data = df)
      predictions <- predict(model, df)
      mse <- mean((df$Y - predictions)^2)
      paste("Mean Squared Error (MSE):", round(mse, 4))
    }
  })
  
}
