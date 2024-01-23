
library(shiny)
library(ggplot2)
library(MASS)
library(bit)

source("www/CorrelationGameSource.R")

function(input, output, session) {
  
  
  ##################################################
  #     ____             _      _____           _  #
  #    | __ )  __ _  ___| | __ | ____|_ __   __| | #
  #   |  _ \ / _` |/ __| |/ / |  _| | '_ \ / _` |  #
  #  | |_) | (_| | (__|   <  | |___| | | | (_| |   #
  # |____/ \__,_|\___|_|\_\ |_____|_| |_|\__,_|    #
  ##################################################
  
  ###################
  # Reactive Values #
  ###################
  
  alpha <- reactiveValues(n = 0)
  
  #####################################
  # Generating Plot Data and R Values #
  #####################################
  
  observeEvent(input$generate, {
    
    alpha$submitted <- F
    
    alpha$r_values = c(round(runif(1, -1, 1), 2))
    
    for (i in 1:3) {
      too_close <- T
      while (too_close) {
        too_close <- F
        new_r <- round(runif(1, -1, 1), 2)
        
        for (j in 1:length(alpha$r_values)) {
          if (abs(new_r - alpha$r_values[j]) < .1) {
            too_close <- T
          }
        }
      }
      alpha$r_values <- c(alpha$r_values, new_r)
    }
    
    for (i in 1:4) {
      updateCheckboxGroupInput(
        session, paste("in", i, sep = ""),
        choices = alpha$r_values, selected = NULL
      )
    }
    
    figures = c()
    alpha$r_true <- copy_vector(alpha$r_values)
    alpha$r_true <- sample(alpha$r_true, replace = F)
    plot_data <- data.frame(matrix(rep(0, input$sample_size*4), ncol = 4))
    
    for (i in 1:4) {
      r <- alpha$r_true[i]
      
      new_data <- mvrnorm(
        n = input$sample_size,
        mu = c(5, 5),
        Sigma = matrix(c(1,r,r,1),
                       nrow = 2),
        empirical = TRUE)
      plot_data[paste("X", i, sep = "")] <- new_data[,1]
      plot_data[paste("Y", i, sep = "")] <- new_data[,2]
    }
    
    alpha$data <- plot_data
  })
  
  
  ######################
  # Submitting Answers #
  ######################
  
  observeEvent(input$submit, {
    
    if (!alpha$submitted) {
      
      alpha$submitted <- T
      
      if (length(input$in1) > 0){
        if (as.numeric(input$in1) == alpha$r_true[1]){
          alpha$n = alpha$n + 1
        }
      }
      if (length(input$in2) > 0) {
        if (as.numeric(input$in2) == alpha$r_true[2]) {
          alpha$n = alpha$n + 1
        }
      }
      if (length(input$in3) > 0) {
        if (as.numeric(input$in3) == alpha$r_true[3]) {
          alpha$n = alpha$n + 1
        }
      }
      if (length(input$in4) > 0) {
        if (as.numeric(input$in4) == alpha$r_true[4]) {
          alpha$n = alpha$n + 1
        }
      }
    }
  })
  
  
  
  
  
  #####################################################
  #      _____                _     _____           _ #
  #    |  ___| __ ___  _ __ | |_  | ____|_ __   __| | #
  #   | |_ | '__/ _ \| '_ \| __| |  _| | '_ \ / _` |  #
  #  |  _|| | | (_) | | | | |_  | |___| | | | (_| |   #
  # |_|  |_|  \___/|_| |_|\__| |_____|_| |_|\__,_|    #
  ##################################################### 
  
  ###############################################
  # Allowing 1 selection per CheckboxGroupInput #
  ###############################################
  
  observeEvent(input$in1, {
    in1 <- input$in1
    if (length(in1) > 1) {
      updateCheckboxGroupInput(
        session, "in1", selected = tail(in1, 1))
    }
  })
  observeEvent(input$in2, {
    in2 <- input$in2
    if (length(in2) > 1) {
      updateCheckboxGroupInput(
        session, "in2", selected = tail(in2, 1))
    }
  })
  observeEvent(input$in3, {
    in3 <- input$in3
    if (length(in3) > 1) {
      updateCheckboxGroupInput(
        session, "in3", selected = tail(in3, 1))
    }
  })
  observeEvent(input$in4, {
    in4 <- input$in4
    if (length(in4) > 1) {
      updateCheckboxGroupInput(
        session, "in4", selected = tail(in4, 1))
    }
  })
  
  
  ####################
  # Displaying score #
  ####################
  
  output$score <- renderText({
    if (input$submit) {
      paste("Score: ", alpha$n, sep = "")
    }
  })
  
  
  ####################
  # Displaying Plots #
  ####################
  
  output$out1 <- renderPlot({
    if (input$generate) {
      new_plot <- ggplot(alpha$data) +
        geom_point(aes(X1,Y1), size = 3) +
        axistheme + plaintheme +
        theme(
          aspect.ratio = 1,
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
      if (alpha$submitted) {
        if (length(input$in3) < 1) {
          new_plot <- new_plot + labs(
            title = paste("Incorrect: r =", alpha$r_true[1]),
            x = "", y = ""
          )
        } else if (as.numeric(input$in1) == alpha$r_true[1]) {
          new_plot <- new_plot + labs(
            title = paste("Correct: r =", alpha$r_true[1]),
            x = "", y = ""
          )
        } else {
          new_plot <- new_plot + labs(
            title = paste("Incorrect: r =", alpha$r_true[1]),
            x = "", y = ""
          )
        }
      } else {
        new_plot <- new_plot +
          labs(x = "", y = "")
      }
      new_plot
    }
  })
  output$out2 <- renderPlot({
    if (input$generate) {
      new_plot <- ggplot(alpha$data) +
        geom_point(aes(X2,Y2), size = 3) +
        axistheme + plaintheme +
        theme(
          aspect.ratio = 1,
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
      if (alpha$submitted) {
        if (length(input$in2) < 1) {
          new_plot <- new_plot + labs(
            title = paste("Incorrect: r =", alpha$r_true[2]),
            x = "", y = ""
          )
        } else if (as.numeric(input$in2) == alpha$r_true[2]) {
          new_plot <- new_plot + labs(
            title = paste("Correct: r =", alpha$r_true[2]),
            x = "", y = ""
          )
        } else {
          new_plot <- new_plot + labs(
            title = paste("Incorrect: r =", alpha$r_true[2]),
            x = "", y = ""
          )
        }
      } else {
        new_plot <- new_plot +
          labs(x = "", y = "")
      }
      new_plot
    }
  })
  output$out3 <- renderPlot({
    if (input$generate) {
      new_plot <- ggplot(alpha$data) +
        geom_point(aes(X3,Y3), size = 3) +
        axistheme + plaintheme +
        theme(
          aspect.ratio = 1,
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
      if (alpha$submitted) {
        if (length(input$in3) < 1) {
          new_plot <- new_plot + labs(
            title = paste("Incorrect: r =", alpha$r_true[3]),
            x = "", y = ""
          )
        } else if (as.numeric(input$in3) == alpha$r_true[3]) {
          new_plot <- new_plot + labs(
            title = paste("Correct: r =", alpha$r_true[3]),
            x = "", y = ""
          )
        } else {
          new_plot <- new_plot + labs(
            title = paste("Incorrect: r =", alpha$r_true[3]),
            x = "", y = ""
          )
        }
      } else {
        new_plot <- new_plot +
          labs(x = "", y = "")
      }
      new_plot
    }
  })
  output$out4 <- renderPlot({
    if (input$generate) {
      new_plot <- ggplot(alpha$data) +
        geom_point(aes(X4,Y4), size = 3) +
        axistheme + plaintheme +
        theme(
          aspect.ratio = 1,
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
      if (alpha$submitted) {
        if (length(input$in4) < 1) {
          new_plot <- new_plot + labs(
            title = paste("Incorrect: r =", alpha$r_true[4]),
            x = "", y = ""
          )
        } else if (as.numeric(input$in4) == alpha$r_true[4]) {
          new_plot <- new_plot + labs(
            title = paste("Correct: r =", alpha$r_true[4]),
            x = "", y = ""
          )
        } else {
          new_plot <- new_plot + labs(
            title = paste("Incorrect: r =", alpha$r_true[4]),
            x = "", y = ""
          )
        } 
      } else {
        new_plot <- new_plot +
          labs(x = "", y = "")
      }
      new_plot
    }
  })
  
}