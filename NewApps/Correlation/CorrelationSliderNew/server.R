
#################################
# Correlation Slider App Server #
#################################


library(shiny)
require(MASS)
library(ggplot2)

function(input, output) {
  
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
  
  alpha <- reactiveValues()
  
  
  
  #####################################################
  #      _____                _     _____           _ #
  #    |  ___| __ ___  _ __ | |_  | ____|_ __   __| | #
  #   | |_ | '__/ _ \| '_ \| __| |  _| | '_ \ / _` |  #
  #  |  _|| | | (_) | | | | |_  | |___| | | | (_| |   #
  # |_|  |_|  \___/|_| |_|\__| |_____|_| |_|\__,_|    #
  ##################################################### 
  
  ###########
  # Results #
  ###########
  
  output$distPlot <- renderPlot({
  
      # Getting from input
    r <- input$r_value_number
    if (input$r_choice == "Slider") {
      r <- input$r_value_slider
    } 
    if (input$seed_choice == "Set") {
      set.seed(input$seed)
    }
    
    # Generating Data
    dat2 <- mvrnorm(n=input$sample_size, 
                    mu=c(5, 5), 
                    Sigma=matrix(c(1, r, r, 1), 
                                 nrow=2), 
                    empirical=TRUE) 
    
    # Turning into Dataframe
    dat2 <- as.data.frame(dat2) 
    
    # Give Dataframe columns names
    colnames(dat2) <- c("X", "Y") 
    
    # Use one or the other plotting function, never both
    # Plotting using ggplot2
    ggplot(dat2) +
      geom_point(aes(X, Y), size = 3) +
      axistheme + plaintheme +
      theme(aspect.ratio = 1, 
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
    
    # Plotting using base R
    
    # plot(Y~X, data = dat2,
    #      ylab = "Y", ylim = c(2,8),
    #      xlab = "X", xlim = c(2,8),
    #      xaxt='n', yaxt='n', asp = 1,
    #      axes = FALSE,
    #      pch = 19, cex.main = 2, cex.lab = 1.5, cex.axis = 1.5) #Make the plot
  })
}
