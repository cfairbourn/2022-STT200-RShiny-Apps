
#########################################################
# Two Mean Difference Confidence Interval Server Script #
#########################################################

library(shiny)
library(ggplot2)
library(dplyr)
library(openintro)
library(rhandsontable)
library(data.table)
library(shinyhelper)

source("www/TwoMeanDiffSource.R")

server <- function(input, output, session) {
  
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
  
  alpha <- reactiveValues(samples = vector(), bool = F)
  
  
  ##########################################################
  # Preset -> Numeric, Category, -> g1, g2 -> Data1, Data2 #
  ##########################################################
  
  observeEvent(input$preset, {
    if (input$preset %in% DataStrings) {
      i = which(DataStrings == input$preset)
      alpha$df <- DataPresets[[i]]
      # get column names to update the numerical and categorical inputs
      numeric_cols <- colnames(select_if(alpha$df, is.numeric))
      categoricals <- colnames(alpha$df)
      # update columns for numeric based on preset
      updateSelectInput(
        session, "numerical",
        choices = numeric_cols,
      )
      # update columns for categorical based on preset
      if (categoricals[1] != numeric_cols[1]) {
        updateSelectInput(
          session, "categorical",
          choices = categoricals
        )
      } else {
        updateSelectInput(
          session, "categorical",
          choices = categoricals,
          selected = categoricals[2]
        )
      }
    } 
  })
  
  # triggering event above trickles down to trigger event below
  observeEvent(input$categorical, {
    i <- which(colnames(alpha$df) == input$categorical)
    categories = sort(unique(unlist(alpha$df[,i])))
    
    # only update if there are more than two categories for that column
    if(length(categories) > 1) {
      updateSelectInput(
        session, "group1",
        choices = categories,
        selected = categories[1]
      )
      updateSelectInput(
        session, "group2",
        choices = categories,
        selected = categories[2]
      )
    }
  })
  
  # triggering events above trickle down to trigger events below
  observeEvent(c(input$numerical, input$group1), {
    i <- which(colnames(alpha$df) == input$categorical)
    j <- which(colnames(alpha$df) == input$numerical)
    # update the data to be numerical column such that categorical == group 1
    alpha$data1 <- alpha$df[alpha$df[,i] == input$group1,]
    alpha$data1 <- unlist(alpha$data1[,j])
  })
  observeEvent(c(input$numerical, input$group2), {
    i <- which(colnames(alpha$df) == input$categorical)
    j <- which(colnames(alpha$df) == input$numerical)
    # update the data to be numerical column such that categorical == group 2
    alpha$data2 <- alpha$df[alpha$df[,i] == input$group2,]
    alpha$data2 <- unlist(alpha$data2[,j])
  })
  
  
  ##################
  # Uploading data #
  ##################
  
  observeEvent(input$file, {
    if (!is.null(input$file)) {
      alpha$upload <- read.csv(input$file$datapath, stringsAsFactors=FALSE)
    }
    alpha$df <- alpha$upload
    numeric_cols <- colnames(select_if(alpha$df, is.numeric))
    categoricals <- colnames(alpha$df)
    # update columns for numeric based on preset
    updateSelectInput(
      session, "numerical",
      choices = numeric_cols,
    )
    # update columns for categorical based on preset
    updateSelectInput(
      session, "categorical",
      choices = categoricals
    )
  })
  
  
  ############################
  # Custom Table Integration #
  ############################
  
  gamma <- reactiveValues(custom = X)
  delta <- reactiveValues(custom = Y)
  
  # Adding Rows to the Custom Table
  observeEvent(input$add_rows1, {
    if(!is.na(input$num_rows1)) {
      # save current table as double (vector)
      x <- gamma$custom[[1]] 
      # get number of rows to add
      n <- ceiling(input$num_rows1)
      # get position of last row to be added
      y <- rep(0, n) 
      # set value at pos i to 1, all values in between are NA
      x <- c(x,y) 
      # update gamma$custom
      gamma$custom <- data.table(x) 
    }
  })
  
  observeEvent(input$add_rows2, {
    if(!is.na(input$num_rows2)) {
      # save current table as double (vector)
      x <- delta$custom[[1]] 
      # get number of rows to add
      n <- ceiling(input$num_rows2)
      # get position of last row to be added
      y <- rep(0, n) 
      # set value at pos i to 1, all values in between are NA
      x <- c(x,y) 
      # update delta$custom
      delta$custom <- data.table(x) 
    }
  })
  
  # Subtracting Rows to the Custom Table
  observeEvent(input$sub_rows1, {
    if(!is.na(input$num_rows1)) {
      # save current table as double (vector)
      x <- gamma$custom[[1]]
      # get total current length of table
      m <- length(x) 
      # get number of rows wished to be subtracted
      n <- ceiling(input$num_rows2) 
      # get new length of table
      i <- max(c(1, m-n)) 
      # set x = to itself up to new length
      x <- x[1:i] 
      # update gamma$custom
      gamma$custom <- data.table(x) 
    }
  })
  
  observeEvent(input$sub_rows2, {
    if(!is.na(input$num_rows2)) {
      # save current table as double (vector)
      x <- delta$custom[[1]]
      # get total current length of table
      m <- length(x) 
      # get number of rows wished to be subtracted
      n <- ceiling(input$num_rows2) 
      # get new length of table
      i <- max(c(1, m-n)) 
      # set x = to itself up to new length
      x <- x[1:i] 
      # update gamma$custom
      delta$custom <- data.table(x) 
    }
  })
  
  observeEvent(c(input$c_table1, input$c_table2), {
    # converts to a list
    gamma$custom <- hot_to_r(input$c_table1) 
    # converts to a list
    delta$custom <- hot_to_r(input$c_table2)
    if(input$preset == "Custom") {
      alpha$data1 <- data.frame(gamma$custom)[,1]
      alpha$data2 <- data.frame(delta$custom)[,1]
    }
  })
  
  
  ######################
  # Generating Samples #
  ######################
  
  observeEvent(input$generate, {
    n = ifelse(is.integer(input$n_samples),
               input$n_samples, 1)
    new_samples <- replicate(
      n, TwoMeanDiffCI(alpha$data1, alpha$data2)
    )
    alpha$samples <- c(alpha$samples, new_samples)
    alpha$bool = length(alpha$samples) > 0
  })
  
  
  #############
  # Resetting #
  #############
  
  # if any data integration settings change
  observeEvent(
    c(input$reset, input$preset, input$numerical,
      input$categorical, input$group1, input$group2,
      alpha$data1, alpha$data2), {
        alpha$samples <- vector()
        alpha$bool = F
      })
  
  
  # ##########################
  # # Dealing with the limit #
  # ##########################
  # 
  # observeEvent(c(input$limit, input$count_type), {
  #   alpha$bool[2] <- !is.na(input$limit)
  #   if(alpha$bool[2]) {
  #     alpha$limit <- input$limit
  #   } else {
  #     alpha$limit <- 0
  #   }
  # })
  
  
  ###############################
  # Interaction Post Generation #
  ###############################
  
  # only allow certain interaction when samples generated > 0
  output$show_samples <- reactive({
    alpha$bool1 <- length(alpha$samples) > 0
    return(alpha$bool1)
  })
  outputOptions(output, "show_samples", suspendWhenHidden = FALSE)
  
  
  ##############################
  # Selecting Specific Samples #
  ##############################
  
  # updating based on number of samples generated
  observeEvent(alpha$samples, {
    updateSliderInput(
      session, "selected_sample",
      min = 1, max = length(alpha$samples)
    )
  })
  
  # Allowing plot to display the specific sample
  
  
  
  
  
  
  
  
  
  
  
  
  #####################################################
  #      _____                _     _____           _ #
  #    |  ___| __ ___  _ __ | |_  | ____|_ __   __| | #
  #   | |_ | '__/ _ \| '_ \| __| |  _| | '_ \ / _` |  #
  #  |  _|| | | (_) | | | | |_  | |___| | | | (_| |   #
  # |_|  |_|  \___/|_| |_|\__| |_____|_| |_|\__,_|    #
  ##################################################### 
  
  ############
  # Data Tab #
  ############
  
  #######################
  # Custom Table Output #
  #######################
  
  # converts list to editable table
  output$c_table1 <- renderRHandsontable({
    rhandsontable(gamma$custom) 
  })
  output$c_table2 <- renderRHandsontable({
    rhandsontable(delta$custom)
  })
  
  #######################
  # Regular Data Output #
  #######################
  
  output$out_data1 <- renderTable({
    alpha$data1
  })
  output$out_data2 <- renderTable({
    alpha$data2
  })
  
  
  
  ####################
  # Data Information #
  ####################
  
  output$og_info1 <- renderUI({
    # Get means and sd
    alpha$og_mean1 <- mean(alpha$data1, na.rm = T)
    alpha$og_sd1 <- sd(alpha$data1, na.rm = T)
    alpha$og_n1 <- length(alpha$data1)
    # Display text with a line break
    str0 <- paste("Group 1:")
    str1 <- paste("Mean:", round(alpha$og_mean1, 3))
    str2 <- paste("SD:", round(alpha$og_sd1, 4))
    str3 <- paste("Count:", alpha$og_n1)
    HTML(paste(
      str0,
      str1,
      str2,
      str3,
      sep = "<br>"))
  })
  output$og_info2 <- renderUI({
    # Get means and sd
    alpha$og_mean2 <- mean(alpha$data2, na.rm = T)
    alpha$og_sd2 <- sd(alpha$data2, na.rm = T)
    alpha$og_n2 <- length(alpha$data2)
    # Display text with a line break
    str0 <- paste("Group 2:")
    str1 <- paste("Mean:", round(alpha$og_mean2, 3))
    str2 <- paste("SD:", round(alpha$og_sd2, 4))
    str3 <- paste("Count:", alpha$og_n2)
    HTML(paste(
      str0,
      str1,
      str2,
      str3,
      sep = "<br>"))
  })
  
  
  
  ###############
  # Visuals Tab #
  ###############
  
  ###########
  # Results #
  ###########
  
  output$results <- renderText({
    if (alpha$bool) {
      samples_df <- data.frame(alpha$samples)
      colnames(samples_df) <- "samples"
      
      alpha$mean <- round(mean(alpha$samples), 3)
      alpha$sd <- round(sd(alpha$samples), 4)
      
      mid <- 0.5*(1+input$conf_level)
      bounds <- samples_df %>%
        summarise(
          lower = quantile(samples, probs = 1-mid),
          upper = quantile(samples, probs = mid)
        )
      
      alpha$lower <- round(bounds$lower, 4)
      alpha$upper <- round(bounds$upper, 4)
      
      samples_df <- samples_df %>%
        mutate(
          int_color = ifelse(
            samples <= bounds$lower | samples >= bounds$upper,
            c_outer, c_inner),
          selected = 1) 
      
      alpha$samples_df <- samples_df
      level <- paste(100*input$conf_level, "%", sep = "")
      paste(level, sprintf(
        "Confidence Interval = (%s, %s)",
        alpha$lower, alpha$upper
      ))
    }
  })
  
  ############
  # Plotting #
  ############
  
  # null distribution plot generation
  output$distribution <- renderPlot({
    
    # only display if samples have been generated
    if (alpha$bool) {
      # colors
      manual_fills = factor(c(c_outer, c_inner),
                            levels = c(c_outer, c_inner))
      manual_colors = factor(c(c_outer, c_inner, c_select),
                             levels = c(c_outer, c_select, c_inner))
      
      n_bin <- ceiling(input$n_bins * length(table(alpha$samples))^0.5)
      
      ###########
      # Dotplot #
      ###########
      if (input$plot_type == "Dotplot") {
        new_df <- ColinDotPlot(alpha$samples_df, n_bin, input$n_cols)
        new_plot <- ggplot(new_df)
        
        
        # Multicolored Plot
        if ("Show Selected Sample" %in% input$lines) {
          # Display selected sample
          new_plot <- new_plot +
            geom_point(aes(
              x_vals, y_vals, color = int_color, size = selected))
        } else {
          # All dots are the same size
          new_plot <- new_plot +
            geom_point(aes(
              x_vals, y_vals, color = int_color))
        }
        
        if ("Overlay T Curve" %in% input$lines) {
          overlay_sd <- sqrt((alpha$og_sd1^2)/alpha$og_n1 + (alpha$og_sd2^2)/alpha$og_n2)
          new_plot <- new_plot +
            stat_function(
              fun = function(x) dnorm(x, alpha$og_diff, overlay_sd) * 
                max(new_df$y_vals) / dnorm(0, alpha$og_diff, overlay_sd),
              color = "black", size = 1.5)
        }
        
        new_plot <- new_plot +
          scale_y_continuous(
            limits = c(0, max(n_bin*0.75, max(new_df$y_vals)*1.01))) +
          scale_color_manual(name = c_outer, values = levels(manual_colors))
        
      } else {
        #############
        # Histogram #
        #############
        
        # Multicolored Plot
        new_plot <- ggplot(alpha$samples_df) +
          geom_histogram(aes(x = samples, fill = int_color),
                         bins = n_bin, color = c_select) +
          scale_color_manual(name = c_outer, values = levels(manual_colors))
        
        if ("Show Selected Sample" %in% input$lines) {
          # Display selected sample position
          new_plot <- new_plot +
            geom_text(aes(x = alpha$select_mean,
                          y = 0, label = alpha$select_mean),
                      color = c_select, size = 5,
                      hjust = -0.5, vjust = -2) +
            geom_vline(xintercept = alpha$select_mean, color = c_select,
                       size = 2, linetype = "longdash")
        }
      }
      
      
      ######################
      # LINE AT NULL VALUE #
      ######################
      
      if("Show Observed Difference" %in% input$lines) {
        new_plot <- new_plot +
          geom_vline(xintercept = alpha$og_diff, size = 2,
                     alpha = 0.5, color = c_inner)
      }
      
      
      #########################
      # LINE AT VALUE ENTERED #
      #########################
      
      if("Show Boundary Lines" %in% input$lines) {
        new_plot <- new_plot +
          # Text showing the numbers
          geom_text(aes(x = alpha$lower, y = 0, label = alpha$lower), 
                    color = c_select, size = 6, 
                    hjust = -0.5, vjust = -21) +
          geom_text(aes(x = alpha$upper, y = 0, label = alpha$upper), 
                    color = c_select, size = 6, 
                    hjust = -0.5, vjust = -21) +
          
          # Line positions in Pink
          geom_vline(xintercept = alpha$lower, color = c_outer, 
                     size = 2, linetype = "longdash") +
          geom_vline(xintercept = alpha$upper, color = c_outer, 
                     size = 2, linetype = "longdash")
      }
      
      furthest = max(abs(min(alpha$samples)), abs(max(alpha$samples)))
      x_ticks <- seq(0, furthest, len = input$x_ticks)
      
      new_plot +
        scale_x_continuous(
          breaks = c(-x_ticks, x_ticks[-1]),
          labels = signif(c(-x_ticks, x_ticks[-1]), 2)) +
        labs(title = "Differences in Randomized Means",
             x = "Difference in Randomized Means", y = "Frequency") +
        scale_fill_manual(name = c_outer, values = levels(manual_fills)) +
        theme(legend.position = "none") +
        plaintheme + axistheme
    }
  })
  
  
  # original data display
  output$data_visuals <- renderPlot({
    
    # Get means and sd
    alpha$og_mean1 <- mean(alpha$data1, na.rm = T)
    alpha$og_sd1 <- sd(alpha$data1, na.rm = T)
    
    alpha$og_mean2 <- mean(alpha$data2, na.rm = T)
    alpha$og_sd2 <- sd(alpha$data2, na.rm = T)
    
    # Title
    alpha$og_diff <- round(alpha$og_mean1 - alpha$og_mean2, 4)
    title <- paste("Observed Difference:", alpha$og_diff)
    
    
    boxplot(alpha$data1, alpha$data2, names = c(input$group1, input$group2),
            col = c("white", "white"), main = title)
    
    
    # Points
    stripchart(alpha$data1,                  # Data
               method = "jitter",            # Random noise
               at = 1,
               pch = 19,                     # Pch symbols
               col = rgb(0,0,1,alpha = 0.5), # Color of the symbol
               vertical = TRUE,              # Vertical mode
               add = TRUE)
    stripchart(alpha$data2,                  # Data
               method = "jitter",            # Random noise
               at = 2,
               pch = 19,                     # Pch symbols
               col = rgb(1,0,0,alpha = 0.5), # Color of the symbol
               vertical = TRUE,              # Vertical mode
               add = TRUE)
    
    
  })
  
  
  ###############################
  # Showing Observed Difference #
  ###############################
  
  # output$observed_diff <- renderText({
  #   paste("Observed Difference = ",alpha$og_diff)
  # })
  
  
  ################################
  # Showing Selected Sample Mean #
  ################################
  
  output$selected_sample_mean <- renderText({
    if (alpha$bool){
      alpha$select_mean <- signif(alpha$samples[input$selected_sample],4)
      
      alpha$samples_df[-input$selected_sample, "selected"] <- 1
      alpha$samples_df[input$selected_sample, "selected"] <- 2
      
      if ("Show Selected Sample" %in% input$lines) {
        paste("Mean: ", alpha$select_mean)
      }
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # output$debug <- renderTable({
  #   alpha$samples_df
  # })
  
  
  
  
}












