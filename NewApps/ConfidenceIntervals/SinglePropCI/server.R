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
library(ggplot2)
library(tidyverse)
library(shinyjs)
library(latex2exp)

source("www/SinglePropSource.R")

# Define server logic required to draw a histogram
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
  
  alpha = reactiveValues(samples = vector(), bool = F)


  ####################
  # Changing Presets #
  ####################
  observeEvent(input$preset, {
    i = which(names(Presets) == input$preset)
    chosen = Presets[[i]]
    # Update input type to be either Proportion or Counts
    updateSelectInput(session, "input_type", selected = Input_Types[chosen[1]])
    # Update Proportion of Success to preset
    updateNumericInput(session, "p_success", value = chosen[2])
    # Update Number of Successes to preset
    updateNumericInput(session, "c_success", value = chosen[3])
    # Update Sample Size to preset
    updateNumericInput(session, "sample_size", value = chosen[4])

  })
  
  #####################
  # Sanitizing Inputs #
  #####################
  # sample size limit
  observeEvent(input$sample_size, {
    if(input$sample_size > 2000) {
      updateNumericInput(session, "sample_size", value = 2000)
    }
  })
  # proportion & count lower and upper limits
  observeEvent(c(input$p_success, input$c_success, input$sample_size), {
    if(input$c_success > input$sample_size | input$p_success > 1) {
      updateNumericInput(session, "c_success", value = input$sample_size)
      updateNumericInput(session, "p_success", value = 1)
    } else if (input$c_success < 0 | input$p_success < 0) {
      updateNumericInput(session, "c_success", value = 0)
      updateNumericInput(session, "p_success", value = 0)
    }
  })
  # changing c_success or p_success changes the other accurately
  observeEvent(input$c_success, {
    p = input$c_success / input$sample_size
    updateNumericInput(session, "p_success", value = p)
  })
  observeEvent(input$p_success, {
    n = round(input$p_success * input$sample_size)
    updateNumericInput(session, "c_success", value = n)
  })


  ####################
  # Generate Samples #
  ####################
  observeEvent(input$generate, {
    n = 1
    if(is.numeric(input$n_sims)) {
      n = input$n_sims
    } 
    if (length(alpha$samples) + n > Sample_Limit) {
      n = Sample_Limit - length(alpha$samples)
    }
    s = input$sample_size
    p = input$p_success

    alpha$samples = c(alpha$samples, rbinom(n, s, p) / s)
    
    alpha$len = length(alpha$samples)
    alpha$bool = alpha$len > 0
    
    df = data.frame(samples = alpha$samples, index = 1:alpha$len)
    alpha$out_df = DotplotDataMaker(df)
    
    
    updateSliderInput(session, "select_sample", max = alpha$len)
  })


  ###############################
  # Interaction Post Generation #
  ###############################
  output$show_samples = reactive({
    return(alpha$bool)
  })
  outputOptions(output, "show_samples", suspendWhenHidden = FALSE)


  #################################################
  # Erase generated samples when input is changed #
  #################################################
  observeEvent(c(input$reset, input$p_success, 
                 input$sample_size, input$c_success), {
    alpha$samples = vector()
    alpha$len = 0
    alpha$bool = FALSE
  })
  
  
  
  
  
  
  #####################################################
  #      _____                _     _____           _ #
  #    |  ___| __ ___  _ __ | |_  | ____|_ __   __| | #
  #   | |_ | '__/ _ \| '_ \| __| |  _| | '_ \ / _` |  #
  #  |  _|| | | (_) | | | | |_  | |___| | | | (_| |   #
  # |_|  |_|  \___/|_| |_|\__| |_____|_| |_|\__,_|    #
  ##################################################### 
  
  ##########################################
  # Display Confidence Interval Boundaries #
  ##########################################
  output$bounds = renderText({
    if(alpha$bool) {
      # Converting to dataframe for integration with dplyr
      
      s = input$sample_size
      mid = 0.5 * (1 + input$conf_level)

      # calculating bounds
      bounds = alpha$out_df %>%
        summarise(
          lower = quantile(samples, probs = 1- mid),
          upper = quantile(samples, probs = mid))
      
      # adding color as a column for use in the plot
      alpha$out_df = alpha$out_df %>%
        mutate(in_interval = ifelse(
          samples <= bounds$lower | samples >= bounds$upper,
          c_outer, c_inner))

      # "rounding" them
      alpha$lower = signif(bounds$lower[1], 4)
      alpha$upper = signif(bounds$upper[1], 4)

      # displaying the text of the boundaries
      percent_str = paste(100*input$conf_level, "%", sep = "")
      output_str = paste(
        percent_str, sprintf("Confidence Interval = (%s, %s)",
                             alpha$lower, alpha$upper))
      paste(output_str)
    } else {
      paste("Press Generate")
    }
  })
  
  
  #########################################
  # Use LaTeX to create P-hat_sim = value #
  #########################################
  output$sample_info = renderUI({
    if(alpha$bool) {
      # for visibility in main plot
      alpha$out_df[-input$select_sample, "selected"] = 1
      alpha$out_df[input$select_sample, "selected"] = 2
      alpha$sample_mean = alpha$out_df[input$select_sample, "samples"]
      
      withMathJax(
        paste0("$$\\hat{p}_{\\small{\\textrm{sim}}} =",
               signif(alpha$sample_mean, 4), "$$"
        )
      )
    }
  })
  
  
  #############################
  # Distribution Main Display #
  #############################
  output$distribution = renderPlot({
    if(alpha$bool) {
      man_fills = factor(c(c_outer, c_inner), levels = c(c_outer, c_inner))
      man_color = factor(c(c_outer, c_inner, c_select), 
                         levels = c(c_outer, c_select, c_inner))
      if(input$color_switch) {
        man_fills = factor(c(c_inner, c_outer), levels = c(c_inner, c_outer))
        man_color = factor(c(c_inner, c_outer, c_select), 
                           levels = c(c_select, c_outer, c_inner))
        
      }
      
      if(input$plot_type == "Dotplot") {
        outplot = ggplot(alpha$out_df)
        
        if(input$sample_position) {
          # selected sample
          outplot = outplot + 
            geom_point(aes(xvals, yvals, color = in_interval, size = selected))
        } else {
          # regular
          outplot = outplot +
            geom_point(aes(xvals, yvals, color = in_interval))
        }
        outplot = outplot +
          scale_y_continuous(limits = c(0, max(alpha$out_df$yvals)*1.1)) +
          scale_color_manual(name = c_outer, values = levels(man_color))
        
      } else { # Histogram
        outplot = ggplot(alpha$df) +
          geom_histogram(aes(samples, fill = in_interval), color = c_select)
        
        # display selected sample mean as a line showing position
        if(input$sample_position) {
          outplot = outplot +
            # text denoting numerical position
            geom_text(aes(alpha$sample_mean, y = 0,
                          label = signif(alpha$sample_mean, 4)),
                      color = c_select, size = 5,
                      hjust = -0.5, vjust = -2) +
            # line position in black
            geom_vline(xintercept = alpha$sample_mean, color = c_select,
                       size = 2, linetype = "longdash")
        }
      }
      
      if (input$outer_percentages) {
        outplot = outplot + 
          geom_text(aes(x = alpha$lower, y = 0, 
                        label = paste((1-input$conf_level)/.02, "%", sep = "")),
                    color = c_select, size = 6,
                    hjust = 2.1, vjust = -10) +
          geom_text(aes(x = alpha$upper, y = 0,
                        label = paste((1-input$conf_level)/.02, "%", sep = "")),
                    color = c_select, size = 6,
                    hjust = -1.5, vjust = -10)
      }
      
      if (input$lines) {
        # display confidence interval boundary lines
        outplot = outplot +
          # text showing numbers
          geom_text(aes(x = alpha$lower, y = 0,
                        label = signif(alpha$lower, 4)),
                    color = c_select, size = 5,
                    hjust = -0.5, vjust = -18) +
          geom_text(aes(x = alpha$upper, y = 0,
                        label = signif(alpha$upper, 4)),
                    color = c_select, size = 5,
                    hjust = -0.5, vjust = -18) +
          # vertical line
          geom_vline(xintercept = alpha$lower, 
                     color = ifelse(input$color_switch, c_select, c_outer),
                     size = 2, linetype = "longdash", alpha = 0.5) +
          geom_vline(xintercept = alpha$upper, 
                     color = ifelse(input$color_switch, c_select, c_outer),
                     size = 2, linetype = "longdash", alpha = 0.5)
      }
      
      p = input$p_success
      s = input$sample_size

      furthest = max(abs(p - alpha$samples))
      lo = seq(p, p - furthest, by = -2/s)
      hi = seq(p, p + furthest, by = 2/s)

      xticks = unique(signif(c(lo, hi), 2))

      outplot +
        scale_x_continuous(breaks = xticks) +
        labs(
          title = paste(alpha$len, "Bootstrapped Proportions"),
          x = "Generated Samples", y = "Frequency") +
        scale_fill_manual(name = c_outer, values = levels(man_fills)) +
        theme(legend.position = "none") +
        plaintheme + axistheme
    }
  })
  
  #############
  # debugging #
  #############
  # output$debug = renderTable({
  #   alpha$out_df
  # })
}
