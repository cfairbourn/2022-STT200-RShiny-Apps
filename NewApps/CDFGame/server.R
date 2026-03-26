# Developed by Colin Fairbourn 2025 07 31
library(shiny)
library(tidyverse)
library(bit)

source("www/CDFGameSource.R")

# function for generating the plots
generate_plot <- function(
  dist_type = "Normal",
  area_type = "Between",
  limits = NULL,
  df = NULL,
  bounds = NULL
) {
  func_list <- c("Normal" = dnorm, "T" = dt, "Chisq" = dchisq)
  arg_list <- list(
    "Normal" = list(),
    "T" = list(df = df),
    "Chisq" = list(df = df)
  )

  # Retrieve the correct function and arguments
  dist_func <- func_list[[dist_type]]
  dist_args <- arg_list[[dist_type]]

  lower <- bounds[1]
  upper <- bounds[2]

  # Compute heights at lower and upper bounds
  y_lower <- do.call(dist_func, c(list(lower), dist_args))
  y_upper <- do.call(dist_func, c(list(upper), dist_args))

  # initialize plot
  new_plot <- ggplot(NULL)
  # drawing the area under the curve
  # if two tailed area type, need separate command for each tail
  if (area_type == "Two-tailed") {
    new_plot <- new_plot +
      stat_function(
        fun = dist_func,
        args = dist_args,
        geom = "area",
        fill = "darkgreen",
        alpha = 0.5,
        xlim = c(limits[1], lower)
      ) +
      stat_function(
        fun = dist_func,
        args = dist_args,
        geom = "area",
        fill = "darkgreen",
        alpha = 0.5,
        xlim = c(upper, limits[2])
      )
  } else {
    # general area under curve plotting for all but two tailed
    new_plot <- new_plot +
      stat_function(
        fun = dist_func,
        args = dist_args,
        geom = "area",
        fill = "darkgreen",
        alpha = 0.5,
        xlim = c(lower, upper)
      )
  }

  new_plot <- new_plot +
    # drawing the vertical lines
    geom_segment(
      aes(x = lower, xend = lower, y = 0, yend = y_lower),
      color = "darkgreen",
      size = 1.5
    ) +
    geom_segment(
      aes(x = upper, xend = upper, y = 0, yend = y_upper),
      color = "darkgreen",
      size = 1.5
    ) +
    # drawing the distribution line
    stat_function(
      fun = dist_func,
      args = dist_args,
      geom = "line",
      color = "black",
      xlim = limits,
      size = 1
    ) +
    labs(x = "", y = "") +
    axistheme +
    plaintheme

  return(new_plot)
}

function(input, output, session) {
  ###############################################
  #  ____             _      _____           _  #
  # | __ )  __ _  ___| | __ | ____|_ __   __| | #
  # |  _ \ / _` |/ __| |/ / |  _| | '_ \ / _` | #
  # | |_) | (_| | (__|   <  | |___| | | | (_| | #
  # |____/ \__,_|\___|_|\_\ |_____|_| |_|\__,_| #
  ###############################################

  ###################
  # Reactive Values #
  ###################

  alpha <- reactiveValues(score = 0, submitted = F)

  ###################################
  # Generating Parameters for Plots #
  ###################################

  observeEvent(input$generate, {
    alpha$submitted <- F

    # function generating areas that will be used for the plots
    generate_areas <- function() {
      return(sample(c(
        # shuffling
        sample(seq(.01, .1, .01), 1), # smaller
        sample(seq(.15, .35, .01), 1), # small
        sample(seq(.45, .65, .01), 1), # large
        sample(seq(.7, .97, .01), 1) # larger
      )))
    }

    area_values <- generate_areas()
    # ensuring no two areas are too similar
    while (any(dist(area_values) < .05)) {
      area_values <- generate_areas()
    }

    # generate random area types based on those allowed
    # this line includes shuffles the areas given and places them in the area types
    area_types <- sample(input$area_type, replace = F)
    if (length(area_types) < 4) {
      # randomly filling out the rest of the types needed for 4 plots
      area_types <- c(
        area_types,
        sample(
          input$area_type,
          4 - length(area_types),
          replace = T
        )
      )
    }
    # final shuffle
    area_types <- sample(area_types)

    # generate area bounds based on the area given, area type ~ distribution
    dist_type <- input$dist_type
    df <- input$df_slider

    # determine the limits based on the distribution
    # find the limits of the graph based on distribution and df (if applicable)
    # that make it so that exactly 99.5% of the total area underneath the curve is displayed
    if (dist_type == "Normal") {
      limits <- c(qnorm(0.0025), qnorm(0.9975))
    } else if (dist_type == "T") {
      limits <- c(qt(.0025, df), qt(.9975, df))
    } else {
      limits <- c(0, qchisq(0.9995, df))
    }

    func_list <- c(
      "Normal" = qnorm,
      "T" = qt,
      "Chisq" = qchisq
    )
    arg_list <- list(
      "Normal" = list(),
      "T" = list(df = input$df_slider),
      "Chisq" = list(df = input$df_slider)
    )

    # Retrieve the correct function and arguments
    dist_func <- func_list[[dist_type]]
    dist_args <- arg_list[[dist_type]]

    # initialize bounds matrix
    bounds <- matrix(0, nrow = 4, ncol = 2)

    for (i in 1:4) {
      area_type <- area_types[i]
      area <- area_values[i]
      if (area_type == "Between") {
        # if normal, different depending on area type
        # between, bounds are random, but area = pnorm(upper) - pnorm(lower)
        lower <- qnorm((1 - area) / 2)
        # choose random point between lower limit and lower bound, new lower bound
        lower <- runif(1, limits[1], lower)
        # find upper bound s.t. area = pnorm(upper) - pnorm(lower)
        upper <- qnorm(area + pnorm(lower))
        # flip a coin to see if the bounds swap
        flip_index <- sample(c(T, F), 1)
        if (flip_index) {
          temp <- lower
          lower <- upper
          upper <- temp
        }
      } else if (area_type == "Left") {
        # left tailed, lower = limit, upper = qfunc(area)
        lower <- limits[1]
        upper <- do.call(dist_func, c(list(area), dist_args))
      } else if (area_type == "Right") {
        # right tailed, lower = qfunc(1-area), upper = limit
        lower <- do.call(dist_func, c(list(1 - area), dist_args))
        upper <- limits[2]
      } else {
        # two-tailed, lower = qfunc(area/2), upper = -1*lower
        lower <- do.call(dist_func, c(list((area) / 2), dist_args))
        upper <- -1 * lower
      }
      bounds[i, ] <- c(lower, upper)
    }

    # adding values to alpha
    alpha$area_values <- area_values # plot display order
    alpha$area_answer <- sample(area_values) # checkbox answer order
    alpha$area_types <- area_types # area type order
    alpha$limits <- limits
    alpha$bounds <- bounds

    # updating checkboxes with answers
    for (i in 1:4) {
      updateCheckboxGroupInput(
        session,
        paste("in", i, sep = ""),
        choices = alpha$area_answer,
        selected = NULL
      )
    }
  })

  ######################
  # Submitting Answers #
  ######################

  observeEvent(input$submit, {
    if (!is.null(alpha$submitted) && !alpha$submitted) {
      alpha$submitted <- T
      inputs <- matrix(0, nrow = 4, ncol = 2)
      user_inputs <- list(input$in1, input$in2, input$in3, input$in4)
      for (i in 1:4) {
        if (!is.null(user_inputs[[i]])) {
          inputs[i, 1] <- user_inputs[[i]] # Actual selected value
        } else {
          inputs[i, 1] <- 0 # 0 means skipped
        }
        inputs[i, 2] <- alpha$area_values[i] # Correct answer
      }
      alpha$score = alpha$score + sum(inputs[, 1] == inputs[, 2])
    }
  })

  ##################################################
  #  _____                _     _____           _  #
  # |  ___| __ ___  _ __ | |_  | ____|_ __   __| | #
  # | |_ | '__/ _ \| '_ \| __| |  _| | '_ \ / _` | #
  # |  _|| | | (_) | | | | |_  | |___| | | | (_| | #
  # |_|  |_|  \___/|_| |_|\__| |_____|_| |_|\__,_| #
  ##################################################

  ###############################################
  # Allowing 1 selection per checkboxGroupInput #
  ###############################################

  observeEvent(input$dist_type, {
    dist <- input$dist_type
    # When dist changes, update options in sidebar
    if (dist == "Normal") {
      # allow all area options for Normal distribution
      updateCheckboxGroupInput(
        session,
        "area_type",
        choices = c("Between", "Left", "Right", "Two-tailed"),
        selected = c("Between", "Left", "Right", "Two-tailed")
      )
    } else if (dist == "T") {
      # disallow "Between" as an area option for T dist
      updateCheckboxGroupInput(
        session,
        "area_type",
        choices = c("Left", "Right", "Two-tailed"),
        selected = c("Left", "Right", "Two-tailed")
      )
      # update slider input for d.o.f to have a max of 15 for chi squared
      updateSliderInput(
        session,
        "df_slider",
        min = 3,
        max = 50,
        value = 10,
        step = 1
      )
    } else {
      # enforce only right tailed area generations for chi squared
      updateCheckboxGroupInput(
        session,
        "area_type",
        choices = c("Right"),
        selected = c("Right")
      )
      # update slider input for d.o.f to have a max of 15 for chi squared
      updateSliderInput(
        session,
        "df_slider",
        min = 3,
        max = 16,
        value = 4,
        step = 1
      )
    }
  })

  ####################
  # Displaying score #
  ####################

  output$score <- renderText({
    if (input$submit) {
      paste("Score: ", alpha$score, sep = "")
    }
  })

  ####################
  # Generating Plots #
  ####################

  observeEvent(input$generate, {
    plots <- list()

    for (i in 1:4) {
      alpha$plots[[i]] <- generate_plot(
        dist_type = input$dist_type,
        area_type = alpha$area_types[i],
        limits = alpha$limits,
        df = input$df_slider,
        bounds = alpha$bounds[i, ]
      )
    }
  })

  ####################
  # Displaying Plots #
  ####################

  output$out1 <- renderPlot({
    req(c(input$generate, alpha$submitted))
    isolate({
      base_plot <- alpha$plots[[1]]

      if (alpha$submitted) {
        title_text <- if (
          length(input$in1) > 0 &&
            abs(as.numeric(input$in1) - alpha$area_values[1]) < 1e-4
        ) {
          paste("Correct: Area =", alpha$area_values[1])
        } else {
          paste("Incorrect: Area =", alpha$area_values[1])
        }

        base_plot <- base_plot + labs(title = title_text)
      }

      base_plot
    })
  })

  output$out2 <- renderPlot({
    req(c(input$generate, alpha$submitted))
    isolate({
      base_plot <- alpha$plots[[2]]

      if (alpha$submitted) {
        title_text <- if (
          length(input$in2) > 0 &&
            abs(as.numeric(input$in2) - alpha$area_values[2]) < 1e-4
        ) {
          paste("Correct: Area =", alpha$area_values[2])
        } else {
          paste("Incorrect: Area =", alpha$area_values[2])
        }

        base_plot <- base_plot + labs(title = title_text)
      }

      base_plot
    })
  })

  output$out3 <- renderPlot({
    req(c(input$generate, alpha$submitted))
    isolate({
      base_plot <- alpha$plots[[3]]

      if (alpha$submitted) {
        title_text <- if (
          length(input$in3) > 0 &&
            abs(as.numeric(input$in3) - alpha$area_values[3]) < 1e-4
        ) {
          paste("Correct: Area =", alpha$area_values[3])
        } else {
          paste("Incorrect: Area =", alpha$area_values[3])
        }

        base_plot <- base_plot +
          labs(title = title_text) +
          geom_text(aes(
            x = -2,
            y = .35,
            label = title_text
          ))
      }

      base_plot
    })
  })

  output$out4 <- renderPlot({
    req(c(input$generate, alpha$submitted))
    isolate({
      base_plot <- alpha$plots[[4]]

      if (alpha$submitted) {
        title_text <- if (
          !is.null(input$in4) &&
            abs(as.numeric(input$in4) - alpha$area_values[4]) < 1e-4
        ) {
          paste("Correct: Area =", alpha$area_values[4])
        } else {
          paste("Incorrect: Area =", alpha$area_values[4])
        }

        base_plot <- base_plot + labs(title = title_text)
      }

      base_plot
    })
  })

  # output$debug <- renderText({
  #   c(input$in1, input$in2, input$in3, input$in4)
  # })
}
