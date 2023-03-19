# ------------------------------------------------------------------------------
# File: TwoProportionResamplingTest/app.R
# Authors: Camille Fairbourn, Colin Fairbourn, Scott Manski
# Date: 08/22/2022 
# Desc: This app performs a two proportion test via randomization. The 
#       resampling test mimics Fisher's Exact Test.
# Published Location: 
# Email: fairbour@msu.edu, fairbou2@msu.edu, manskisc@msu.edu
#
# For questions or concerns, please email the authors. This work is licensed 
# under a Creative Commons Attribution-ShareAlike 4.0 International License 
# (https://creativecommons.org/licenses/by-sa/4.0/).
# ------------------------------------------------------------------------------


library(shiny)
library(DT)
library(tidyverse)
library(openintro)
library(shinyjs)
library(latex2exp)
library(htmltools)

source("www/TwoPropTest2-source.R")

# Presets$`preset name` C- c(r1c1, r2c1, r1c2, r2c2)
Presets = list()
Presets$`Custom` <- c(
  0,0,0,0,"A","B","Row A","Row B")
Presets$`Dolphin Therapy` <- c(
  10,4,5,11, "Improved", "Did not improve","Dolphin therapy","Control group")
Presets$`Pass or Fail` <- c(
  52,30,13,25, "Pass", "Fail", "Urban", "Rural")
Presets$`Sales Pitches` <- c(
  21,15,54,60, "Yes", "No", "New Pitch", "Current Pitch")
Presets$`Duct Tape Therapy` <- c(
  22,15,4,10, "Wart Gone", "Wart Remains","Duct Tape","Cryotherapy")
Presets$`Gender Discrimination` <- c(
  21,14,3,10, "Promotion", "No Promotion","Male","Female")
Presets$`Opportunity Cost` <- c(
  56,41,19,34, "buy DVD", "not buy DVD","control","treatment")
Presets$`Avandia` <- c(
  2593,5386,65000,154592, "Yes", "No", "Rosiglitazone","Pioglitazone")



ui <- fluidPage(
  titlePanel("Randomization Test for a Difference of Two Proportions"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Controls",
          tags$div(class="header", checked = NA,
                   tags$p("Enter your data into the table below, or choose one 
                           of the data presets. Press Construct the Null 
                           Distribution to simulate the results under 
                           an independence null model."
                   )
          ),
          hr(),
          selectInput("graph.type","Graph Type",
                      choices = c("Dotplot","Histogram")),
          selectInput("preset","Presets", choices = names(Presets), 
                      selected = "Custom"), # <- Change Default Value Here *****
          
          html_table,
          
          numericInput("n.samp","Number of Simulations",
                       value = 100), # <- Change Default Value Here **********
          actionButton("generate","Construct the Null Distribution"),
          actionButton("reset","Reset"),
          checkboxInput("show.adv","Advanced Controls"),
          conditionalPanel(
            condition = "input['show.adv']",
            sliderInput("dot.size","Dots Size",
                        min = 0.5, max = 4.5, step = 0.1,
                        value = 2), # <- Change Default Value Here **********
            sliderInput("bin.width","Number of Bins per SE",
                        min = 2, max = 6, step = 1,
                        value = 2) # <- Change Default Value Here **********
          )
          
        ),
        tabPanel(
          "Instructions",
          tags$div(class="header", checked = NA,
                   tags$p("Check 'Show Observed Difference' to see the observed 
                           difference of proportions from the entered data 
                           table. Enter the value in the text box under the 
                           graph."),
                   tags$p("For a two-sided alternate hypothesis, select 'further
                           from null value' from the dropdown menu. The graph 
                           will color the simulated results that are further 
                           from 0 than the value in the textbox and report
                           the fraction of colored results as the estimated
                           p-value. "),
                   tags$p("For a one-sided alternate hypothesis, select 
                           'greater than' or 'less than' from the dropdown menu.
                           The graph will color and report the fraction of 
                           the simulated results that are greater/less than the 
                           value in the textbox."),
                   hr(),
                   hr(),
                   tags$p("This app was written by Colin Fairbourn and 
                          Scott Manski for the Statistical Methods course 
                          at Michigan State University"),
                   tags$p("This work is licensed under a "),
                   tags$a(href="http://creativecommons.org/licenses/by-sa/4.0/", 
                          "Creative Commons Attribution-ShareAlike 4.0 
                          International License"),
                   hr(),
                   tags$p("The previous version of this app is archived at"),
                   tags$a(href="https://shiny.stt.msu.edu/fairbour/Archive/TwoProportion/",
                          "Archived Two Proportion Test"),
                   hr(),
                   bookmarkButton()
          )
        )
      )
    ),
    mainPanel(
      plotOutput("null.dist"),
      checkboxGroupInput(
        "lines", "", inline = T, choices = c(
          "Show Observed Difference",
          "Overlay Normal Curve",
          "Show Null Line",
          "Show Observed Line")), # <- Change Default Here
      htmlOutput("len.samps"),
      fluidRow(
        tags$head(
          tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }"))
        ),
        column(
          3,
          style = "margin-top: 25px; text-align: right;",
          tags$div(
            class = "header", checked = NA,
            tags$p("Count Proportions of Simulations"), 
          )
        ),
        column(
          3,
          selectInput(
            "count","",
            choices = c("further from null value than",
                        "greater than","less than"),
            selected = "further from null value than")
        ),
        column(
          3,
          numericInput("limit","","")
        ),
        column(
          3,
          style = "margin-top: 25px;",
          htmlOutput("results"),
          tags$head(
            tags$style(
              "#results{color: red;
            }"
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          checkboxInput("show.info","Show Null Distribution Information")
        ),
        column(
          6,
          style = "margin-top: 10px;",
          conditionalPanel(
            condition = "input['show.info']",
            htmlOutput("null.info")
          )
        )
      ),
      tableOutput("debug")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  alpha <- reactiveValues()
  alpha$samples <- vector()
  alpha$bool1 <- F
  alpha$bool2 <- F
  
  alpha$data <- matrix(rep(NA, 9), ncol=3)
  
  # beta <- reactiveValues(data = {
  #   data.frame(x = numeric(0), y = numeric(0), z = numeric(0),
  #              stringsAsFactors = F) %>%
  #     add_row(x = rep(0,3), y = rep(0,3), z = rep(0,3))
  # })
  
  # output$betatable <- renderDT({
  #   DT::datatable(
  #     beta$data, 
  #     editable = list(
  #       target = "cell", 
  #       numeric = 1:3),
  #     selection = 'none',
  #     options = list(dom = 't', ordering = F))
  # })
  
  output$TRT <- renderText({
    alpha$data[1, 3]
  })
  output$TRB <- renderText({
    alpha$data[2, 3]
  })
  output$TBL <- renderText({
    alpha$data[3, 1]
  })
  output$TBR <- renderText({
    alpha$data[3, 2]
  })
  output$Total <- renderText({
    alpha$data[3, 3]
  })
  
  observeEvent(
    c(input$TL, input$TR, input$BL, input$BR, input$C1N,
      input$C2N, input$R1N, input$R2N), {
        # combine the current table inputs into a vector
        current <- c(
          input$TL, input$BL, 
          input$TR, input$BR, 
          input$C1N, input$C2N,
          input$R1N, input$R2N)
        # loops through each preset and determines the number of cells that match the current table
        preset <- unlist(lapply(names(Presets), function(i) {
          sum(Presets[[i]] == current)
        }))
        
        alpha$samples <- vector()
        alpha$bool1 <- F
        
        # if all cells match, change the SelectInput value to that preset,
        # otherwise change the value to "Custom"
        if (sum(preset == 8) > 0) {
          updateSelectInput(session, "preset", selected = names(Presets)[which(preset == 8)])
        } else {
          updateSelectInput(session, "preset", selected = "Custom")
        }
      }, ignoreInit = TRUE)
  
  observeEvent(c(input$TL, input$TR, input$BL, input$BR), {
    alpha$data[1,1] <- as.numeric(input$TL) # top left position
    alpha$data[1,2] <- as.numeric(input$TR) # top right position
    alpha$data[2,1] <- as.numeric(input$BL) # bottom left position
    alpha$data[2,2] <- as.numeric(input$BR) # bottom right position
    # alpha$data[1,3] <- as.numeric(input$TL) + as.numeric(input$TR) #top sum
    # alpha$data[2,3] <- as.numeric(input$BL) + as.numeric(input$BR) # bottom sum
    # alpha$data[3,1] <- as.numeric(input$TL) + as.numeric(input$BL) # left sum
    # alpha$data[3,2] <- as.numeric(input$TR) + as.numeric(input$BR) # right sum
    # alpha$data[3,3] <- sum(as.numeric(
    #   c(input$TL, input$TR, input$BL, input$BR)))
    
    for (i in 1:2) {
      alpha$data[i,3] <- sum(alpha$data[i,1:2])
      alpha$data[3,i] <- sum(alpha$data[1:2,i])
    }
    alpha$data[3,3] <- sum(alpha$data[3,1:2])
    
    alpha$set <- unique(c(alpha$data[,3], alpha$data[3,]))
    alpha$obs.diff <- (alpha$data[1,1] / 
                         alpha$data[1,3]) - (alpha$data[2,1] / 
                                              alpha$data[2,3])
    
    alpha$p.hat <- (alpha$data[1,1] + alpha$data[2,1]) / alpha$data[3,3]
    alpha$old.SE <- sqrt(alpha$p.hat * (1 - alpha$p.hat) * 
                           ((1 / alpha$data[1,3]) + (1 / alpha$data[2,3])))
    
    alpha$bool1 <- F
    alpha$samples <- vector()
    
  })
  
  observeEvent(input$reset, {
    alpha$samples <- vector()
    alpha$bool1 <- F
  })
  
  observeEvent(input$preset, {
    chosen <- which(names(Presets) == input$preset)
    chosen <- Presets[[chosen]]
    
    # editable values
    alpha$data[1,1] <- as.numeric(chosen[1])
    alpha$data[2,1] <- as.numeric(chosen[2])
    alpha$data[1,2] <- as.numeric(chosen[3])
    alpha$data[2,2] <- as.numeric(chosen[4])
    
    # totals
    alpha$data[3,1] <- sum(as.numeric(chosen[1:2]))
    alpha$data[3,2] <- sum(as.numeric(chosen[3:4]))
    alpha$data[1,3] <- sum(as.numeric(chosen[c(1,3)]))
    alpha$data[2,3] <- sum(as.numeric(chosen[c(2,4)]))
    
    # sum totals
    alpha$data[3,3] <- sum(as.numeric(chosen[1:4]))
    alpha$samples <- vector()
    
    alpha$data.names <- chosen[5:8]
    
    updateTextInput(session, "TL", value = alpha$data[1,1])
    updateTextInput(session, "TR", value = alpha$data[1,2])
    updateTextInput(session, "BL", value = alpha$data[2,1])
    updateTextInput(session, "BR", value = alpha$data[2,2])
    updateTextInput(session, "C1N", value = alpha$data.names[1])
    updateTextInput(session, "C2N", value = alpha$data.names[2])
    updateTextInput(session, "R1N", value = alpha$data.names[3])
    updateTextInput(session, "R2N", value = alpha$data.names[4])
  })
  
  # observeEvent(c(input$preset, input$reset), {
  #   # data was changed, reset samples
  #   alpha$samples <- vector()
  #   alpha$bool1 <- F
  #   alpha$len <- 0
  #   
  #   # get values
  #   info = input$betatable_cell_edit
  #   i = as.numeric(info$row)
  #   j = as.numeric(info$col)
  #   k = as.numeric(info$value)
  #   
  #   # write values to reactive
  #   beta$data[i,j] <- k
  #   
  #   # update totals
  #   for (i in 1:2) {
  #     beta$data[i,3] <- sum(beta$data[i,1:2])
  #     beta$data[3,j] <- sum(beta$data[1:2,j])
  #   }
  #   beta$data[3,3] <- sum(beta$data[3,1:2])
  #   
  #   alpha$set <- unique(c(beta$data[,3], beta$data[3,]))
  #   alpha$obs.diff <- (beta$data[1,1] / 
  #                        beta$data[1,3]) - (beta$data[2,1] / 
  #                                             beta$data[2,3])
  #   
  #   
  #   
  # })
  
  
  # observeEvent(input$preset, {
  #   if (input$presets != "Custom") {
  #     preset_index <- which(names(Presets) == input$presets)
  #     preset <- Presets[[preset_index]]
  #     DF <- data.frame("X1" = as.numeric(preset[1:2]),
  #                      "X2" = as.numeric(preset[3:4]))
  #     DF[3, ] <- apply(DF, 2, sum)
  #     DF[, 3] <- apply(DF, 1, sum)
  #     alpha$data <- DF
  #     
  #   }
  # })
  
  
  observeEvent(input$generate, {
    if (!(0 %in% alpha$set)) {
      
      new.samples <- rhyper(input$n.samp, alpha$data[1,3],
                            alpha$data[2,3], alpha$data[3,1])
      
      new.samples <- (new.samples / 
                        alpha$data[1,3]) - ((alpha$data[3, 1] - new.samples) / 
                                             alpha$data[2, 3])
      
      alpha$bool1 <- T
      alpha$samples <- c(alpha$samples, new.samples)
      alpha$len <- length(alpha$samples)
    }
  })
  
  observeEvent(input$limit, {
    alpha$bool2 <- !is.na(input$limit)
    if (alpha$bool2) {
      alpha$limit <- input$limit
    }
  })
  
  output$len.samps <- renderUI({
    if ("Show Observed Difference" %in% input$lines) {
      str1 <- paste("Observed Difference:", signif(alpha$obs.diff,4))
      str2 <- paste("Samples Generated:", alpha$len)
      HTML(paste(str1, str2, sep = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))
    } else {
      HTML(paste("Samples Generated:", alpha$len))
    }
  })
  
  output$null.info <- renderUI({
    if (alpha$bool1) {
      alpha$mean <- signif(mean(alpha$samples),3)
      alpha$null.SE <- signif(sd(alpha$samples),4)
      
      str1 <- paste("Mean:", alpha$mean)
      str2 <- paste("SE:", alpha$null.SE)
      HTML(paste(str1, str2, sep = "&nbsp;&nbsp;&nbsp;"))
    } else {
      HTML(paste("Press Generate"))
    }
  })
  
  output$null.dist <- renderPlot({
    if (alpha$bool1) {
      
      man.fill <- factor(c(c.outer, c.inner),
                         levels = c(c.outer, c.inner))
      man.color <- factor(c(c.outer, c.inner),
                          levels = c(c.outer, c.select))
      
      if (input$graph.type == "Dotplot") {
        
        new.df <- SimpleDotPlot(alpha$df)
        # dotplot
        null.plot <- ggplot(new.df) 
        if (alpha$bool2 & alpha$n > 0) {
          null.plot <- null.plot + 
            geom_point(aes(x = x.vals, y = y.vals, color = int.color)) +
            scale_color_manual(values = levels(man.color))
        } else {
          null.plot <- null.plot + 
            geom_point(aes(x = x.vals, y = y.vals, color = c.select)) +
            scale_color_manual(values = c.select)
        }
        
        if ("Overlay Normal Curve" %in% input$lines) {
          null.plot <- null.plot +
            stat_function(
              fun = function(x) dnorm(
                x, mean = 0, sd = alpha$old.SE) *
                max(new.df$y.vals) / 
                dnorm(
                  x = 0,
                  mean = 0,
                  sd = alpha$old.SE
                )
            )
        }
        
      } else {
        # histogram
        if (alpha$bool2 & alpha$n > 0) {
          null.plot <- ggplot(alpha$df) +
            geom_histogram(
              aes(samples, fill = int.color), color = c.select,
              binwidth = alpha$old.SE / input$bin.width) +
            scale_fill_manual(values = levels(man.fill))
        } else {
          null.plot <- ggplot(alpha$df) +
            geom_histogram(
              aes(samples), color = c.select, fill = c.inner,
              binwidth = alpha$old.SE / input$bin.width)
        }
      }
      
      if ("Show Null Line" %in% input$lines) {
        null.plot <- null.plot +
          geom_vline(xintercept = 0, 
                     size = 3, color = c.inner, alpha = 0.5)
      }
      if ("Show Observed Line" %in% input$lines) {
        null.plot <- null.plot +
          geom_vline(xintercept = alpha$limit, 
                     size = 3, color = c.outer, alpha = 0.5)
        if (input$count == "further from null value than") {
          other.line = -alpha$limit
          null.plot <- null.plot + 
            geom_vline(xintercept = other.line, 
                       size = 3, color = c.outer, alpha = 0.5)
        }
      }
      
      dist <- 4 * alpha$old.SE
      xticks <- seq(
        0 - dist,
        0 + dist, 
        len = 9)
      
      null.plot +
        plaintheme + axistheme +
        labs(
          title = "Simulated Null Distribution", # <- CHNAGE TITLE HERE
          x = "Simulated difference of sample proportions", # <- CHANGE X LABEL HERE *************
          y = "Frequency") +
        scale_x_continuous(
          breaks = xticks,
          labels = signif(unique(xticks),3),
          limits = c(min(xticks), max(xticks)))
    }
  })
  
  output$results <- renderUI({
    
    df <- data.frame(alpha$samples)
    colnames(df) <- "samples"
    
    if (alpha$bool2) {
      if (input$count == "less than") {
        df <- df %>%
          mutate(int.color = ifelse(
            samples <= alpha$limit + 1 / (1.5 * alpha$data[3,3]),
            c.outer, c.inner))
      } else if (input$count == "greater than") {
        df <- df %>%
          mutate(int.color = ifelse(
            samples >= alpha$limit - 1 / (1.5 * alpha$data[3,3]),
            c.outer, c.inner))
      } else {
        abs.diff <- abs(alpha$limit) - 1 / (1.5 * alpha$data[3,3])
        alpha$abs.diff <- abs.diff
        df <- df %>%
          mutate(int.color = ifelse(
            samples <= -abs.diff |
              samples >= 0 + abs.diff,
            c.outer, c.inner))
      }
      
      alpha$n <- sum(df$int.color == c.outer)
      
      str0 <- paste("Estimated P-value =")
      str1 <- sprintf("%s/%s", alpha$n, alpha$len)
      str2 <- ifelse(alpha$bool1,
                     signif(alpha$n / alpha$len,4), 0)
      out.str <- paste(str0, str1, "=", str2)
    } else {
      df <- df %>%
        mutate(int.color = c.inner)
      
      alpha$n <- 0
      out.str <- paste("Enter a Value")
      paste("Enter a Value")
    } 
    
    alpha$df <- df
    HTML(paste("<b>",out.str,"</b>"))
    # paste(alpha$bool2)
  })
  
  
  output$debug <- renderTable({
    # # paste(alpha$len)
    # new.df <- SimpleDotPlot(alpha$df)
    # paste(alpha$len / max(new.df$y.vals))
    # paste(alpha$set)
    # paste(alpha$limit)
    # data.frame(table(alpha$samples))
    # paste(alpha$limit)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)




