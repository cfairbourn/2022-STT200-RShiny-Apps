# ------------------------------------------------------------------------------
# File: SinglePropTest2-app.R
# Authors: Camille Fairbourn, Colin Fairbourn, Scott Manski
# Date: 08/22/2022 
# Desc: This app performs a bootstrap test for a single proportion.
# Published Location: https://shiny.stt.msu.edu/fairbour/SinglePropTest/
# Email: fairbour@msu.edu, fairbou2@msu.edu, manskisc@msu.edu
#
# For questions or concerns, please email the authors. This work is licensed 
# under a Creative Commons Attribution-ShareAlike 4.0 International License 
# (https://creativecommons.org/licenses/by-sa/4.0/).
# ------------------------------------------------------------------------------


library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(latex2exp)

source("www/SinglePropTest2-source.R")

Input.Types <- c("Proportion","Counts")

Presets <- list()
# Presets$`name` <- (proportion, sample.size)
Presets$`Medical Consultant` <- c(signif(3/62,3), 62)
Presets$`Tappers and Listeners` <- c(signif(3/120,3), 120)
Presets$`Outside Youtube Videos` <- c(signif(37/128,3), 128)
Presets$`Twitter Users and News` <- c(0.52, 763)
Presets$`Custom` <- c(.5,100) # <- Change Default Value

ui <- fluidPage(
  titlePanel("Bootstrap Test for a Single Proportion"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Controls",
          
          selectInput("graph.type","Graph Type",
                      choices = c("Dotplot","Histogram")),
          selectInput("preset","Presets", choices = names(Presets), 
                      selected = "Custom"), # <- Change Default Value Here *****
          numericInput("p.succ",
                       "Null Parameter Value for Population Proportion",
                       value = 0.5), # <- Change Default Value Here **********
          numericInput("samp.size","Sample Size",
                       value = 100), # <- Change Default Value Here **********
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
          tags$div(
            class = "header", checked = NA,
            tags$p("To conduct a hypothesis test for a single proportion using the bootstrap method, do the following:"),
            hr(),
            tags$p("1. Enter the null parameter value for the proportion as a number between 0 and 1."), # line of text
            tags$p("2. Enter the sample size from the observed sample."),
            tags$p("3. Change the Number of Simulations, if desired. 
                   The maximum allowed value is 5000."),
            tags$p("4. Enter the observed proportion into the textbox next to Enter a Value."),
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
            tags$a(href="https://shiny.stt.msu.edu/fairbour/Archive/OneProportion/",
                   "Archived One Proportion Test"),
            hr(),
          )
        )
      )
    ),
    mainPanel(
      plotOutput("null.dist"),
      checkboxGroupInput(
        "lines", "", inline = T, choices = c(
          "Overlay Normal Curve","Show Null Line","Show Observed Line")),
      textOutput("len.samps"),
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
          checkboxInput(
            "show.theo","Show Theoretical Null Distribution Information")
        ),
        column(
          6, 
          conditionalPanel(
            condition = "input['show.theo']",
            htmlOutput("theo.info")
          )
        )
      ),
      fluidRow(
        column(
          6,
          checkboxInput(
            "show.info","Show Simulated Null Distribution Information")
        ),
        column(
          6,
          style = "margin-top: 10px;",
          conditionalPanel(
            condition = "input['show.info']",
            htmlOutput("null.info")
          ),
          
        )
      ),
      
      textOutput("debug")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  alpha <- reactiveValues()
  alpha$samples <- vector()
  alpha$bool1 <- F
  alpha$bool2 <- F
  
  observeEvent(input$generate, {
    
    new.samples <- rbinom(
      n = as.integer(input$n.samp),
      size = input$samp.size,
      prob = input$p.succ)
    
    alpha$samples <- c(alpha$samples, new.samples)
    alpha$len <- length(alpha$samples)
    alpha$bool1 <- T
  })
  
  observeEvent(c(input$reset, input$p.succ, input$samp.size), {
    alpha$samples <- vector()
    alpha$bool1 <- F
    alpha$len <- 0
    
    alpha$old.SE <- sqrt((input$p.succ * (1- input$p.succ))/
                           input$samp.size)
  })
  
  observeEvent(c(input$limit, input$samp.size), {
    alpha$bool2 <- !is.na(input$limit)
    if (alpha$bool2) {
      alpha$limit <- round(input$limit * input$samp.size)
    }
  })
  
  output$len.samps <- renderText({
    paste("Samples Generated:", alpha$len)
  })
  
  output$null.info <- renderUI({
    if (alpha$bool1) {
      alpha$null.mean <- signif(mean(alpha$samples),3)
      alpha$null.SE <- signif(sd(alpha$samples),4)
      
      str1 <- paste("Mean:", alpha$null.mean / input$samp.size)
      str2 <- paste("SE:", alpha$null.SE / input$samp.size)
      HTML(paste(str1, str2, sep = "&nbsp;&nbsp;&nbsp;"))
    } else {
      HTML(paste("Press Generate"))
    }
  })
  
  output$theo.info <- renderUI({
    if (alpha$bool1) {
      alpha$theo.mean <- signif(input$p.succ, 4)
      theo.se <- sqrt(input$p.succ * (1-input$p.succ) / input$samp.size )
      alpha$theo.SE <- signif(theo.se,4)
      
      str1 <- paste("Mean:", alpha$theo.mean)
      str2 <- paste("SE:", alpha$theo.SE)
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
                x, mean = input$p.succ, sd = alpha$old.SE) *
                max(new.df$y.vals) / 
                dnorm(
                  x = input$p.succ,
                  mean = input$p.succ,
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
          geom_vline(xintercept = input$p.succ, 
                     size = 3, color = c.inner, alpha = 0.5)
      }
      if ("Show Observed Line" %in% input$lines) {
        null.plot <- null.plot +
          geom_vline(xintercept = alpha$limit / input$samp.size, 
                     size = 3, color = c.outer, alpha = 0.5)
        if (input$count == "further from null value than") {
          other.line = 2 * input$p.succ - alpha$limit
          null.plot <- null.plot + 
            geom_vline(xintercept = other.line, 
                       size = 3, color = c.outer, alpha = 0.5)
        }
      }
      
      dist <- 4 * alpha$old.SE
      xticks <- seq(
        input$p.succ - dist,
        input$p.succ + dist, 
        len = 9)
      
      null.plot +
        plaintheme + axistheme +
        labs(
          title = "Simulated Null Distribution", # <- CHANGE TITLE HERE* ***********
          x = "Simulated sample proportions", # <- CHANGE X LABEL HERE **************
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
            samples <= alpha$limit,
            c.outer, c.inner))
      } else if (input$count == "greater than") {
        df <- df %>%
          mutate(int.color = ifelse(
            samples >= alpha$limit,
            c.outer, c.inner))
      } else {
        abs.diff <- abs(input$p.succ - alpha$limit / input$samp.size)
        # alpha$abs.diff <- abs.diff
        df <- df %>%
          mutate(int.color = ifelse(
            samples <= round(input$samp.size * (input$p.succ - abs.diff)) |
              samples >= round(input$samp.size * (input$p.succ + abs.diff)),
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
    
    alpha$df <- df %>% 
      mutate(samples = samples / input$samp.size)
    HTML(paste("<b>",out.str,"</b>"))
    # paste(alpha$bool2)
  })
  
  
  output$debug <- renderText({
    # # paste(alpha$len)
    # new.df <- SimpleDotPlot(alpha$df)
    # paste(alpha$len / max(new.df$y.vals))
    # paste(alpha$limit, alpha$abs.diff)
    # paste(sum(alpha$samples >= (input$p.succ - alpha$abs.diff)))
    # paste(alpha$df['samples'])
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)





