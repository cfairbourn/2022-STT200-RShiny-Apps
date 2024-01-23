# information
# form to function







library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(latex2exp)

source("www/SinglePropCI-source.R")

Input.Types <- c("Proportion","Counts")

Presets <- list()
# Presets['name'] <- (proportion, counts, sample.size)
Presets$`Medical Consultant` <- c(1, signif(3/62,3), 3, 62)
Presets$`Tappers and Listeners` <- c(1, signif(3/120,3), 3, 120)
Presets$`Outside Youtube Videos` <- c(1, signif(37/128,3), 37, 128)
Presets$`Twitter Users and News` <- c(2, 0.52, round(0.52*763), 763)
Presets$`Custom` <- c(1,.5,50,100) # <- Change Default Value

ui <- fluidPage(
  titlePanel("Bootstrap Confidence Interval for a Single Proportion"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Controls",
          # textOutput("debug"),
          selectInput("graph.type","Graph Type",
                      choices = c("Dotplot","Histogram")),
          selectInput("preset","Presets", choices = names(Presets), 
                      selected = "Custom"), # <- Change Default Value Here ****
          selectInput("input.type","Input Type",
                      choices = c("Proportion","Counts"),
                      selected = "Counts"), # <- Change Default Value Here 
          conditionalPanel(
            condition = "input['input.type'] == 'Proportion'",
            numericInput("p.succ","Proportion of Successes in Original Sample",
                         value = 0.5) # <- Change Default Value Here **********
          ),
          conditionalPanel(
            condition = "input['input.type'] == 'Counts'",
            numericInput("c.succ","Number of Successes in Original Sample",
                         value = 50) # <- Change Default Value Here **********
          ),
          numericInput("samp.size","Sample Size",
                       value = 100), # <- Change Default Value Here **********
          sliderInput("conf.level","Confidence Level",
                      min = .8, max = 1, step = 0.01,
                      value = 0.95), # <- Change Default Value Here **********
          numericInput("n.samp","Number of Simulations",
                       value = 1000), # <- Change Default Value Here **********
          actionButton("generate","Generate Bootstrap Proportions"),
          actionButton("reset","Reset"),
          checkboxInput("show.adv","Advanced Controls"),
          conditionalPanel(
            condition = "input['show.adv'] & output['show.samples']",
            sliderInput("bin.size","Bins Size",
                        min = 1, max = 5, step = 1,
                        value = 1), # <- Change Default Value Here **********
            sliderInput("dot.size","Dots Size",
                        min = 0.5, max = 4.5, step = 0.1,
                        value = 2) # <- Change Default Value Here **********
          )
        ),
        tabPanel(
          "Instructions",
          tags$div(
            class = "header", checked = NA,
            
            hr(),
            tags$p("Line of text"), # line of text
            hr(), # this is a line break
            tags$p("Another line after line break")
          )
        )
      )
    ),
    mainPanel(
      plotOutput("new.dist"),
      fluidRow(
        column(
          6,
          # htmlOutput("new.info"),
          checkboxInput("lines","Show Interval Boundaries"),
          textOutput("length"),
          textOutput("bounds"),
          tags$head(
            tags$style(
              "#bounds{font-size: 20px;
                   font-style: bold;
                   }"
            )
          ),
        ),
        column(
          6,
          conditionalPanel(
            condition = "output['show.samples']",
            checkboxInput("samp.pos", "Show Sample Position"),
            sliderInput("samp.select","Selected Sample",
                        min = 1, max = 1, step = 1,
                        value = 1), # <- Not a Default
          )
        )
      ),
      fluidRow(
        column(
          6,
          withMathJax(),
          uiOutput("og.samp"),
          plotOutput("og.dist", height = "200px")
        ),
        column(
          6,
          withMathJax(),
          uiOutput("samp.info"),
          plotOutput("samp.dist", height = "200px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  alpha <- reactiveValues()
  alpha$samples <- vector()
  
  output$show.samples <- reactive({
    alpha$bool <- length(alpha$samples) > 0
    return(alpha$bool)
  })
  # Allow for further interaction only after there is more than 0 data
  outputOptions(output, "show.samples", suspendWhenHidden = FALSE)
  
  observeEvent(c(input$reset, input$p.succ, input$c.succ, input$samp.size), {
    alpha$samples <- vector()
    alpha$bool <- F
    alpha$len <- 0
  })
  
  observeEvent(c(input$p.succ, input$samp.size, input$c.succ), {
    if(input$input.type == "Proportion") {
      updateNumericInput(session, "c.succ", 
                         value = round(input$p.succ * input$samp.size))
    } else {
      updateNumericInput(session, "p.succ", 
                         value = round(input$c.succ / input$samp.size, 4))
    }
  })
  
  
  observeEvent(input$preset, {
    # Determining which preset is selected
    i <- which(names(Presets) == input$preset)
    chosen.preset <- Presets[[i]]
    # Updating Input Type to be either Proportion or Counts
    updateSelectInput(
      session, "input.type", selected = Input.Types[chosen.preset[1]])
    # Updating Proportion of Success to be that preset's value
    updateNumericInput(
      session, "p.succ", value = chosen.preset[2])
    # Updating Number of Successes to be that preset's value
    updateNumericInput(
      session, "c.succ", value = chosen.preset[3])
    # Updating Sample Size to be that preset's value
    updateNumericInput(
      session, "samp.size", value = chosen.preset[4])
  })
  
  generate.samples <- eventReactive(input$generate, {
    
    new.samples <- rbinom(input$n.samp, input$samp.size, input$p.succ)
    new.samples <- new.samples / input$samp.size
    
    alpha$samples <- c(alpha$samples, new.samples)
    
    alpha$new.mean <- signif(mean(alpha$samples),4)
    alpha$new.stdErr <- signif(sd(alpha$samples),4)
    
    
    alpha$man.fills <- factor(c(c.outer, c.inner),
                              levels = c(c.outer, c.inner))
    alpha$man.color <- factor(c(c.outer, c.inner, c.select),
                              levels = c(c.outer, c.select, c.inner))
    
    alpha$len <- length(alpha$samples)
    
  })
  
  
  output$new.dist <- renderPlot({
    generate.samples()
    
    # Plots graph of newly generated samples
    if(alpha$bool) {
      samp.table <- table(alpha$samples)
      
      if(input$graph.type == "Dotplot") {
        # Plot Dotplot
        new.df <- SimpleDotPlot(alpha$df)
        new.plot <- ggplot(new.df)
        
        if(input$samp.pos) {
          # Change graph colors and dot sizes to highlight a sample's dot
          new.plot <- new.plot +
            geom_point(aes(x = x.vals, y = y.vals, 
                           color = int.color, size = selected))
        } else {
          new.plot <- new.plot +
            geom_point(aes(x = x.vals, y = y.vals, color = int.color),
                       size = input$dot.size)
        }
        
        new.plot <- new.plot +
          scale_y_continuous(
            limits = c(
              0, max(length(samp.table),
                     max(new.df$y.vals)*1.01))) +
          scale_color_manual(name = c.outer, values = levels(alpha$man.color))
        
      } else {
        # Plot Histogram
        new.plot <- ggplot(alpha$df) +
          geom_histogram(aes(x = samples, fill = int.color),color = c.select, 
                         binwidth = input$bin.size / input$samp.size)
        if(input$samp.pos) {
          # Display Selected Sample Mean as a line showing it's position
          new.plot <- new.plot +
            # Text showing the position
            geom_text(aes(x = alpha$samp.mean, y = 0,
                          label = signif(alpha$samp.mean, 4)),
                      color = c.select, size = 5,
                      hjust = -0.5, vjust = -2) +
            # Line position in Black
            geom_vline(xintercept = alpha$samp.mean, color = c.select,
                       size = 2, linetype = "longdash")
        }
      }
      if(input$lines) {
        # Plot Confidence Interval Boundary Lines
        new.plot <- new.plot +
          # Text showing the numbers
          geom_text(aes(x = alpha$lower, y = 0,
                        label = signif(alpha$lower, 4)),
                    color = c.select, size = 5,
                    hjust = -0.5, vjust = -18) +
          geom_text(aes(x = alpha$upper, y = 0,
                        label = signif(alpha$upper, 4)),
                    color = c.select, size = 5,
                    hjust = -0.5, vjust = -18) +
          
          # Line positions in Pink
          geom_vline(xintercept = alpha$lower, color = c.outer,
                     size = 2, linetype = "longdash", alpha = 0.5) +
          geom_vline(xintercept = alpha$upper, color = c.outer,
                     size = 2, linetype = "longdash", alpha = 0.5)
      }
      
      furthest <- max(abs(input$p.succ - alpha$samples))
      lo <- seq(input$p.succ, input$p.succ - furthest, by = -2/input$samp.size)
      hi <- seq(input$p.succ, input$p.succ + furthest, by = 2/input$samp.size)
      
      xticks <- unique(signif(c(lo, hi),2))
      
      new.plot + 
        scale_x_continuous(breaks = xticks) +
        labs(
          title = "Bootstrapped Proportions",
          x = "Generated Samples",
          y = "Frequency") +
        scale_fill_manual(name = c.outer, values = levels(alpha$man.fills)) +
        theme(legend.position = "none") +
        plaintheme + axistheme # +
      # geom_text(aes(x = min(alpha$samples), y = 0, 
      #               label = paste("Samples Generated:", alpha$len)),
      #           size = 5, vjust = -24, hjust = -0.0265)  +
      # geom_text(aes(x = min(alpha$samples), y = 0, 
      #               label = paste("Mean:", alpha$new.mean)),
      #           size = 5, vjust = -22, hjust = -0.05) +
      # geom_text(aes(x = min(alpha$samples), y = 0, 
      #               label = paste("SE:", alpha$new.stdErr)), 
      #           size = 5, vjust = -20, hjust = -0.05)
      
    }
  })
  
  # output$new.info <- renderUI({
  #   generate.samples()
  #   
  #   if(alpha$bool) {
  #     str1 <- paste("Mean:",alpha$new.mean)
  #     str2 <- paste("SD:", alpha$new.stdErr)
  #     str3 <- paste("Samples Generated",alpha$len)
  #     HTML(paste(str1, str2, str3, sep = "&nbsp;&nbsp;&nbsp;&nbsp;"))
  #   }
  # })
  
  output$bounds <- renderText({
    generate.samples()
    if (alpha$bool) {
      
      # bounds
      
      df <- data.frame(alpha$samples)
      colnames(df) <- "samples"
      
      mid <- .5 * (1 + input$conf.level)
      
      bounds <- df %>%
        summarise(
          lower = quantile(samples, probs = 1 - mid),
          upper = quantile(samples, probs = mid))
      
      alpha$lower <- signif(bounds$lower[1]*input$samp.size, 4)/input$samp.size
      alpha$upper <- signif(bounds$upper[1]*input$samp.size, 4)/input$samp.size
      
      # colors for out of confidence interval
      
      df <- df %>%
        mutate(int.color = ifelse(
          (samples <= bounds$lower | samples >= bounds$upper),
          c.outer, c.inner
        ))
      
      # distinction for which sample is selected
      
      updateSliderInput(session, "samp.select", max = alpha$len)
      
      alpha$df <- df
      
      perc <- paste(100 * input$conf.level, "%", sep = "")
      out.str <- paste(
        perc,
        sprintf(
          "Confidence Interval = (%s, %s)",
          signif(alpha$lower, 4),
          signif(alpha$upper, 4)
        )
      )
      paste(out.str)
    } else {
      paste("Press Generate")
    }
  })
  
  output$og.samp <- renderUI({
    if(!(is.na(input$c.succ) & is.na(input$p.succ))) {
      withMathJax(
        paste0(
          "Original Sample:", "$$\\hat{p} =", 
          signif(input$p.succ, 4), "$$"
        )
      )
    }
  })
  
  output$og.dist <- renderPlot({
    if(!(is.na(input$c.succ) & is.na(input$p.succ))) {
      OGSampPlotMaker(input$c.succ, input$samp.size)
    }
  })
  
  output$samp.info <- renderUI({
    if (alpha$bool) {
      withMathJax(
        paste0(
          "Selected Sample:", 
          "$$\\hat{p}_{\\small{\\textrm{sim}}} =", 
          signif(alpha$samp.mean, 4), "$$"
        )
      )
    } 
  })
  
  output$samp.dist <- renderPlot({
    generate.samples()
    
    df <- alpha$df %>%
      mutate(selected = 1)
    
    df[-input$samp.select, "selected"] <- 1
    df[input$samp.select, "selected"] <- 2
    
    alpha$samp.mean <- alpha$df[input$samp.select,"samples"]
    alpha$df <- df
    
    if(alpha$bool) {
      sampleToDF(alpha$samp.mean * input$samp.size, input$samp.size)
    }
    # Shows the selected sample as a figure of dots, green success, white fail
  })
  
  output$length <- renderText({
    paste("Samples Generated:", alpha$len)
  })
  
  output$debug <- renderText({
    # paste(alpha$p.hat)
  })
}

shinyApp(ui, server)