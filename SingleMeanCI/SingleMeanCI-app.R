# ------------------------------------------------------------------------------
# File: 
# Authors: Camille Fairbourn, Colin Fairbourn, Scott Manski, Harrison Adams
# Date: 07/19/2022 
# Desc: 
# Published Location: 
# Email: fairbour@msu.edu, fairbou2@msu.edu, manskisc@msu.edu, adamshar@msu.edu
#
# For questions or concerns, please email the authors. This work is licensed 
# under a Creative Commons Attribution-ShareAlike 4.0 International License 
# (https://creativecommons.org/licenses/by-sa/4.0/).
# ------------------------------------------------------------------------------

# Format into Functionality
# help from https://tinyurl.com/bddrnpn5

# written by Colin Fairbourn, Harrison Adams, with snippets from Scott Manski


library(shiny)
library(ggplot2)
library(dplyr)
library(openintro)
library(rhandsontable)
library(data.table)
library(shinyhelper)


source("www/SingleMeanCI-source.R")

X <- rep(0, 10)
dataX <- data.table(X)

Presets <- list("Custom","Upload","mtcars","iris","chickwts","births14","wages","roadsign")
DataPresets <- list(mtcars, iris, chickwts, as.data.frame(births14), 
                    data.frame("wages" = c(21.25,	19.00,	26.50,	23.45,	36.35,	26.45)),
                    data.frame("roadsign" = c(440, 490, 600, 540, 540, 600, 380, 440, 
                                              360, 600, 490, 400, 490, 540, 440, 490)))
DataStrings <- Presets[3:length(Presets)]


ui <- fluidPage(
  titlePanel("Bootstrap Confidence Interval for a Single Mean"),
  sidebarLayout(
    
    # Sidebar
    sidebarPanel(
      tabsetPanel(
        
        # Controls
        tabPanel(
          "Controls",
          # textOutput("debug"),
          numericInput("n.samples","Number of Samples", 
                       value = 500), # <- Change Default Here ****************
          
          # Buttons
          actionButton("generate","Generate "),
          actionButton("reset","Reset"),
          # Plot choice and confidence level interaction
          selectInput("plot","Plot Type",
                      choices = c("Dotplot","Histogram"),
                      selected = "Dotplot"), # <- Change Default Here *****
          sliderInput("confLvl", "Confidence Level", 
                      value = 0.95, # <- Change Default Here ****************
                      min = .5, max = .99, step = .01),
          
          # Advanced Controls
          checkboxInput("advanced","Show Advanced Controls"),
          conditionalPanel(
            condition = "input.advanced & output.showSamples",
            # Dotplot
            sliderInput("numCols","Number of Columns",
                        value = 4, # <- Change Default Here *****************
                        min = 2, max = 5, step = 1),
            sliderInput("dot.size", "Point Size",
                        value = 2.5, # <- Change Default Here *************
                        min = 0.5, max = 7.5, step = 0.1),
            checkboxInput("show.dist","Show Distribution Only"),
            
            tags$div(
              class = "header", checked = NA,
              
              tags$p("Warning: Performance issues occur with higher values")
            ),
            sliderInput("numBins", "Number of Bins (Scalar)", 
                        value = 1, # <- Change Default Here *****************
                        min = 1, max = 4, step = 0.1),
          )
        ),
        
        # Instructions
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
    
    # Main Panel
    mainPanel(
      tabsetPanel(
        
        # Visualization of Data
        tabPanel(
          "Visuals",
          # Plot Generated Samples
          plotOutput("new.dist"),
          # Row, 1st column is interaction, 2nd is OG Data Distribution 
          fluidRow(
            column(
              6,
              # Mean, SD, Samples Generated 
              htmlOutput("newInfo"),
              # Confidence Interval Boundaries
              textOutput("bounds"),
              tags$head(
                tags$style(
                  "#bounds{font-size: 20px;
                   font-style: bold;
                   }"
                )
              ),
              # Row of checkboxes, Conf.Int boundary & Select.Samp's mean lines
              fluidRow(
                column(
                  3,
                  # Allow Boundary Lines to be visible on plot above
                  checkboxInput("lines", "Show Interval Boundaries")
                ),
                column(
                  3,
                  # Shows up only if samples have been generated
                  conditionalPanel(
                    "output.showSamples",
                    # Allow Selected Sample mean to be visible on plot above
                    checkboxInput("sample.pos", "Show Sample Position")
                  )
                )
              ),
              # show lines
              conditionalPanel(
                "output.showSamples",
                # Allows for the selection of a specific sample
                sliderInput("sample.num", "Select Sample", 
                            min = 1, max = 1, value = 1, step = 1),
                # Display selected sample's mean
                textOutput("sample.info")
              )
            ),
            column(
              6,
              # Original Distribution of Data
              # htmlOutput("ogInfo"),
              plotOutput("og.dist")
            )
          )
        ),
        
        # Data Integration, Selection, and Interaction
        tabPanel(
          "Data",
          h4("Data Integration, Selection, and Interaction"),
          # Row of Preset, Column (only for specific presets) selections 
          fluidRow(
            column(
              6,
              selectInput("preset","Choose Data",
                          choices = Presets, 
                          selected = "mtcars"), # <- Change Default Here ******
              # Custom Dataset, input own data
              conditionalPanel(
                condition = "input.preset == 'Custom'",
                rHandsontableOutput(outputId = "c.table"),
              ),
              
              # Upload Dataset from computer
              conditionalPanel(
                condition = "input.preset == 'Upload'",
                helper(
                  fileInput(
                    "file", "Choose CSV File",
                    multiple = FALSE,
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
                  icon = "question-circle",
                  colour = NULL,
                  type = "inline",
                  title = "Choose Your Own Data",
                  content = c(
                    "Choose a csv file to upload.",
                    "The first row of the csv should be a title.",
                    "All subsequent rows must be numeric."
                  )
                )
              ),
              conditionalPanel(
                condition = "input.preset != 'Custom'",
                tableOutput("outData")
              ),
            ),
            column(
              6,
              conditionalPanel(
                condition = "input.preset != 'Custom'",
                
                selectInput("column","Choose Column", 
                            choices = colnames(mtcars),
                            selected = colnames(mtcars)[1]) # <- Not a Default
              ),
              # Custom Data Interaction (Adding Rows)
              conditionalPanel(
                condition = "
                input.preset == 'Custom'",
                fluidRow(
                  column(
                    5,
                    # numerical input to add rows
                    numericInput("numRows", "Additional Rows",
                                 value = 5, # <- Change Default Here **********
                                 min = 1, max = 100)
                  ),
                  column(
                    1,
                    actionButton("addRows","Add Rows"),
                    actionButton("subRows","Subtract Rows")
                  )
                )
              ),
              # Display Mean and SD of original Data
              htmlOutput("ogInfo"),
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Finished Code
  # Notes on what variable is what
  # alpha           original reactive values
  # alpha$data      original dataset
  # alpha$samples   the generated sample means
  # alpha$df        alpha$samples as dataframe with colors based on confLvl
  # alpha$upload    uploaded data with only numeric columns
  # gamma           Reactive used for custom table
  # gamma$custom    custom handsontable as r object (list)
  
  # Set up Reactive Values
  alpha <- reactiveValues()
  alpha$samples <- vector()
  
  # If preset or inputed data changes, reset generated data
  observeEvent(
    c(input$reset,input$preset, input$column, alpha$data), {
    alpha$samples <- vector()
  })
  
  # Generate samples when a button is pressed
  generate.samples <- eventReactive(input$generate, {
    if(is.integer(input$n.samples) & input$n.samples > 0) {
      new.means <- replicate(input$n.samples, 
                             mean(sample(alpha$data, replace = T)))
      alpha$samples <- c(alpha$samples, new.means)
    } else {
      new.mean <- mean(sample(alpha$data, replace = T))
      alpha$samples <- c(alpha$samples, new.mean)
    }
  })
  
  # show selectInput for 'Sample Number' but only when there are samples
  output$showSamples <- reactive({
    if (length(alpha$samples) > 0) {
      beta <- TRUE
      # update the slider input for selecting the sample
      updateSliderInput(session, "sample.num", max = length(alpha$samples))
    } else {
      beta <- FALSE
    }
    return(beta)
  })
  # Allow for further interaction only after there is more than 0 data
  outputOptions(output, "showSamples", suspendWhenHidden = FALSE)
  
  
  
  # Graphing
  
  # generate plot of generated samples
  output$new.dist <- renderPlot({
    generate.samples()
    
    # only display if there are samples generated
    if(length(alpha$samples) > 0) {
      # used for colors
      manual.fills <- factor(c(c.outer, c.inner), 
                              levels = c(c.outer, c.inner))
      manual.colors <- factor(c(c.outer, c.inner, c.select),
                              levels = c(c.outer, c.select, c.inner))
      
      # used for number of bins dependent on number of unique sample means
      bin.num <- ceiling(input$numBins * length(table(alpha$samples))^0.5)
      
      
      if(input$plot == "Dotplot"){
        
        newdf <- ColinDotPlot(alpha$df, bin.num, input$numCols)
        new.plot <- ggplot(newdf)
        
        if(input$sample.pos) {
          # Calling my Dotplot Dataframe creation function, located in source
          new.plot <- new.plot +
            geom_point(aes(xVals, yVals, color = int.color, 
                           size = selected)) # difference, increase dot size
        } else {
          # Calling my Dotplot Dataframe creation function, located in source
          new.plot <- new.plot +
            geom_point(aes(xVals, yVals, color = int.color))
        }
        new.plot <- new.plot  +
          scale_y_continuous(
            limits = c(0, max(bin.num*0.75,max(newdf$yVals)*1.01)))
        
      } else {
        # plotting histogram
        
        new.plot <- ggplot(alpha$df) +
          geom_histogram(aes(x = samples, fill = int.color),
                         bins = bin.num, color = c.select)
        if(input$sample.pos) {
          # Display Selected Sample Mean as a line showing it's position
          new.plot <- new.plot +
            # Text showing the position
            geom_text(aes(x = alpha$selectSamp, y = 0, 
                          label = signif(alpha$selectSamp, 4)), 
                      color = c.select, size = 5, 
                      hjust = -0.5, vjust = -2) +
            # Line position in Black
            geom_vline(xintercept = alpha$selectSamp, color = c.select, 
                       size = 2, linetype = "longdash")
        } 
      }
      if(input$lines) {
        # Display Boundary Lines
        new.plot <- new.plot + 
          # Text showing the numbers
          geom_text(aes(x = alpha$lower, y = 0, 
                        label = signif(alpha$lower, 4)), 
                    color = c.select, size = 5, 
                    hjust = -0.5, vjust = -21) +
          geom_text(aes(x = alpha$upper, y = 0, 
                        label = signif(alpha$upper, 4)), 
                    color = c.select, size = 5, 
                    hjust = -0.5, vjust = -21) +
          
          # Line positions in Pink
          geom_vline(xintercept = alpha$lower, color = c.outer, 
                     size = 2, linetype = "longdash") +
          geom_vline(xintercept = alpha$upper, color = c.outer, 
                     size = 2, linetype = "longdash")
      }
      
      if (input$show.dist) {
        new.plot <- new.plot +
          scale_color_manual(name = "", values = c.inner)
      } else {
        new.plot <- new.plot +
          scale_color_manual(name = "", values = levels(manual.colors))
          
      }
      
      new.plot +  # Display the plot with some labels and more xticks
        scale_x_continuous(
          breaks = signif(
            seq(min(alpha$samples),
                max(alpha$samples),
                len = 10),3)) +
        labs(title = "Resampled Means",
             x = "Generated Means", 
             y = "Frequency") +
        scale_fill_manual(name = c.outer, values = levels(manual.fills)) + 
        theme(legend.position = "none") +
        plaintheme + axistheme
        
    }
  })
  
  output$og.dist <- renderPlot({
    # Generate histogram of original data
    df <- alpha$data
    ogDF <- data.frame(df)
    colnames(ogDF) <- "samples"
    
    if(input$plot == "Dotplot") {
      bin.num <- ceiling(input$numBins * length(table(alpha$data))^0.5)
      ogDF <- ColinDotPlot(ogDF, bin.num, input$numCols)
      ogPlot <- ggplot(ogDF) +
        geom_point(aes(xVals, yVals),
                   fill = c.inner, size = input$dot.size) +
        scale_y_continuous(
          limits = c(0, max(bin.num*0.75,max(ogDF$yVals)*1.01)))
    } else {
      ogPlot <- ggplot() +
        geom_histogram(aes(df), fill = c.inner, color = c.select)
    }
    ogPlot <- ogPlot +
      labs(
        title = "Original Sample",
        x = "Original Data",
        y = "Frequency")+
      plaintheme + axistheme +
      scale_x_continuous(
        breaks = signif(
          seq(min(alpha$data),
              max(alpha$data),
              len = 7),3))
    ogPlot
  })
  
  
  
  # DF Creation
  
  output$sample.info <- renderText({
    generate.samples()
    # Calculate selected sample
    if(length(alpha$samples) > 0) {
      # Save it for further use
      alpha$selectSamp <- signif(alpha$samples[input$sample.num], 4)
      # set the color value for selected to something else
      # reset the df back to normal
      alpha$df[-input$sample.num, "selected"] <- 1
      alpha$df[input$sample.num, "selected"] <- 2
      # Display it as a text
      paste("Mean:", alpha$selectSamp)
    } 
  })
  
  output$bounds <- renderText({
    generate.samples()
    # Calculate confidence interval boundaries of generated samples
    
    # Only do so if samples have been generated
    if(length(alpha$samples) > 0) {
      # Turn the generated samples into a data.frame that's easier to work with
      samples <- alpha$samples
      samples <- as.data.frame(samples)
      colnames(samples) <- "samples"
      df <- data.frame(samples)
      
      # **********************************
      # Calculating Confidence Interval
      # **********************************
      
      
      # calculate boundaries
      mid <- .5 * (1 + input$confLvl)
      bounds <- df %>%
        summarise(
          lower = quantile(samples, probs = 1-mid),
          upper = quantile(samples, probs = mid)
        )
      
      
      # **********************************
      # Calculating Confidence Interval
      # **********************************
      
      # Save the boundaries for further use
      alpha$lower <- round(bounds$lower, 4)
      alpha$upper <- round(bounds$upper, 4)
      
      # need to create a vector of colors for confidence interval
      df <- df %>%
        mutate(
          int.color = ifelse(
            samples <= bounds$lower | samples >= bounds$upper,
            c.outer, c.inner),
        ) %>%
        mutate(
          selected = 1
        )
      
      if (input$show.dist) {
        df <- df %>%
          mutate(int.color = c.inner)
      }
      
      # save the dataframe for further use
      alpha$df <- df
      
      # Text display of the Confidence Interval
      lvl <- paste(100*input$confLvl, "%", sep = "")
      out.str <- sprintf(
        "Confidence Interval = (%s, %s)",
        round(alpha$lower, 4),round(alpha$upper, 4)
      )
      paste(lvl, out.str)
    }
  })
  
  
  
  # Information and Dataframe displays
  
  output$newInfo <- renderUI({
    # Calculate and display mean and sd only if samples have been generated
    if(length(alpha$samples) > 0) {
      alpha$newMean <- signif(mean(alpha$samples),3)
      alpha$newSD <- signif(sd(alpha$samples),4)
      str1 <- paste("Mean:", round(alpha$newMean, 3))
      str2 <- paste("SD:", round(alpha$newSD, 4))
      # Also display the total number of samples generated
      str3 <- paste("Samples Generated:", length(alpha$samples))
      HTML(paste(str1, str2, str3,
                 sep = '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'))
    } else {
      # If samples haven't been generated, tell use to generate for progression
      str1 <- paste("Press Generate")
      HTML(paste(str1))
    }
  })
  
  output$ogInfo <- renderUI({
    # Calculate mean and sd of original data
    alpha$ogMean <- mean(alpha$data)
    alpha$ogSD <- sd(alpha$data)
    # Display text with a line break
    str1 <- paste("Mean:", round(alpha$ogMean, 3))
    str2 <- paste("SD:", round(alpha$ogSD, 4))
    str3 <- paste("Count:", length(alpha$data))
    HTML(paste(str1, str2, str3,
               sep = "<br>"))
  })
  
  # Render and display table 
  output$outData <- renderTable({
    alpha$data
  })
  
  
  
  # Altering, storing, and chosing original data
  # if preset is changed, update columns selection
  observeEvent(input$preset, {
    # if preset is changed to a pregiven dataframe, update columns
    if (input$preset %in% DataStrings) {
      # get index of DataStrings that input has selected
      i <- which(DataStrings == input$preset) 
      chosen.df <- DataPresets[[i]]
      col.choices <- colnames(select_if(chosen.df, is.numeric))
      # update columns based on that dataframe
      updateSelectInput(
        session, "column", 
        choices = col.choices,
        selected = col.choices[1])
    }
  })
  
  
  # if column is changed, and preset is a given dataframe, update data
  observeEvent(c(input$preset, input$column), {
    # if preset is changed to a data preset, use column to change alpha$data
    if (input$preset %in% DataStrings) {
      # get index of DataStrings that input has selected
      i <- which(DataStrings == input$preset) 
      chosen.df <- DataPresets[[i]]
      cols <- colnames(chosen.df)
      j <- which(cols == input$column)
      
      chosen.vec <- chosen.df[,j]
      chosen.vec <- chosen.vec[!is.na(chosen.vec)]
      
      alpha$data <- chosen.vec
    } else if (input$preset == "Upload") {
      # alpha$upload is uploaded data with only numeric columns
      chosen.df <- alpha$upload
      cols <- colnames(chosen.df)
      j <- which(cols == input$column)
      
      chosen.vec <- chosen.df[,j]
      chosen.vec <- chosen.vec[!is.na(chosen.vec)]
      
      alpha$data <- chosen.vec
    }
  })
  
  # sets up data based on preset chosen, or waits for input$file
  # reads input$file
  observeEvent(input$file, {
    if (!is.null(input$file)) {
      alpha$upload <- read.csv(input$file$datapath, stringsAsFactors=FALSE)
      alpha$upload <- select_if(alpha$upload, is.numeric)
      updateSelectInput(session, "column", choices = names(alpha$upload))
    }
  })
  
  
  
  # Custom Data Integration
  
  gamma <- reactiveValues(custom = dataX)
  
  output$c.table <- renderRHandsontable({
    rhandsontable(gamma$custom) # converts list to editable table
  })
  
  observeEvent(input$c.table, {
    gamma$custom <- hot_to_r(input$c.table) # converts to a list
    if(input$preset == "Custom") {
      alpha$custom <- data.frame(gamma$custom)
      alpha$data <- alpha$custom[,1]
    }
  })
  
  # Adding Rows to the Custom Table
  observeEvent(input$addRows, {
    if(!is.na(input$numRows)) {
      x <- gamma$custom[[1]] # save current table as double (vector)
      n <- ceiling(input$numRows)
      y <- rep(0, n) # get position of last row to be added
      x <- c(x,y) # set value at pos i to 1, all values in between are NA
      gamma$custom <- data.table(x) # update gamma$custom
    }
  })
  
  # Subtracting Rows to the Custom Table
  observeEvent(input$subRows, {
    if(!is.na(input$numRows)) {
      x <- gamma$custom[[1]]# save current table as double (vector)
      m <- length(x) # get total current length of table
      n <- ceiling(input$numRows) # get number of rows wished to be subtracted
      i <- m-n # get new length of table
      x <- x[1:i] # set x = to itself up to new length
      gamma$custom <- data.table(x) # update gamma$custom
    }
  })
  
  
  
  
  
  
  
  
  
  # For Debugging
  
  # output$debug <- renderText({
  #   typeof(alpha$data)
  # })
  

}


shinyApp(ui = ui, server = server)
