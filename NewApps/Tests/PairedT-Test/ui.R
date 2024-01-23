
########################
# Paired T Test App UI #
########################

library(shiny)
library(openintro)
library(rhandsontable)
library(data.table)
library(shinyhelper)

source("www/PairedSource.R")

# Define UI for application that draws a histogram
fluidPage(

  titlePanel("Paired T Test"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Controls",
          # tableOutput("debug"),
          numericInput(
            "n_samples", "Number of Samples",
            value = 2000), 
          
          # button
          actionButton("generate", "Generate"),
          actionButton("reset","Reset"),
          
          # plot choice 
          selectInput(
            "plot_type","Plot Type",
            choices = c("Dotplot", "Histogram"),
            selected = "Dotplot"),
          
          # slider input for confidence level
          # sliderInput("conf_level", "Confidence Level",
          #             value = 0.95, 
          #             min = 0.5, max = 0.99, step = 0.01),
          checkboxInput("advanced_controls", "Show Advanced Controls")  ,
          conditionalPanel(
            condition = "input.advanced_controls & output.show_samples",
            # dotplot
            sliderInput(
              "n_cols", "Number of Columns",
              min = 2, max = 5, step = 1, value = 4),
            sliderInput(
              "x_ticks","Ticks Intervals",
              min = 1, max = 15, step = 1, value = 5),
            tags$div(
              class = "header", checked = NA,
              tags$p("Warning: Performance issues may occur with higher values")
            ),
            sliderInput(
              "n_bins","Number of Bins (Scalar)",
              min = 1, max = 4, step = 0.1, value = 1)
          )
        ),
        tabPanel(
          "Instructions"
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data",
          h4("Data Integration"),
          
          fluidRow(
            # left column
            column(
              4, 
              # preset
              selectInput(
                "preset","Data Preset",
                choices = Presets,
                selected = "UCLA_Books"),
              
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
              )
            )
          ),
          
          # second row
          fluidRow(
            # left column
            column(
              4,
              conditionalPanel(
                condition = "input.preset != 'Custom'",
                selectInput(
                  "group1", "Group 1",
                  choices = colnames(UCLA_Books),
                  selected = colnames(UCLA_Books)[1]
                ),
                tableOutput("out_data1")
              ),
              
              conditionalPanel(
                condition = "input.preset == 'Custom'",
                numericInput(
                  "num_rows1", "Number of Rows",
                  value = 5, min = 1, max = 100),
                actionButton("add_rows1","Add"),
                actionButton("sub_rows1","Subtract"),
                rHandsontableOutput("c_table1")
              )
            ),
            
            # middle column
            column(
              4,
              conditionalPanel(
                condition = "input.preset != 'Custom'",
                selectInput(
                  "group2", "Group 2",
                  choices = colnames(UCLA_Books),
                  selected = colnames(UCLA_Books)[2]
                ),
                
                tableOutput("out_data2")
              ),
              
              conditionalPanel(
                condition = "input.preset == 'Custom'",
                numericInput(
                  "num_rows2", "Number of Rows",
                  value = 5, min = 1, max = 100),
                actionButton("add_rows2","Add"),
                actionButton("sub_rows2","Subtract"),
                rHandsontableOutput("c_table2")
              )
            ),
            column(
              4,
              tags$div(hr()),
              htmlOutput("og_info0"),
              tags$div(hr()),
              htmlOutput("og_info1"),
              tags$div(hr()),
              htmlOutput("og_info2"),
              tags$div(hr()),
              htmlOutput("og_info3")
            )
          )
        ),
        tabPanel(
          "Visuals",
          
          fluidRow(
            plotOutput("distribution")
          ),
          
          # test
          fluidRow(
            #  text
            column(
              3,
              style = "margin-top: 25px; text-align: right;",
              tags$div(
                class = "header", checked = NA,
                tags$p("Count Proportions of Simulations"), 
              )
            ),
            
            # count what kind of samples
            column(
              3,
              selectInput(
                "count_type","",
                choices = c("further from null value than",
                            "greater than","less than"),
                selected = "further from null value than")
            ),
            
            # limit 
            column(
              3,
              numericInput("limit", "", "", step = 0.1)
            ),
            
            # returning output
            column(
              3,
              style = "margin-top: 25px;",
              htmlOutput("results"),
              tags$head(
                tags$style(
                  "#results{color: red; font-style: bold;}"
                )
              )
            )
          ),
          
          # data display
          fluidRow(
            column(
              8,
              checkboxGroupInput(
                "lines","", inline = T,
                choices = c(
                  "Show Null Line", 
                  "Show Observed Line",
                  "Show Selected Sample"#, 
                  # "Overlay Normal Curve"
                )),
              fluidRow(
                column(
                  6,
                  textOutput("observed_diff"),
                  tags$head(
                    tags$style("#observed_diff{font-size: 14px;}")
                  )
                ),
                column(
                  6,
                  sliderInput(
                    "selected_sample","Select Sample",
                    min = 1, max = 1, step = 1, value = 1),
                  textOutput("selected_sample_mean")
                ),
              )
            ),
            column(
              4,
              # checkboxGroupInput(
              #   "visibility", "", choices = c("Group 1","Group 2"),
              #   inline = T, selected = c("Group 1","Group 2"),
              # ),
              plotOutput("data_visuals", height = "275px")
            )
          )
        )
      )
    )
  )
  
)
