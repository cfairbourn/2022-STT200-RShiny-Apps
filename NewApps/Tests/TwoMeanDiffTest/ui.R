
################################
# Two Mean Difference Test App #
################################

library(shiny)
library(openintro)
library(rhandsontable)
library(data.table)
library(shinyhelper)

source("www/TwoMeanDiffSource.R")

ui <- fluidPage(
  titlePanel("Difference of Two Mean Randomization Test"),
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
          "Instructions",
          tags$div(
            class = "header", checked = NA,
            
            hr(),
            tags$p("In the 'Data' tab, first select the data underneath the Data Preset as instructed."),
            tags$p("Then select the column for the Numerical data and the column for the Categorical variable."),
            tags$p("Then select the two variables to subset the Numerical data into Group 1 and Group 2."),
            tags$p("From here, you can see the data displayed below, as well as it's mean,",
                   "standard deviation, and count on the right hand side."),
            hr(), # this is a line break
            tags$p("Next go to the 'Visuals' tab, where you will see a plot in the bottom right that",
                   "displays how the data chosen on the Data Tab is distributed."),
            tags$p("Return to the 'Controls' tab and choose the number of samples you'd like to generate,",
                   "and then press the Generate button."),
            tags$p("From here, select the type of test you are running, by choosing how you will count the samples.",
                   "And then choose the threshold from which the program should begin counting samples."),
            tags$p("The estimated p-value is displayed to the right of the box where you enter the threshold."),
            tags$p("If you'd like to visualize each individual sample that was generated,",
                   "press 'Show Selected Sample' and move the slider to be the desired sample you'd want to look at."),
            tags$p("From this same area, you can also allow the plot to display lines at",
                   "0 as well as the Observed Difference"),
            hr(),
            tags$p("Pressing the Reset button, as well as changing the data in any way,",
                   "will remove all samples and reset the graph"),
            hr(),
            tags$p("For further interation with the displays of the plot, advanced controls can be found",
                   "on the 'Controls' tab.")
          )
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        # data integration
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
                selected = "classdata"),
              
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
            ),
            
            # remaining columns
            column(
              4,
              conditionalPanel(
                condition = "input.preset != 'Custom'",
                
                selectInput(
                  "numerical", "Numerical",
                  choices = colnames(classdata),
                  selected = colnames(classdata)[1]),
              )
            ),
            
            column(
              4,
              conditionalPanel(
                condition = "input.preset != 'Custom'",
                
                selectInput(
                  "categorical", "Categorical",
                  choices = colnames(classdata),
                  selected = colnames(classdata)[2])
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
                  choices = unique(classdata$lecture),
                  selected = unique(classdata$lecture)[1]
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
                  choices = unique(classdata$lecture),
                  selected = unique(classdata$lecture)[2]
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
              htmlOutput("og_info1"),
              tags$div(hr()),
              htmlOutput("og_info2")
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