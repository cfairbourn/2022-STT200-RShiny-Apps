
library(shiny)

fluidPage(
  # This is the code that allows for a person to select only a single checkbox
  # as well as unselect that box (functionality not given with radio buttons)
  tags$head(
    tags$script('
      $(document).ready(function() {
        // Function to handle checkbox clicks for each group
        function updateCheckBoxes(el) {
          var group = "input[name=\'" + $(el).attr("name") + "\']";
          $(group).not(el).prop("checked", false);
        }

        // Handle checkbox clicks for each group
        $(document).on("click", "input[type=\'checkbox\']", function() {
          updateCheckBoxes(this);
        });
      });
    ')
  ),
  
  # Green heading
  tags$head(
    tags$style(HTML('
      /* CSS to center the titlePanel */
      .center-title {
        display: flex;
        justify-content: center;
        color: green;
      }
    '))
  ),
  div(class = "center-title",
      titlePanel("Matching Correlation Game")),
  
  fluidRow(
    # spacing / margins / whatever
    column(3),
    column(
      6,
      # Input
      fluidRow(
        column(
          4,
          # Sample Size
          numericInput(
            "sample_size", "Sample Size",
            min = 0, max = 1000, step = 1, value = 50),
        ),
        column(
          4,
          # Making the buttons Green
          tags$head(
            tags$style(HTML('
              /* CSS to style the action buttons as green */
              .btn-green {
                background-color: green;
                color: white;
              }
            '))
          ),
          # Generate Plots and Submit Answer buttons
          actionButton("generate","New Plots", class = "btn-green"),
          actionButton("submit","Submit", class = "btn-green"),
        ),
        column(
          4,
          textOutput("score")
        )
      ),
      
      # Output
      fluidRow(
        # plot 1
        column(1, checkboxGroupInput("in1","", choices = rep(0,4))),
        column(5, plotOutput("out1")),
        # plot 2
        column(1, checkboxGroupInput("in2","", choices = rep(0,4))),
        column(5, plotOutput("out2"))
      ),
      fluidRow(
        # plot 3
        column(1, checkboxGroupInput("in3","", choices = rep(0,4))),
        column(5, plotOutput("out3")),
        # plot 4
        column(1, checkboxGroupInput("in4","", choices = rep(0,4))),
        column(5, plotOutput("out4"))
      ),
    ),
    # spacing / margins / whatever
    column(3)
  )
)