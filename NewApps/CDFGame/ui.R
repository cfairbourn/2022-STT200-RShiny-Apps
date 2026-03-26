# Developed by Colin Fairbourn 2025 07 31
library(shiny)

fluidPage(
  withMathJax(),
  # This is the code that allows for a person to select only a single checkbox
  # as well as unselect that box (functionality not given with radio buttons)
  tags$head(
    tags$script(HTML(
      '
      $(document).ready(function() {
        // Only apply this to checkboxes inside single-select groups
        $(document).on("click", ".single-select input[type=\'checkbox\']", 
        function() {
          var group = "input[name=\'" + $(this).attr("name") + "\']";
          $(group).not(this).prop("checked", false);
        });
      });
    '
    )),

    # Green heading
    tags$head(
      tags$style(HTML(
        '
      /* CSS to center the titlePanel */
      .center-title {
        display: flex;
        justify-content: center;
        color: green;
      }
    '
      ))
    ),
    div(class = "center-title", titlePanel("Matching CDF Game")),
  ),

  sidebarLayout(
    sidebarPanel(
      # textOutput("debug"),
      # Input
      radioButtons(
        "dist_type",
        "Distributions",
        choiceValues = c("Normal", "T", "Chisq"),
        choiceNames = c("Normal", "T", HTML("\\(\\chi^2\\)")),
        selected = "Normal"
      ),
      conditionalPanel(
        # degrees of freedom slider only visible for T and X^2 dist
        condition = "input.dist_type != 'Normal'",
        sliderInput(
          "df_slider",
          "Degrees of Freedom",
          min = 1,
          max = 2,
          value = 1,
          step = 1
        )
      ),
      conditionalPanel(
        # Area type selection - not visible for X^2 dist
        condition = "input.dist_type != 'Chisq'",
        checkboxGroupInput(
          "area_type",
          "Type of Area",
          choices = c("Between", "Left", "Right", "Two-tailed"),
          selected = c("Between", "Left", "Right", "Two-tailed")
        ),
      ),
      conditionalPanel(
        # showing Right as the only selectable [and selected] option for X^2
        condition = "input.dist_type == 'Chisq'",
        radioButtons(
          "area_type_radio",
          "Type of Area",
          choices = c("Right")
        )
      ),

      # turning action buttons into green
      tags$head(
        tags$style(HTML(
          '
              /* CSS to style the action buttons as green */
              .btn-green {
                background-color: green;
                color: white;
              }
            '
        ))
      ),
      # Generate Plots and Submit Answer buttons
      actionButton("generate", "New Plots", class = "btn-green"),
      conditionalPanel(
        condition = "input.generate",
        actionButton("submit", "Submit", class = "btn-green"),
      ),
      textOutput("score")
    ),
    mainPanel(
      # Output
      fluidRow(
        # plot 1
        column(
          1,
          div(
            class = "single-select",
            checkboxGroupInput("in1", "", choices = rep(0, 4))
          )
        ),
        column(5, plotOutput("out1")),
        # plot 2
        column(
          1,
          div(
            class = "single-select",
            checkboxGroupInput("in2", "", choices = rep(0, 4))
          )
        ),
        column(5, plotOutput("out2"))
      ),
      fluidRow(
        # plot 3
        column(
          1,
          div(
            class = "single-select",
            checkboxGroupInput("in3", "", choices = rep(0, 4))
          )
        ),
        column(5, plotOutput("out3")),
        # plot 4
        column(
          1,
          div(
            class = "single-select",
            checkboxGroupInput("in4", "", choices = rep(0, 4))
          )
        ),
        column(5, plotOutput("out4"))
      )
    )
  )
)
