#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(r2d3)
library(DT)
library(shinyjs)
library(colourpicker)

# Define UI for application that draws a histogram
navbarPage(
  "Can 'You Draw It'?",
  
  tabPanel(
    title = "Example: Eye Fitting Straight Lines in the Modern Era",
    fluidRow(
      column(
        width = 12,
        div(style = "position: relative;",
            d3Output("shinydrawr", height = "500px", width = "800px"),
            actionButton("inputData", "Input Data", 
                         style = "position: absolute; top: 10px; left: 600px;"),
            actionButton("simulateData", "Simulate Data", 
                         style = "position: absolute; top: 60px; left: 600px;"),
            actionButton("reset", "Reset", 
                         style = "position: absolute; top: 445px; left: 600px;",
                         class = "btn btn-primary"),
            div(
              style = "position: absolute; top: 100px; left: 600px;",
              checkboxInput("newLine", "New Line", value = FALSE),
              checkboxInput(
                inputId = "tooltipButton",
                label = "Show/Hide Tooltip",
                value = TRUE
              ),
              checkboxInput("showConfInterval", 
                            "Confidence Interval", 
                            value = FALSE),
              useShinyjs(),
              hidden(
                div(
                  id = "dataSelector",
                  radioButtons("line_selector", label = "Select data to display", 
                               choices = list("Original line" = "original", "New lines" = "new"),
                               selected = "original")
                ),
                div(
                  id = "line_number",
                  sliderInput("line_number", "Choose line:", 
                              min = 1, max = 2, 
                              value = 1, step = 1,
                              width = "120px")
                )
              )
            ),
            useShinyjs(),
            hidden(
              div(
                id = "recordedDataSection",
                style = "position: absolute; top: 15px; left: 750px;",
                p("Recorded Data:"),
                div(
                  style = "position: absolute; top: -10px; right: 10px;",
                  downloadButton("saveData", "Save Data")),
                div(
                  style = "min-width: 300px; width: 95%;",
                  dataTableOutput("drawndata")
                )
              )
            )
        ),
        colourInput("region_color", "Select color of progress region", 
                    value = "rgba(255,255,0,.8)", 
                    allowTransparent = TRUE)
      )
    )
  )
)
