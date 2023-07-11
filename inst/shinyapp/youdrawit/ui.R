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
            actionButton("reset", "Reset", 
                         style = "position: absolute; top: 445px; left: 600px;"),
            div(
              style = "position: absolute; top: 60px; left: 600px;",
              checkboxInput(
                inputId = "tooltipButton",
                label = "Show/Hide Tooltip",
                value = TRUE
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
        )
      )
    )
  )
)
