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
              checkboxInput(
                inputId = "tooltipButton",
                label = "Show/Hide Tooltip",
                value = TRUE
              ),
              checkboxInput("showConfInterval", 
                            "Confidence Interval", 
                            value = FALSE),
              checkboxInput("changeColor", "Change color", value = FALSE),
              tags$style(
                HTML(
                  ".normal-button {
                      color: black;
                      background-color: white;
                    }
                    .red-button, .red-button:focus {
                      color: white;
                      background-color: #E60000;
                    }
                  .red-button:hover {
                    color: white;
                    background-color: #B30000;
                  }
                  .hidden-on-load {
                    display: none;
                  }"
                )
              ),
              actionButton("mybutton", "Add New Line", icon = icon("pencil"),
                           class = "normal-button"),
              useShinyjs(),
              hidden(
                div(
                  id = "dataSelector",
                  radioButtons("line_selector", label = "Select data to display", 
                               choices = list("Original line" = "original", "New lines" = "new"),
                               selected = "original"),
                  div(
                    id = "line_number",
                    sliderInput("line_number", "Choose line:", 
                                min = 1, max = 2, 
                                value = 1, step = 1,
                                width = "120px")
                  )
                )
              )
            ),
              div(
                id = "colorSection",
                style = "position: absolute; top: 0px; left: 750px;",
                conditionalPanel(
                  condition = "input.changeColor == true",
                  class = "hidden-on-load",
                selectInput("colorChoice", "Select color to change", 
                            choices = list("Progress region color" = "region_color",
                                           "Drawn line color" = "draw_color",
                                           "Regression line & conf_int color" = "finished_color")),
                conditionalPanel(condition = "input.colorChoice == 'region_color'",
                                 colourInput("region_color", NULL, 
                                             value = "yellow",
                                             showColour = "background",
                                             palette = "limited",
                                             closeOnClick = T)),
                conditionalPanel(condition = "input.colorChoice == 'draw_color'",
                                 colourInput("draw_color", NULL, 
                                             value = "steelblue",
                                             showColour = "background",
                                             palette = "limited",
                                             allowedCols = 
                                               c("#000000FF", "#333333FF", "#4D4D4DFF", "#666666FF", "#7F7F7FFF", "#999999FF", "#B3B3B3FF", "#E5E5E5FF",
                                             "#FFFFFFFF", "#27408BFF", "#000080FF", "#0000FFFF", "#1E90FFFF", "steelblue", "#97FFFFFF", "#00FFFFFF",
                                             "#00868BFF", "#008B45FF", "#458B00FF", "#008B00FF", "#00FF00FF", "#7FFF00FF", "#54FF9FFF", "#00FF7FFF",
                                             "#7FFFD4FF", "#8B4500FF", "#8B0000FF", "#FF0000FF", "#FF6A6AFF", "#FF7F00FF", "#FFFF00FF", "#FFF68FFF",
                                             "#F4A460FF", "#551A8BFF", "#8B008BFF", "#8B0A50FF", "#9400D3FF", "#FF00FFFF", "#FF1493FF", "#E066FFFF"),
                                             closeOnClick = T)),
                conditionalPanel(condition = "input.colorChoice == 'finished_color'",
                                 colourInput("finished_color", NULL, 
                                             value = "steelblue",
                                             showColour = "background",
                                             allowedCols = 
                                               c("#000000FF", "#333333FF", "#4D4D4DFF", "#666666FF", "#7F7F7FFF", "#999999FF", "#B3B3B3FF", "#E5E5E5FF",
                                                 "#FFFFFFFF", "#27408BFF", "#000080FF", "#0000FFFF", "#1E90FFFF", "steelblue", "#97FFFFFF", "#00FFFFFF",
                                                 "#00868BFF", "#008B45FF", "#458B00FF", "#008B00FF", "#00FF00FF", "#7FFF00FF", "#54FF9FFF", "#00FF7FFF",
                                                 "#7FFFD4FF", "#8B4500FF", "#8B0000FF", "#FF0000FF", "#FF6A6AFF", "#FF7F00FF", "#FFFF00FF", "#FFF68FFF",
                                                 "#F4A460FF", "#551A8BFF", "#8B008BFF", "#8B0A50FF", "#9400D3FF", "#FF00FFFF", "#FF1493FF", "#E066FFFF"),
                                             palette = "limited",
                                             closeOnClick = T))),
              useShinyjs(),
              hidden(
                div(
                  id = "recordedDataSection",
                  wellPanel(
                    fluidRow(
                    column(
                      width = 6,
                      p("Recorded Data:")
                    ),
                    column(
                      width = 6,
                      align = "right",
                      div(style = "margin-top: -10px;",  # Adjust this value as needed
                          downloadButton("saveData", "Save Data")
                          )
                      )
                    ),
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
    )
  )