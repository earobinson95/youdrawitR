#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(youdrawitR)
library(jsonlite)
library(DT)
library(readxl)
library(utils)
library(stats)
library(r2d3)

# Define server logic required to draw a histogram
function(input, output, session) {
    dataSubmitted <- FALSE
    
    if (!dataSubmitted) {
      drawr_output <- reactive({
        input$reset
        
        data <- linearDataGen(
          y_xbar = rnorm(1, 5, 1),
          slope = runif(1, -2, 2),
          sigma = runif(1, 1, 3),
          x_min = 0,
          x_max = 20,
          N = 40,
          x_by = 0.25,
          conf_int = input$showConfInterval
        )
        
        drawr(data, hide_buttons = T, conf_int = input$showConfInterval)
      })
    }
    
    # Add a reactive value to track whether the reset button was clicked
    resetClicked <- reactiveVal(FALSE)
    resetFunction <- reactive({
      session$sendCustomMessage("resetAction", "true")
      if (input$newLine) {
        resetClicked(TRUE)
        updateCheckboxInput(session, "newLine", 
                            label = "New Line", value = FALSE)
      }
      updateRadioButtons(session, "line_selector", selected = "original")
    })
    
    observeEvent(input$reset, {
      resetFunction()
    })
    
    observeEvent(input$showConfInterval, {
      resetFunction()
    })

    # Observe the newLine checkbox and only send message when it's clicked
    observeEvent(input$newLine, {
      if (!resetClicked()) {
        if (input$newLine) {
          updateCheckboxInput(session, "newLine", label = "Stop Drawing", value = TRUE)
          session$sendCustomMessage("newLine", "true")
        } else {
          updateCheckboxInput(session, "newLine", label = "New Line", value = FALSE)
          session$sendCustomMessage("newLine", "false")
        }
      }
      resetClicked(FALSE)  # reset the state of resetClicked after using it
    })
    
    new_line_data <- eventReactive(input$newLineData, {
      newLineData <- input$newLineData
      # Convert the JSON data to a list or data frame
      jsonlite::fromJSON(newLineData)
    })
    
    observeEvent(input$completedLineData, {
      shinyjs::show("recordedDataSection")
    })
    
    observeEvent(input$newLineData, {
      new_line_data <- jsonlite::fromJSON(input$newLineData)
      if (length(new_line_data) > 0) {
        shinyjs::show("dataSelector")
      }
      else {
        shinyjs::hide("dataSelector")
        updateRadioButtons(session, "line_selector", selected = "original")
      }
    })
    
    user_line_data <- eventReactive(input$completedLineData, {
      completedLineData <- input$completedLineData
      # Convert the JSON data to a list or data frame
      jsonlite::fromJSON(completedLineData)
    })
    
    output$drawndata <- renderDataTable({
      # Based on the user selection, display the original or the new line data
      selected_line <- input$line_selector
      if (selected_line == "original") {
        shinyjs::hide("line_number")
        updateSliderInput(session, "line_number", value = 1)
        DT::datatable(user_line_data(), rownames = FALSE)
      } else if (selected_line == "new") {
          if (length(new_line_data()) > 1) {
            shinyjs::show("line_number")
            updateSliderInput(session, "line_number", 
                              max = length(new_line_data()))
            
          }
          else {
            shinyjs::hide("line_number")
            updateSliderInput(session, "line_number", value = 1)
          }
          DT::datatable(new_line_data()[[input$line_number]], rownames = FALSE)
      }
    })
    
    output$saveData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        # Based on the user selection, save the original or the new line data
        selected_line <- input$line_selector
        data_to_save <- NULL
        if (selected_line == "original") {
          data_to_save <- user_line_data()
        } else if (selected_line == "new") {
           data_to_save <- new_line_data()[[input$line_number]]
        }
        
        if (!is.null(data_to_save)) {
          write.csv(data_to_save, file, row.names = FALSE)
        }
      }
    )
    
    output$regressionTitle <- renderUI({
      tags$h3(paste(input$regressionType, "Regression Options"), style = "font-size: 16px; margin-top: -10px;")
    })
    
    observeEvent(input$inputData, {
      session$sendCustomMessage("resetAction", "true")
      if (input$newLine) {
        resetClicked(TRUE)
        updateCheckboxInput(session, "newLine", 
                            label = "New Line", value = FALSE)
      }
      showModal(
        modalDialog(
          fluidRow(
            column(
              width = 6,
              textInput("xColumn", "X Column Name", placeholder = "x"),
              textInput("yColumn", "Y Column Name", placeholder = "y"),

              # Add tooltips to the text input fields
              tags$script(HTML("
                $('#xColumn').tooltip({
                  title: 'Optional if no x colname. (Will use 1st column in dataset)',
                  trigger: 'focus',
                  placement: 'top',
                });
              
                $('#yColumn').tooltip({
                  title: 'Optional if no y colname. (Will use 2nd column in dataset)',
                  trigger: 'focus',
                  placement: 'top'
                });
              "))
            ),
            column(
              width = 6,
              radioButtons("regressionType", "Regression Type:",
                           choices = c("Linear", "Logistic", "Polynomial", "Loess"),
                           selected = "Linear")
            )
          ),
          fluidRow(
            column(
              width = 6,
              selectInput("dataInputOption", "Data Input Option:",
                          choices = c("Upload", "Text"),
                          selected = "Upload"),
              conditionalPanel(
                condition = "input.dataInputOption == 'Upload'",
                fileInput("dataFile", "Upload File", multiple = FALSE)
              ),
              conditionalPanel(
                condition = "input.dataInputOption == 'Text'",
                textAreaInput("dataInput", "Enter Data Here", rows = 5, 
                              placeholder = 'Copy and paste data here \n(" " , ; | or tab delimited data accepted)\n\nExample:\n1,2\n3,4\n5,6\n7,8')
              )
            ),
            column(
              width = 6,
              wellPanel(
                uiOutput("regressionTitle"),
              conditionalPanel(
                condition = "input.regressionType == 'Linear'",
                checkboxInput(inputId = "confInt",
                              label = "Display 95% Confidence Interval Bounds",
                              value = FALSE)
              ),
              conditionalPanel(
                condition = "input.regressionType == 'Polynomial'",
                sliderInput("degree", "Degree:", min = 2, max = 10, value = 2)
              ),
              conditionalPanel(
                condition = "input.regressionType == 'Logistic'",
                textInput("successLevel", "Success Level:"),
                tags$script(HTML("
                $('#successLevel').tooltip({
                  title: 'Y must be a binary categorical variable. (If empty success level is first category alphabetically)',
                  trigger: 'focus',
                  placement: 'top',
                });
              "))
              ),
              conditionalPanel(
                condition = "input.regressionType == 'Loess'",
                sliderInput("degree", "Degree:", min = 0, max = 2, value = 1)
              ),
              conditionalPanel(
                condition = "input.regressionType == 'Loess'",
                sliderInput("span", "Span:", min = 0, max = 1, value = 0.75, step = 0.05)
              )
            )
          )
          ),
          footer = tagList(
            actionButton("submitData", "Submit"),
            modalButton("Cancel")
          )
      )
      )
    })
    
    observeEvent(input$submitData, {
      shinyjs::hide("showConfInterval")
      # Rename the columns based on user input or set to NULL if no input
      if (is.null(input$xColumn) || input$xColumn == "" || is.null(input$yColumn) || input$yColumn == "") {
        colnames <- NULL
      } else {
        colnames <- c(input$xColumn, input$yColumn)
      }
      if (input$dataInputOption == "Upload") {
        if (!is.null(input$dataFile$datapath)) {
          file_ext <- tools::file_ext(input$dataFile$name)
          if (file_ext != "txt") {
            dataInput <- switch(file_ext,
                                  "csv" = read.csv(input$dataFile$datapath, header = !is.null(colnames)),
                                  "tsv" = read.table(input$dataFile$datapath, sep = "\t", header = !is.null(colnames), fill = TRUE),
                                  "xls" = readxl::read_excel(input$dataFile$datapath, col_names = !is.null(colnames)),
                                  "xlsx" = readxl::read_excel(input$dataFile$datapath, col_names = !is.null(colnames)),
                                  # Add more file types and their corresponding read functions as needed
                                  stop("Unsupported file type.")
              )
          }
          else {
            # Set the delimiter based on user input (default to space)
            if (grepl("\t", input$dataInput)) {
              separator <- "\t"
            } else if (grepl(";", input$dataInput)) {
              separator <- ";"
            } else if (grepl(":", input$dataInput)) {
              separator <- ":"
            } else if (grepl("\\|", input$dataInput)) {
              separator <- "|"
            } else if (grepl(",", input$dataInput)) {
              separator <- ","
            } else {
              separator <- " "
            }
            dataInput <- read.table(input$dataFile$datapath, header = !is.null(colnames), sep = separator, fill = TRUE)
            print(dataInput)
          }
        } else {
          stop("No file selected.")
        } 
        } else {
          if (is.null(input$dataInput) || input$dataInput == "") {
            stop("No data entered.")
          }
          # Set the delimiter based on user input (default to space)
          if (grepl("\t", input$dataInput)) {
            separator <- "\t"
          } else if (grepl(";", input$dataInput)) {
            separator <- ";"
          } else if (grepl(":", input$dataInput)) {
            separator <- ":"
          } else if (grepl("\\|", input$dataInput)) {
            separator <- "|"
          } else if (grepl(",", input$dataInput)) {
            separator <- ","
          } else {
            separator <- " "
          }
          
          # Use the text entered in the text area input
          dataInput <- read.table(text = input$dataInput, header = !is.null(colnames), sep = separator, fill = TRUE)
        }
      # Get the selected regression type
      regression_type <- input$regressionType
      if (!is.null(colnames)) {
        if (regression_type == "Polynomial") {
          data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, degree = input$degree)
        }
        else if (regression_type == "Logistic") {
          if (!is.null(input$successLevel) && input$successLevel != "") {
            data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, success_level = input$successLevel)
          }
          else {
            data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type)
          }
        }
        else if (regression_type == "Loess") {
          data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, degree = input$degree, span = input$span)
        }
        else {
          data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, conf_int = input$confInt)
        }
      }
      else {
        if (regression_type == "Polynomial") {
          data <- customDataGen(dataInput, regression_type = regression_type, degree = input$degree)
        }
        else if (regression_type == "Logistic") {
          if (!is.null(input$successLevel) && input$successLevel != "") {
            data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, success_level = input$successLevel)
          }
          else {
            data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type)
          }
        }
        else if (regression_type == "Loess") {
          data <- customDataGen(dataInput, regression_type = regression_type, degree = input$degree, span = input$span)
        }
        else {
          data <- customDataGen(dataInput, regression_type = regression_type, conf_int = input$confInt)
        }
      }
      
      # Update the drawr output with the processed data
      if ((regression_type == "Linear") && (input$confInt)) {
        output$shinydrawr <- r2d3::renderD3({ drawr(data, hide_buttons = T, conf_int = TRUE) })
      }
      else {
        output$shinydrawr <- r2d3::renderD3({ drawr(data, hide_buttons = T) })
      }
      dataSubmitted <- TRUE
      # Close the modal dialog
      removeModal()
    })
    
    # Function to send the tooltip state to JavaScript
    sendTooltipState <- function(state) {
      session$sendCustomMessage("tooltipState", state)
    }
    
    # Initialize the tooltip state
    tooltipState <- reactiveVal(TRUE)
    
    # Update the tooltip state based on the checkbox input
    observeEvent(input$tooltipButton, {
      tooltipState(input$tooltipButton)
      sendTooltipState(tooltipState())
    })
    
    if (!dataSubmitted) {
      output$shinydrawr <- r2d3::renderD3({ drawr_output() })
    }
  }
