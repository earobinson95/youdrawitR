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
library(colourpicker)
library(clipr)

# Define server logic required to draw a histogram
function(input, output, session) {
    color = reactive({input$region_color})

    dataSubmitted <- FALSE
    
    if (!dataSubmitted) {
      drawr_output <- reactive({
        input$reset
        
        data <- linearDataGen(
          y_int = rnorm(1, 5, 15),
          slope = runif(1, -2, 2),
          sigma = runif(1, 1, 3),
          x_min = 0,
          x_max = 20,
          N = 40,
          conf_int = input$showConfInterval
        )
        
        drawr(data, hide_buttons = T, conf_int = input$showConfInterval, 
              draw_region_color = color())
      })
    }
    
    observeEvent(input$region_color, {
      session$sendCustomMessage("regionColorAction", input$region_color)
      
      session$sendCustomMessage("resetAction", "true")
      if (input$mybutton %% 2 == 1) { # If button has been clicked odd number of times
        resetClicked(TRUE)
        updateActionButton(session, "mybutton", icon = icon("pencil"), label = "Add New Line")
        removeCssClass("mybutton", "red-button")
        addCssClass("mybutton", "normal-button")
      }
      updateRadioButtons(session, "line_selector", selected = "original")
      shinyjs::hide("dataSelector")
      shinyjs::hide("line_number")
      shinyjs::hide("recordedDataSection")
      
      session$onFlushed(once=TRUE, function() {
        session$sendCustomMessage("finishedColorAction", isolate(input$finished_color))
        session$sendCustomMessage("drawColorAction", isolate(input$draw_color))
      })
    })
    
    observeEvent(input$draw_color, {
      session$sendCustomMessage("drawColorAction", input$draw_color)
    })
    
    observeEvent(input$finished_color, {
      session$sendCustomMessage("finishedColorAction", input$finished_color)
    })
    
    # Add a reactive value to track whether the reset button was clicked
    resetClicked <- reactiveVal(FALSE)
    
    observeEvent(input$reset, {
      session$sendCustomMessage("resetAction", "true")
      if (input$mybutton %% 2 == 1) { # If button has been clicked odd number of times
        resetClicked(TRUE)
        updateActionButton(session, "mybutton", icon = icon("pencil"), label = "Add New Line")
        removeCssClass("mybutton", "red-button")
        addCssClass("mybutton", "normal-button")
      }
      updateRadioButtons(session, "line_selector", selected = "original")
      shinyjs::hide("dataSelector")
      shinyjs::hide("line_number")
      shinyjs::hide("recordedDataSection")
      
      session$onFlushed(once=TRUE, function() {
        session$sendCustomMessage("finishedColorAction", isolate(input$finished_color))
        session$sendCustomMessage("drawColorAction", isolate(input$draw_color))
      })
    })
    
    observeEvent(input$showConfInterval, {
      session$sendCustomMessage("resetAction", "true")
      if (input$mybutton %% 2 == 1) { # If button has been clicked odd number of times
        resetClicked(TRUE)
        updateActionButton(session, "mybutton", icon = icon("pencil"), label = "Add New Line")
        removeCssClass("mybutton", "red-button")
        addCssClass("mybutton", "normal-button")
      }
      updateRadioButtons(session, "line_selector", selected = "original")
      shinyjs::hide("dataSelector")
      shinyjs::hide("line_number")
      shinyjs::hide("recordedDataSection")
      
      session$onFlushed(once=TRUE, function() {
        session$sendCustomMessage("finishedColorAction", isolate(input$finished_color))
        session$sendCustomMessage("drawColorAction", isolate(input$draw_color))
      })
    })

    # Observe the newLine checkbox and only send message when it's clicked
    observeEvent(input$mybutton, {
      if (!resetClicked()) {
        if(input$mybutton %% 2 == 1) {
          updateActionButton(session, "mybutton", icon = icon("stop-circle"),
                             label = "Stop Drawing")
          removeClass("mybutton", "normal-button")
          addCssClass("mybutton", "red-button")
          session$sendCustomMessage("newLine", "true")
        } else {
          updateActionButton(session, "mybutton", 
                             icon = icon("pencil"),
                             label = "Add New Line")
          removeClass("mybutton", "red-button")
          addCssClass("mybutton", "normal-button")
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
    session$onSessionEnded(stopApp)
    
    copied_text <- reactiveValues(text = "")
    
    output$clip <- renderUI({
      selected_line <- input$line_selector
      line_number <- input$line_number
      
      data_to_copy <- NULL
      if (selected_line == "original") {
        data_to_copy <- user_line_data()
      } else if (selected_line == "new") {
        data_to_copy <- new_line_data()[[line_number]]
      }
      
      text_lines  <- paste("x, y")
      for (i in 1:nrow(data_to_copy)) {
        text_lines  <- paste(text_lines, paste(data_to_copy[i, ], collapse = ", "), sep = "\n")
      }
      
      copied_text$text <- text_lines

        rclipButton(
          inputId = "clipbtn",
          label = "",
          clipText = copied_text$text,
          icon = icon("clipboard"),
          title = "Copy Data")
    })
    
    
    observeEvent(input$clipbtn, {
      is_local <- Sys.getenv('SHINY_PORT') == ""
      if (is_local) {
        selected_line <- input$line_selector
        line_number <- input$line_number
        
        data_to_copy <- NULL
        if (selected_line == "original") {
          data_to_copy <- user_line_data()
        } else if (selected_line == "new") {
          data_to_copy <- new_line_data()[[line_number]]
        }
        # Only assign if not on shinyapps.io (local R session)
        assign("copied_data", data_to_copy, envir = .GlobalEnv)
      }
      
      showNotification("Data copied to clipboard!", type = "message")
    })

    
    
    output$regressionTitle <- renderUI({
      tags$h3(paste(input$regressionType, "Regression Options"), style = "font-size: 16px; margin-top: -10px;")
    })
    
    observeEvent(input$simulateData, {
      session$sendCustomMessage("resetAction", "true")
      if (input$mybutton %% 2 == 1) { # If button has been clicked odd number of times
        resetClicked(TRUE)
        updateActionButton(session, "mybutton", icon = icon("pencil"), label = "Add New Line")
        removeCssClass("mybutton", "red-button")
        addCssClass("mybutton", "normal-button")
      }
      updateRadioButtons(session, "line_selector", selected = "original")
      shinyjs::hide("dataSelector")
      shinyjs::hide("line_number")
      shinyjs::hide("recordedDataSection")
      session$onFlushed(once=TRUE, function() {
        session$sendCustomMessage("finishedColorAction", isolate(input$finished_color))
        session$sendCustomMessage("drawColorAction", isolate(input$draw_color))
      })
      showModal(
        modalDialog(
          title = "Simulate Linear Data Parameters",
          fluidRow(
            column(
              width = 5,
              sliderInput("beta", "Slope (Beta):", min = -1, max = 1, value = 0.2, step = 0.05),
              sliderInput("sd", "Standard Deviation:", min = 0.01, max = 2, value = 0.2, step = 0.05),
              sliderInput("y_int", "Y Intercept:", min = -15, max = 30, value = 5, step = 1),
            ),
            column(
              width = 5,
              sliderInput("Npoints", "Number of Points:", min = 10, max = 100, value = 20, step = 5),
              sliderInput("x_range", "X Range:", min = -20, max = 50, value = c(0, 20)),
              checkboxInput(inputId = "confInt",
                            label = "Display 95% Confidence Interval Bounds",
                            value = FALSE)
            )
          ),
          footer = tagList(
            actionButton("submitSimulateData", "Submit", class = "btn btn-primary"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    observeEvent(input$submitSimulateData, {
      shinyjs::hide("showConfInterval")
      
      data <- linearDataGen(slope = input$beta,
                            y_int = input$y_int,
                            sigma = input$sd,
                            x_min = input$x_range[1],
                            x_max = input$x_range[2],
                            N = input$Npoints,
                            conf_int = input$confInt)
      # Update the drawr output with the processed data
      if (input$confInt) {
        output$shinydrawr <- r2d3::renderD3({ drawr(data, hide_buttons = T, conf_int = TRUE, draw_region_color = color()) })
      }
      else {
        output$shinydrawr <- r2d3::renderD3({ drawr(data, hide_buttons = T, draw_region_color = color) })
      }
      
      session$onFlushed(once=TRUE, function() {
        session$sendCustomMessage("finishedColorAction", isolate(input$finished_color))
        session$sendCustomMessage("drawColorAction", isolate(input$draw_color))
      })
      
      dataSubmitted <- TRUE
      # Close the modal dialog
      removeModal()
    })
    
    observeEvent(input$inputData, {
      session$sendCustomMessage("resetAction", "true")
      if (input$mybutton %% 2 == 1) { # If button has been clicked odd number of times
        resetClicked(TRUE)
        updateActionButton(session, "mybutton", icon = icon("pencil"), label = "Add New Line")
        removeCssClass("mybutton", "red-button")
        addCssClass("mybutton", "normal-button")
      }
      updateRadioButtons(session, "line_selector", selected = "original")
      shinyjs::hide("dataSelector")
      shinyjs::hide("line_number")
      shinyjs::hide("recordedDataSection")
      session$onFlushed(once=TRUE, function() {
        session$sendCustomMessage("finishedColorAction", isolate(input$finished_color))
        session$sendCustomMessage("drawColorAction", isolate(input$draw_color))
      })
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
            actionButton("submitData", "Submit", class = "btn btn-primary"),
            modalButton("Cancel")
          )
      )
      )
    })
    
    observeEvent(input$submitData, {
      shinyjs::hide("showConfInterval")
      dataInput <- tryCatch({
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
              read.table(input$dataFile$datapath, header = !is.null(colnames), sep = separator, fill = TRUE)
            }
          } else {
            stop("No file selected.")
          } 
          } else {
            if (is.null(input$dataInput) || input$dataInput == "") {
              stop("No data entered.")
            }
            dataInput <- input$dataInput
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
              # Remove extra white spaces
              dataInput <- gsub(" +", " ", input$dataInput)
            }
            
            # Use the text entered in the text area input
            read.table(text = dataInput, header = !is.null(colnames), sep = separator, fill = TRUE)
          }  
        }, error = function(e) {
          showNotification(paste("Error reading data:", e$message), type = "error")
          removeModal()
          return(NULL)  # return NULL on error
        })
      
      # Stop execution if an error occurred while reading data
      if(is.null(dataInput)) return()
      
      # Get the selected regression type
      regression_type <- input$regressionType
      
      data <- tryCatch({
        if (!is.null(colnames)) {
          if (regression_type == "Polynomial") {
            customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, degree = input$degree)
          }
          else if (regression_type == "Logistic") {
            if (!is.null(input$successLevel) && input$successLevel != "") {
              customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, success_level = input$successLevel)
            }
            else {
              customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type)
            }
          }
          else if (regression_type == "Loess") {
            customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, degree = input$degree, span = input$span)
          }
          else {
            customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, conf_int = input$confInt)
          }
        }
        else {
          if (regression_type == "Polynomial") {
            customDataGen(dataInput, regression_type = regression_type, degree = input$degree)
          }
          else if (regression_type == "Logistic") {
            if (!is.null(input$successLevel) && input$successLevel != "") {
              customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, success_level = input$successLevel)
            }
            else {
              customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type)
            }
          }
          else if (regression_type == "Loess") {
            customDataGen(dataInput, regression_type = regression_type, degree = input$degree, span = input$span)
          }
          else {
            customDataGen(dataInput, regression_type = regression_type, conf_int = input$confInt)
          }
          }
        }, error = function(e) {
          showNotification(paste("Error generating data:", e$message), type = "error")
          removeModal()
          return(NULL)  # return NULL on error
        })
      
      # Stop execution if an error occurred while generating data
      if(is.null(data)) return()
      
      # Update the drawr output with the processed data
      if ((regression_type == "Linear") && (input$confInt)) {
        output$shinydrawr <- r2d3::renderD3({ drawr(data, hide_buttons = T, conf_int = TRUE, draw_region_color = color) })
      }
      else {
        output$shinydrawr <- r2d3::renderD3({ drawr(data, hide_buttons = T, draw_region_color = color) })
      }
      session$onFlushed(once=TRUE, function() {
        session$sendCustomMessage("finishedColorAction", isolate(input$finished_color))
        session$sendCustomMessage("drawColorAction", isolate(input$draw_color))
      })
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
    
    # Reactive values to hold selected x and y columns
    selectedColumns <- reactiveVal(c(NA, NA))
    
    # Watch for rDataset click
    observeEvent(input$rDataset, {
      # Reset selectedColumns
      selectedColumns(c(NA, NA))
      
      session$sendCustomMessage("resetAction", "true")
      if (input$mybutton %% 2 == 1) { # If button has been clicked odd number of times
        resetClicked(TRUE)
        updateActionButton(session, "mybutton", icon = icon("pencil"), label = "Add New Line")
        removeCssClass("mybutton", "red-button")
        addCssClass("mybutton", "normal-button")
      }
      updateRadioButtons(session, "line_selector", selected = "original")
      shinyjs::hide("dataSelector")
      shinyjs::hide("line_number")
      shinyjs::hide("recordedDataSection")
      session$onFlushed(once=TRUE, function() {
        session$sendCustomMessage("finishedColorAction", isolate(input$finished_color))
        session$sendCustomMessage("drawColorAction", isolate(input$draw_color))
      })
      
      # Show modal dialog
      showModal(modalDialog(
        title = "Select Dataset and Columns",
        
        fluidRow(
          column(width = 6,
              selectizeInput('datasetSelector', 'Choose a dataset', 
                                choices = c('mtcars', 'iris', 'airquality'))
          ),
          column(width = 6,
              wellPanel(uiOutput("selectedColumns"))
          )),
        
        # Wrap DTOutput in a div with style for horizontal scrolling
        div(id = "tableDisplay", style = "max-width: 100%; overflow-x: auto;", DTOutput('tableDisplay')),
        
        tags$script(HTML('
          var targetNode = document.getElementById("selectedColumns");
        
          function updateTooltip() {
            var selectedColumnsText = $("#selectedColumns").text();
        
            // Safeguard: Only proceed if the text is present
            if (selectedColumnsText) {
              var selectedColumns = selectedColumnsText.trim().split(",");
        
              // Further safeguards: Only proceed if split operation produces expected results
              if (selectedColumns.length >= 2) {
                var xSelected = selectedColumns[0].split(":")[1].trim().replace(/"/g, "");  // remove double quotes
                var ySelected = selectedColumns[1].split(":")[1].trim().replace(/"/g, "");
        
                var message = "";
                if (xSelected === "NA" && ySelected === "NA") {
                  message = "Click a column to select as X-var";
                } else if (ySelected === "NA") {
                  message = "Click a column to select as Y-var";
                } else {
                  message = "Submit or restart by clicking new X-var column";
                }
                $("#tableDisplay").attr("title", message);
              }
            }
          }
        
          var observerOptions = {
            childList: true,
            attributes: true,
            characterData: true
          };
        
          var observer = new MutationObserver(updateTooltip);
          observer.observe(targetNode, observerOptions); // when the selectedColumns changed updateTooltip
        ')),
        
        footer = tagList(
          actionButton("submitRData", "Submit", class = "btn btn-primary"),
          modalButton("Cancel")
        )
      ))
    })
    
    observeEvent(input$datasetSelector, {
      selectedColumns(c(NA, NA))
    })
    
    output$tableDisplay <- renderDT({
      req(input$datasetSelector)
      
      dataset <- switch(input$datasetSelector,
                        mtcars = mtcars,
                        iris = iris,
                        airquality = airquality)
      
      datatable(dataset, 
                options = list(columnDefs = list(list(targets = '_all', className = 'dt-center'))),
                selection = "none", 
                callback = JS("
                    table.on('click', 'td', function() {
                        var colIndex = table.cell(this).index().column;
                        var colName = table.column(colIndex).header();
                        Shiny.setInputValue('clickedColumn', $(colName).html(), {priority: 'event'});
                    });
                  "))
    })
    
    # Rest of the logic remains same
    observeEvent(input$clickedColumn, {
      currentColumns <- selectedColumns()
      if (is.na(currentColumns[1])) {
        currentColumns[1] <- input$clickedColumn
      } else if (is.na(currentColumns[2])) {
        currentColumns[2] <- input$clickedColumn
      } else {
        currentColumns <- c(input$clickedColumn, NA)
      }
      selectedColumns(currentColumns)
    })
    
    output$selectedColumns <- renderUI({
      cols <- selectedColumns()
      output_text <- paste("<strong>Currently Selected Columns</strong><br>",
                           "X:", cols[1], ", Y:", cols[2])
      HTML(output_text)
    })
    
    observeEvent(input$submitRData, {
      shinyjs::hide("showConfInterval")
      
      cols <- selectedColumns()
      dataset <- switch(input$datasetSelector,
                        mtcars = mtcars,
                        iris = iris,
                        airquality = airquality)
      
      data <- tryCatch({
        customDataGen(dataset, cols[1], cols[2])
        }, error = function(e) {
        showNotification(paste("Error generating data:", e$message), type = "error")
        removeModal()
        return(NULL)  # return NULL on error
      })
      
      # Stop execution if an error occurred while generating data
      if(is.null(data)) return()

      output$shinydrawr <- r2d3::renderD3({ drawr(data, hide_buttons = T, draw_region_color = color) })
      removeModal()
    })
    
    if (!dataSubmitted) {
      output$shinydrawr <- r2d3::renderD3({ drawr_output() })
    }
    
  }
