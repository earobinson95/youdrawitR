#' Create a Shiny app for drawing
#'
#' \code{drawr_app()} creates a Shiny app for drawing an interactive you-draw-it plot for interactive testing of graphics
#'
#' @param drawr_output The output from the \code{drawr()} function to render the drawing interface. If null will auto randomly generate data using \code{linearDataGen()} function. (Default: NULL) 
#'
#' @return A Shiny app object.
#'
#' @export
#' 
#' @importFrom shiny shinyApp navbarPage tabPanel tags fluidRow column helpText h4 br actionButton observeEvent eventReactive observe reactive div showModal modalDialog textInput fileInput radioButtons conditionalPanel sliderInput tagList modalButton removeModal checkboxInput reactiveVal p downloadButton downloadHandler
#' @importFrom stats runif
#' @importFrom utils read.csv read.table write.csv
#' @importFrom readxl read_excel
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom r2d3 d3Output renderD3 
#' @importFrom shinyjs useShinyjs hidden show
#' @importFrom jsonlite fromJSON
drawr_app <- function(drawr_output = NULL) {
  ui <- navbarPage(
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
                  style = "position: absolute; top: -10px; left: 350px;",
                  downloadButton("saveData", "Save Data")),
                dataTableOutput("drawndata", width = "90%")
              )
              )
          )
        )
      )
    )
  )
  
  if (is.null(drawr_output)) {
    server <- function(input, output, session) {
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
            x_by = 0.25
          )
          
          drawr(data, run_app = T)
        })
      }
      
      observeEvent(input$reset, {
        session$sendCustomMessage("resetAction", "true")
      })
      
      observeEvent(input$completedLineData, {
        show("recordedDataSection")
      })
      
      user_line_data <- eventReactive(input$completedLineData, {
        completedLineData <- input$completedLineData
        # Convert the JSON data to a list or data frame
        jsonlite::fromJSON(completedLineData)
      })
      
      output$drawndata <- renderDataTable({
        DT::datatable(user_line_data(), rownames = FALSE)
      })
      
      output$saveData <- downloadHandler(
        filename = "data.csv",
        content = function(file) {
          write.csv(user_line_data(), file, row.names = FALSE)
        }
      )
      
      observeEvent(input$inputData, {
        showModal(
          modalDialog(
            fluidRow(
              column(
                width = 6,
                textInput("xColumn", "X Column Name", placeholder = "x"),
                textInput("yColumn", "Y Column Name", placeholder = "y")
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
                fileInput("dataFile", "Upload File", multiple = FALSE)
              ),
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.regressionType == 'Polynomial'",
                  sliderInput("degree", "Degree:", min = 2, max = 10, value = 2)
                ),
                conditionalPanel(
                  condition = "input.regressionType == 'Logistic'",
                  textInput("successLevel", "Success Level:")
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
            ),
            footer = tagList(
              actionButton("submitData", "Submit"),
              modalButton("Cancel")
            )
          )
        )
      })
      
      observeEvent(input$submitData, {
        if (!is.null(input$dataFile$datapath)) {
          file_ext <- tools::file_ext(input$dataFile$name)
          dataInput <- switch(file_ext,
                              "csv" = read.csv(input$dataFile$datapath),
                              "tsv" = read.table(input$dataFile$datapath, sep = "\t"),
                              "xls" = readxl::read_excel(input$dataFile$datapath),
                              "xlsx" = readxl::read_excel(input$dataFile$datapath),
                              # Add more file types and their corresponding read functions as needed
                              stop("Unsupported file type.")
          )
        } else {
          # Use the text entered in the text area input
          dataInput <- read.table(text = input$dataInput, header = TRUE)
        }
        # Rename the columns based on user input
        colnames <- c(input$xColumn, input$yColumn)
        
        # Get the selected regression type
        regression_type <- input$regressionType
        
        if (regression_type == "Polynomial") {
          data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, degree = input$degree)
        }
        else if (regression_type == "Logistic") {
          data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, success_level = input$successLevel)
        }
        else if (regression_type == "Loess") {
          data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, degree = input$degree, span = input$span)
        }
        else {
          data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type)
        }
        
        # Update the drawr output with the processed data
        output$shinydrawr <- r2d3::renderD3({ drawr(data, run_app = T) })
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
  }
  else {
    server <- function(input, output, session) {
      dataSubmitted <- FALSE
      observeEvent(input$reset, {
        reset = "true"
        session$sendCustomMessage("resetAction", "true")
      })
      
      observeEvent(input$completedLineData, {
        show("recordedDataSection")
      })
      
      user_line_data <- eventReactive(input$completedLineData, {
        completedLineData <- input$completedLineData
        
        # Convert the JSON data to a list or data frame
        jsonlite::fromJSON(completedLineData)
      })
      
      output$drawndata <- renderDataTable({
        DT::datatable(user_line_data(), rownames = FALSE)
      })
      
      output$saveData <- downloadHandler(
        filename = "data.csv",
        content = function(file) {
          write.csv(user_line_data(), file, row.names = FALSE)
        }
      )
      
      observeEvent(input$inputData, {
        showModal(
          modalDialog(
            fluidRow(
              column(
                width = 6,
                textInput("xColumn", "X Column Name", placeholder = "x"),
                textInput("yColumn", "Y Column Name", placeholder = "y")
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
                fileInput("dataFile", "Upload File", multiple = FALSE)
              ),
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.regressionType == 'Polynomial'",
                  sliderInput("degree", "Degree:", min = 2, max = 10, value = 2)
                ),
                conditionalPanel(
                  condition = "input.regressionType == 'Logistic'",
                  textInput("successLevel", "Success Level:")
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
            ),
            footer = tagList(
              actionButton("submitData", "Submit"),
              modalButton("Cancel")
            )
          )
        )
      })
      
      
      observeEvent(input$submitData, {
        if (!is.null(input$dataFile$datapath)) {
          file_ext <- tools::file_ext(input$dataFile$name)
          dataInput <- switch(file_ext,
                              "csv" = read.csv(input$dataFile$datapath),
                              "tsv" = read.table(input$dataFile$datapath, sep = "\t"),
                              "xls" = readxl::read_excel(input$dataFile$datapath),
                              "xlsx" = readxl::read_excel(input$dataFile$datapath),
                              # Add more file types and their corresponding read functions as needed
                              stop("Unsupported file type.")
          )
        } else {
          # Use the text entered in the text area input
          dataInput <- read.table(text = input$dataInput, header = TRUE)
        }
        # Rename the columns based on user input
        colnames <- c(input$xColumn, input$yColumn)
        
        # Get the selected regression type
        regression_type <- input$regressionType
        
        if (regression_type == "Polynomial") {
          data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, degree = input$degree)
        }
        else if (regression_type == "Logistic") {
          data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type, success_level = input$successLevel)
        }
        else {
          data <- customDataGen(dataInput, colnames[1], colnames[2], regression_type = regression_type)
        }
        
        # Update the drawr output with the processed data
        output$shinydrawr <- r2d3::renderD3({ drawr(data, run_app = T) })
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
        output$shinydrawr <- r2d3::renderD3({ drawr_output })
      }
    }
  }
  
  return(shinyApp(ui = ui, server = server))
}