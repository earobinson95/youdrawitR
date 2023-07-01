#' Create a Shiny app for drawing
#'
#' \code{shinyApp()} creates a Shiny app for drawing an interactive you-draw-it plot for interactive testing of graphics
#'
#' @param drawr_output The output from the \code{drawr()} function to render the drawing interface.
#'
#' @return A Shiny app object.
#'
#' @export
drawr_app <- function(drawr_output = NULL) {
  if (is.null(drawr_output)) {
    df <- data.frame(
      Time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      Cost = c(1, 4, 9, 16, 18, 16, 9, 4, 2, 1)
    )
    
    data <- df |>  customDataGen(regression_type = "polynomial")
    
    drawr_output <- drawr(data, run_app = T)
  }
  drawr_output <<- drawr_output
  ui <- navbarPage(
    "Can 'You Draw It'?",
    
    tabPanel(
      title = "Example: Eye Fitting Straight Lines in the Modern Era",
      fluidRow(
        column(
          width = 12,
          helpText(h4("Use your mouse to fill in the trend in the yellow box region")),
          r2d3::d3Output("shinydrawr", height = "500px"),
          br(),
          actionButton("reset", "Reset")
        )
      )
    )
  )

  server <- function(input, output, session) {
    observeEvent(input$reset, {
      reset = "true"
      session$sendCustomMessage("resetAction", "true")
    })
    
    user_line_data <- eventReactive(input$completedLineData, {
      completedLineData <- input$completedLineData
      
      # Convert the JSON data to a list or data frame
      jsonlite::fromJSON(completedLineData)
    })
    
    observe({
      # Access the value of user_line_data() within the observe block
      print(user_line_data())
    })
    
    output$shinydrawr <- r2d3::renderD3({ drawr_output })
  }
  
  return(shinyApp(ui = ui, server = server))
}