#' Drawr
#'
#' Draws an interactive you-draw-it plot, calling the D3.js file.
#'
#' @param data The data containing line data and point data.
#' @param linear Whether the data represents a linear relationship (default: "true").
#' @param draw_start The starting point for drawing (default: NULL).
#' @param points_end The ending point for the drawn line (default: NULL).
#' @param x_by The increment value for x. (default: 0.25)
#' @param free_draw Whether to allow freehand drawing (default: T).
#' @param points The type of points to be displayed (default: "partial").
#' @param aspect_ratio The aspect ratio of the plot (default: 1.5).
#' @param title The title of the plot. (default: "")
#' @param x_range The range of x values. (default: NULL)
#' @param y_range The range of y values.(default: NULL)
#' @param x_lab The x-axis label. (default: "")
#' @param y_lab The y-axis label. (default: "")
#' @param subtitle The subtitle of the plot. (default: "")
#' @param drawn_line_color The color of the drawn line. (default: "steelblue")
#' @param data_tab1_color The color of . (default: "steelblue")
#' @param x_axis_buffer The buffer for the x-axis. (default: 0.01)
#' @param y_axis_buffer The buffer for the y-axis. (default: 0.05)
#' @param show_finished Whether to show the finished plot (default: TRUE).
#' @param shiny_message_loc The location to display the shiny message. (default = NULL)
#' 
#' @return NULL
#' @export
#' 
#' @importFrom r2d3 r2d3
#' @importFrom jsonlite toJSON
drawr <- function(data, 
                  linear            = "true", 
                  draw_start        = NULL,
                  points_end        = NULL,
                  x_by              = 0.25,
                  free_draw         = T,
                  points            = "partial",
                  aspect_ratio      = 1.5,
                  title             = "", 
                  x_range           = NULL, 
                  y_range           = NULL,
                  x_lab             = "", 
                  y_lab             = "", 
                  subtitle          = "",
                  drawn_line_color  = "steelblue",
                  data_tab1_color   = "steelblue", 
                  x_axis_buffer     = 0.01, 
                  y_axis_buffer     = 0.05,
                  show_finished     = T,
                  shiny_message_loc = NULL) {
  
  line_data  <- data$line_data
  point_data <- data$point_data
  
  x_min <- min(line_data$x)
  x_max <- max(line_data$x)
  y_min <- min(line_data$y)
  y_max <- max(line_data$y)
  
  if (is.null(x_range)) {
    x_buffer <- (x_max - x_min) * x_axis_buffer
    x_range <- c(x_min - x_buffer, x_max + x_buffer)
  }
  if (is.null(y_range)) {
    y_buffer <- (y_max - y_min) * y_axis_buffer
    y_range <- c(y_min - y_buffer, y_max + y_buffer)
    if (linear != "true") {
      if (y_range[1] <= 0) {
        y_range[1] <- min(y_min, y_axis_buffer)
      }
    }
  } else {
    if (y_range[1] > y_min | y_range[2] < y_max) {
      stop("Supplied y range doesn't cover data fully.")
    }
  }
  
  if ((draw_start <= x_min) | (draw_start >= x_max)) {
    stop("Draw start is out of data range.")
  }
  
  # Turn a list of data into a json file
  data_to_json <- function(data) {
    toJSON(data, 
                     dataframe = "rows", 
                     auto_unbox = FALSE, 
                     rownames = TRUE)
  } 
  
  r2d3(data   = data_to_json(data), 
             script = system.file("www/you-draw-it.js", package = "youdrawitR"),
             d3_version = "5",
             dependencies = c("d3-jetpack"),
             options = list(draw_start        = draw_start, 
                            points_end        = points_end,
                            linear            = as.character(linear),
                            free_draw         = free_draw, 
                            points            = points,
                            aspect_ratio      = aspect_ratio,
                            pin_start         = T, 
                            x_range           = x_range,
                            x_by              = x_by,
                            x_lab             = x_lab,
                            y_range           = y_range,
                            y_lab             = y_lab,
                            subtitle          = subtitle,
                            line_style        = NULL,
                            data_tab1_color   = data_tab1_color, 
                            drawn_line_color  = drawn_line_color,
                            show_finished     = show_finished,
                            shiny_message_loc = shiny_message_loc,
                            title             = title)
  )
  
}