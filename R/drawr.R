#' Drawr
#'
#' \code{drawr()} draws an interactive you-draw-it plot for interactive testing of graphics. 
#' Data can be simulated using \code{linearDataGen()} or inputted using a data frame from 
#' \code{customDataGen()}.
#'
#'
#' @param data The data containing line data (with equation info) and point data.
#' @param linear Whether the data represents a linear relationship (default: "true").
#' @param draw_start The starting point for drawing. Must be larger than minimum x value and smaller than maximum x value. If null is provided will use minimum x plus smallest possible positive number such that x min != sum. (default: NULL).
#' @param points_end The ending x-value for the points. Will only affect the graph if points are "partial" (default: NULL).
#' @param x_by The increment value for x. (default: 0.25)
#' @param free_draw Whether to allow freehand drawing (default: T).
#' @param points The type of points to be displayed. Choices: "full" or "partial". Full will always display all points, while for partial the user can choose not to include points. (default: "full").
#' @param aspect_ratio The aspect ratio of the plot (default: 1).
#' @param title The title of the plot. (default: "")
#' @param x_range The range of x values. If null is provided will use range of line data x values. WARNING: even if x_range is smaller than x range of point data, the line is still fitted for all points in dataset. (default: NULL)
#' @param y_range The range of y values. If null is provided will use range of line data y values. WARNING: even if y_range is smaller than y range of point data, the line is still fitted for all points in dataset. (default: NULL)
#' @param x_lab The x-axis label. (default: "")
#' @param y_lab The y-axis label. (default: "")
#' @param subtitle The subtitle of the plot. (default: "")
#' @param drawn_line_color The color of the drawn line. (default: "steelblue")
#' @param data_tab1_color The color of . (default: "steelblue")
#' @param x_axis_buffer The buffer for the x-axis added to the x range, calculated as a percent of x range. Only used if x_range is NULL, and must be greater than or equal to 0. (default: 0)
#' @param y_axis_buffer The buffer for the y-axis added to the y range, calculated as a percent of y range. Only used if y_range is NULL, and must be greater than or equal to 0. (default: 0.05)
#' @param show_finished Whether to show the finished plot (default: TRUE).
#' @param shiny_message_loc The location to display the shiny message. (default = NULL)
#' 
#' @examples
#' # Example 1: Simulating linear data and plotting
#' data <- linearDataGen(y_xbar = 3.9,
#'                       slope  = 0.8,
#'                       sigma  = 2.8,
#'                       x_min  = 0,
#'                       x_max  = 20,
#'                       N      = 40,
#'                       x_by   = 0.25)
#' print(drawr(data))
#'
#' # Example 2: Using custom data frame and custom options
#' df <- data.frame(Time = c(0, 1, 2, 3, 4, 5, 9, NA, 12, 6, 7),
#'                  Cost = c(NA, 2, 4, 6, 8, 10, 18, 12, 10, 14, 14))
#' data <- customDataGen(df, "Time", "Cost")
#' print(drawr(data           = data,
#'             aspect_ratio   = 0.85,
#'             title          = "Title",
#'             subtitle       = "Subtitle",
#'             x_lab          = "x-axis",
#'             y_lab          = "y-axis",
#'             x_axis_buffer  = 0,
#'             y_axis_buffer  = 1))
#' 
#' @return The rendered interactive you-draw-it plot. The plot is displayed
#' automatically when function is called if not assigned to a variable for further use.
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
                  points            = "full",
                  aspect_ratio      = 1,
                  title             = "", 
                  x_range           = NULL, 
                  y_range           = NULL,
                  x_lab             = "", 
                  y_lab             = "", 
                  subtitle          = "",
                  drawn_line_color  = "steelblue",
                  data_tab1_color   = "steelblue", 
                  x_axis_buffer     = 0, 
                  y_axis_buffer     = 0.05,
                  show_finished     = T,
                  shiny_message_loc = NULL) {
  if (x_axis_buffer < 0) {
    stop("Error: x_axis_buffer must be greater than or equal to 0")
  }
  
  if (y_axis_buffer < 0) {
    stop("Error: y_axis_buffer must be greater than or equal to 0")
  }
  
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
  else {
    if (length(x_range) != 2) {
      stop("Error: Please supply min and max x values for x_range")
    }
    if (sum(point_data$x >= x_range[1]) < 2 || sum(point_data$x <= x_range[2]) < 2) {
      stop("Error: The provided x_range does not include at least two points from point_data.")
    }
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
    if (length(y_range) != 2) {
      stop("Error: Please supply min and max y values for y_range")
    }
    if (sum(point_data$y >= y_range[1]) < 2 || sum(point_data$x <= y_range[2]) < 2) {
      stop("Error: The provided y_range does not include at least two points from point_data.")
    }
    if (y_range[1] > y_min | y_range[2] < y_max) {
      warning("The provided y_range does not cover data, the line will still be fitted from entire dataset.")
      point_data <- point_data[point_data$y >= y_range[1] & point_data$y <= y_range[2], ]
    }
  }
  
  # If x_range is not equal to range of data recalculate line_data so that it takes up 
  # entire range/only range
  ## Depends if both greater, less than, or one or other
  if (!(identical(x_range, range(line_data$x)))) {
    min_range <- x_range[1]
    max_range <- x_range[2]
    # If given range larger than line range increase line size
    if (x_min > min_range || x_max < max_range) {
      if (x_min > min_range && x_max >= max_range) {
        y_min <- line_data$coef[1] * min_range + line_data$int[1]
        x <- c(min_range, line_data$x)
        y <- c(y_min, line_data$y)
      }
      else if (x_max < max_range && x_min <= min_range) {
        y_max <- line_data$coef[1] * max_range + line_data$int[1]
        x <- c(line_data$x, max_range)
        y <- c(line_data$y, y_max)
      }
      else {
        y_min <- line_data$coef[1] * min_range + line_data$int[1]
        y_max <- line_data$coef[1] * max_range + line_data$int[1]
        x <- c(min_range, line_data$x, max_range)
        y <- c(y_min, line_data$y, y_max)
      }
      line_data <- tibble(x = x,
                          y = y)
    }
    
    # If provided range is less than line and point range, remove points and line data outside of range
    if (x_min < min_range || x_max > max_range) {
      warning("The provided x_range does not cover data, the line will still be fitted
              from entire dataset.")
      line_data <- line_data[line_data$x >= min_range & line_data$x <= max_range, ]
      point_data <- point_data[point_data$x >= min_range & point_data$x <= max_range, ]
    }
  }
  
  data$line_data <- line_data
  data$point_data <- point_data
  # If draw_start is NULL calculate lowest possible draw_start such that draw_start != x_min
  if (is.null(draw_start)) {
    draw_start <- x_min + max(.Machine$double.eps * abs(x_min), .Machine$double.xmin)
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
  
  return(r2d3(data   = data_to_json(data), 
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
  ))
}