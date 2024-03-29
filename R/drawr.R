#' Drawr
#'
#' \code{drawr()} draws an interactive you-draw-it plot for interactive testing of graphics. 
#' Data can be simulated using \code{linearDataGen()} or inputted using a data frame from 
#' \code{customDataGen()}.
#'
#'
#' @param data The data containing line data (with equation info) and point data.
#' @param save_html_file_path File path to save d3 output as an html. If null will not save. Can just provide filename and will save to current working directory. (default: NULL)
#' @param hide_buttons Logical value indicating whether to show or hide buttons. TRUE = hide, FALSE = show (default: FALSE)
#' @param conf_int Whether to generate a 95\% confidence interval for the fitted line. Must select conf_int = TRUE in \code{linearDataGen()} or \code{customDataGen()} functions to generate interval. (default: FALSE)
#' @param linear Choice of a linear or log y-scale, "true" = linear, else = log. If using log scale choose log_y = TRUE in \code{customDataGen()} function when generating data. (default: "true").
#' @param log_base The base of the log scale, only affects graph if not linear is not "true". If NULL will use natural logarithm. Log_base should match log_base choice in \code{customDataGen()} function (default = NULL)
#' @param draw_start The starting point for drawing. Must be larger than minimum x value and smaller than maximum x value. If null is provided will use minimum x plus smallest possible positive number such that x min != sum. Only provide if free_draw != TRUE. (default: NULL).
#' @param points_end The ending x-value for the points. Will only affect the graph if points are "partial" (default: NULL).
#' @param x_by The offset applied to rectangle in relation to current progress. (default: 0)
#' @param free_draw Whether to allow freehand drawing for entire graph. If false, begin drawing at draw_start. (default: T).
#' @param points The type of points to be displayed. Choices: "full" or "partial". Full will always display all points, while for partial the user can choose not to include points. (default: "full").
#' @param aspect_ratio The aspect ratio of the plot (default: 1).
#' @param title The title of the plot. (default: "")
#' @param x_range The range of x values. If null is provided will use range of line data x values. WARNING: even if x_range is smaller than x range of point data, the line is still fitted for all points in dataset. (default: NULL)
#' @param y_range The range of y values. If null is provided will use range of line data y values. WARNING: even if y_range is smaller than y range of point data, the line is still fitted for all points in dataset. (default: NULL)
#' @param x_lab The x-axis label. (default: "")
#' @param y_lab The y-axis label. (default: "")
#' @param subtitle The subtitle of the plot. (default: "")
#' @param drawn_line_color The color of the drawn lines. (default: "steelblue")
#' @param true_line_color The color of the true drawn lines and confidence interval region. (default: "steelblue")
#' @param draw_region_color The color of the drawing region that displays progress. If NULL, region will be transparent. (default: "rgba(255,255,0,.8)" (yellow))
#' @param x_axis_buffer The buffer for the x-axis added to the x range, calculated as a percent of x range. Only used if x_range is NULL, and must be greater than or equal to 0. (default: 0)
#' @param y_axis_buffer The buffer for the y-axis added to the y range, calculated as a percent of y range. Only used if y_range is NULL, and must be greater than or equal to 0. (default: 0.05)
#' @param show_finished Whether to show the finished plot (default: TRUE).
#' @param show_tooltip Whether to display tooltips or not. (default: FALSE)
#' 
#' @examples
#' # Example 1: Simulating linear data and plotting
#' data <- linearDataGen(y_int = -4,
#'                       slope  = 0.8,
#'                       sigma  = 2.8,
#'                       x_min  = 0,
#'                       x_max  = 20,
#'                       N      = 40)
#' print(drawr(data))
#'
#' # Example 2: Using custom data frame and custom options
#' df <- data.frame(Time = c(0, 1, 2, 3, 4, 5, 9, NA, 12, 6, 7),
#'                  Cost = c(NA, 2, 4, 6, 8, 10, 18, 12, 10, 14, 14))
#' data <- customDataGen(df, "Time", "Cost")
#' print(drawr(data           = data,
#'             aspect_ratio   = 0.85,
#'             title          = "Title",
#'             x_range        = c(0, 15),
#'             subtitle       = "Subtitle",
#'             x_lab          = "x-axis",
#'             y_lab          = "y-axis",
#'             x_axis_buffer  = 0,
#'             y_axis_buffer  = 1,
#'             show_tooltip   = TRUE))
#'             
#' # Example 3: Using a non-linear scale
#' df <- data.frame(Time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' df$Cost <- exp(df$Time)
#' data <- customDataGen(df = df, log_y = TRUE, log_base = 2)
#' print(drawr(data, linear = "no", log_base = 2))
#' 
#' # Example 4: Start drawing in the middle and include tooltips
#' df <- data.frame(
#'   Time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'   Cost = c(1, 4, 9, 16, 18, 16, 9, 4, 2, 1)
#' )
#' 
#' data <- df |>  customDataGen()
#' 
#' print(drawr(data, free_draw = FALSE, draw_start = 5))
#' 
#' @return The rendered interactive you-draw-it plot. The plot is displayed
#' automatically when function is called if not assigned to a variable for further use.
#' @export
#' 
#' @importFrom r2d3 r2d3 renderD3 save_d3_html
#' @importFrom jsonlite toJSON fromJSON
drawr <- function(data, 
                  save_html_file_path = NULL,
                  hide_buttons      = FALSE,
                  conf_int          = FALSE,
                  linear            = "true", 
                  log_base          = NULL,
                  draw_start        = NULL,
                  points_end        = NULL,
                  x_by              = 0,
                  free_draw         = TRUE,
                  points            = "full",
                  aspect_ratio      = 1,
                  title             = "", 
                  x_range           = NULL, 
                  y_range           = NULL,
                  x_lab             = "", 
                  y_lab             = "", 
                  subtitle          = "",
                  drawn_line_color  = "steelblue",
                  true_line_color   = "steelblue",
                  draw_region_color = "rgba(255,255,0,.8)",
                  x_axis_buffer     = 0, 
                  y_axis_buffer     = 0.05,
                  show_finished     = TRUE,
                  show_tooltip      = FALSE) {
  if (!is.list(data) || !("line_data" %in% names(data)) || !("point_data" %in% names(data))) {
    stop("Error: 'data' should be a list containing 'line_data' and 'point_data'")
  }
  
  if (x_axis_buffer < 0) {
    stop("Error: x_axis_buffer must be greater than or equal to 0")
  }
  
  if (y_axis_buffer < 0) {
    stop("Error: y_axis_buffer must be greater than or equal to 0")
  }
  
  if (!is.numeric(aspect_ratio) || aspect_ratio <= 0) {
    stop("Error: 'aspect_ratio' must be a positive number")
  }
  
  if (!(points %in% c("full", "partial"))) {
    stop("Error: 'points' must be either 'full' or 'partial'")
  }
  
  if (!is.numeric(x_by)) {
    stop("Error: 'x_by' must be a number")
  }
  
  if (!is.logical(free_draw)) {
    stop("Error: 'free_draw' must be a boolean")
  }
  
  if (!is.logical(show_finished)) {
    stop("Error: 'show_finished' must be a boolean")
  }
  
  if (!is.logical(show_tooltip)) {
    stop("Error: 'show_tooltip' must be a boolean")
  }
  
  if (!is.null(log_base)) {
    if (!is.numeric(log_base) || log_base <= 0) {
      stop("Error: 'log_base', if provided, must be a positive number")
    }
  }
  
  line_data  <- data$line_data
  point_data <- data$point_data
  if (conf_int) {
    if (!all(c("lower_bound", "upper_bound") %in% names(data$line_data))) {
      stop("Error: 'lower_bound' and 'upper_bound' must be in 'line_data'. Remember to set 'conf_int = TRUE' in 'linearDataGen' or 'customDataGen' to generate confidence interval data or set conf_int = FALSE in `drawr`")
    }
    
    lower_bound <- tibble(x = line_data$x,
                          y = line_data$lower_bound)
    upper_bound <- tibble(x = line_data$x,
                          y = line_data$upper_bound)
  }

  x_min <- min(line_data$x)
  x_max <- max(line_data$x)
  y_min <- min(c(min(line_data$y), min(point_data$y)))
  y_max <- max(c(max(line_data$y), max(point_data$y)))
  
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
    if (sum(point_data$y >= y_range[1]) < 2 || sum(point_data$y <= y_range[2]) < 2) {
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
      if (exists("coef", where = line_data)) {
        if (x_min > min_range && x_max >= max_range) {
          x <- c(min_range, line_data$x)
          if (is.numeric(line_data$coef[1])) {
            # If both numeric it is linear regression
            if (is.numeric(line_data$int[1])) {
              y_min <- line_data$coef[1] * min_range + line_data$int[1]
            }
            # If intercept is character it is logistic regression
            else {
              linear_pred <- line_data$coef[1] * min_range + as.numeric(line_data$int[1])
              y_min <- exp(linear_pred) / (1 + exp(linear_pred))
            }
            y <- c(y_min, line_data$y)
          } # If coefficient is character it is polynomial regression
          else {
            poly_coefficients <- as.numeric(strsplit(line_data$coef[1], ", ")[[1]])
            y <- c(sapply(min_range, function(x_val) sum((x_val^seq_along(poly_coefficients)) * poly_coefficients) + line_data$int[1]), line_data$y)
          }
        }
        else if (x_max < max_range && x_min <= min_range) {
          x <- c(line_data$x, max_range)
          if (is.numeric(line_data$coef[1])) {
            if (is.numeric(line_data$int[1])) {
              y_max <- line_data$coef[1] * max_range + line_data$int[1]
            }
            else {
              linear_pred <- line_data$coef[1] * max_range + as.numeric(line_data$int[1])
              y_max <- exp(linear_pred) / (1 + exp(linear_pred))
            }
            y <- c(line_data$y, y_max)
          }
          else {
            poly_coefficients <- as.numeric(strsplit(line_data$coef[1], ", ")[[1]])
            y <- c(line_data$y, sapply(max_range, function(x_val) sum((x_val^seq_along(poly_coefficients)) * poly_coefficients) + line_data$int[1]))
          }
        }
        else {
          x <- c(min_range, line_data$x, max_range)
          if (is.numeric(line_data$coef[1])) {
            if (is.numeric(line_data$int[1])) {
              y_min <- line_data$coef[1] * min_range + line_data$int[1]
              y_max <- line_data$coef[1] * max_range + line_data$int[1]
            }
            else {
              linear_pred <- line_data$coef[1] * x_range + as.numeric(line_data$int[1])
              y_min <- exp(linear_pred[1]) / (1 + exp(linear_pred[1]))
              y_max <- exp(linear_pred[2]) / (1 + exp(linear_pred[2]))
            }
            y <- c(y_min, line_data$y, y_max)
          }
          else {
            poly_coefficients <- as.numeric(strsplit(line_data$coef[1], ", ")[[1]])
            pred_values <- sapply(x_range, function(x_val) sum((x_val^seq_along(poly_coefficients)) * poly_coefficients) + line_data$int[1])
            y <- c(pred_values[1], line_data$y, pred_values[2])
          }
        }
      }
      else {
        x <- line_data$x
        y <- line_data$y
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
  
  if (linear != "true") {
    if (is.null(log_base)) {
      line_data$y <- exp(line_data$y)
    }
    else {
      line_data$y <- log_base ^ line_data$y
    }
  }
  
  data$line_data <- line_data
  data$point_data <- point_data
  if (conf_int) {
    data$lower_bound <- lower_bound
    data$upper_bound <- upper_bound
  }
  
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
  
  drawr_out <- r2d3(data   = data_to_json(data), 
                    script = system.file("www/you-draw-it.js", package = "youdrawitR"),
                    d3_version = "5",
                    dependencies = c("d3-jetpack"),
                    options = list(draw_start        = draw_start, 
                                   hide_buttons      = hide_buttons,
                                   points_end        = points_end,
                                   linear            = as.character(linear),
                                   log_base          = log_base,
                                   free_draw         = free_draw, 
                                   points            = points,
                                   aspect_ratio      = aspect_ratio,
                                   pin_start         = TRUE, 
                                   x_range           = x_range,
                                   x_by              = x_by,
                                   x_lab             = x_lab,
                                   y_range           = y_range,
                                   y_lab             = y_lab,
                                   subtitle          = subtitle,
                                   line_style        = NULL,
                                   drawn_line_color  = drawn_line_color,
                                   data_line_color   = true_line_color,
                                   draw_region_color = draw_region_color,
                                   show_finished     = show_finished,
                                   show_tooltip      = show_tooltip,
                                   title             = title,
                                   conf_int          = conf_int))
  
  if (!is.null(save_html_file_path)) {
    save_d3_html(drawr_out, save_html_file_path)
  }
  return(drawr_out)
}