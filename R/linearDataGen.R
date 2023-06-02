#' Linear Data Generation
#'
#' Generates linear line and point data.
#'
#' @param y_xbar The y-intercept of the line data
#' @param slope The slope of the true line data
#' @param sigma The standard deviation of the line data.
#' @param points_choice Indicates which type of points to generate for point data. (default: "full")
#' @param points_end_scale A scaling factor for determining the end point of the drawn line. (default: 1)
#' @param N The number of points to generate. (default: 30)
#' @param x_min The minimum x value. (default: 0)
#' @param x_max The maximum x value. (default: 20)
#' @param x_by The increment value for x. (default: 0.25)
#' 
#' @return A list containing the generated point data and line data.
#' @export
#' 
#' @importFrom tibble tibble
linearDataGen <- 
  function(y_xbar, 
           slope, 
           sigma, 
           points_choice = "full", 
           points_end_scale = 1,
           N = 30,
           x_min = 0,
           x_max = 20,
           x_by  = 0.25){
    
    points_end_scale <- ifelse(points_choice == "full", 1, points_end_scale)
    
    # Set up x values
    xVals <- seq(x_min, x_max, length.out = N)
    xVals <- ifelse(xVals < x_min, x_min, xVals)
    xVals <- ifelse(xVals > x_max, x_max, xVals)
    
    yintercept = y_xbar - slope*mean(xVals)
    
    errorVals <- rnorm(N, 0, sigma)
    
    
    # Simulate point data
    point_data <- tibble(data = "point_data",
                         x = xVals,
                         y = yintercept + slope*x + errorVals) %>%
      arrange(x)
    
    # Obtain least squares regression coefficients
    lm.fit <- lm(y ~ x, data = point_data)
    yintercepthat <- coef(lm.fit)[1] %>% as.numeric()
    slopehat <- coef(lm.fit)[2] %>% as.numeric()
    
    # Simulate best fit line data
    line_data <- tibble(data = "line_data", 
                        x = seq(x_min, x_max, x_by), 
                        y = yintercepthat + slopehat*x)
    
    data <- list(point_data = point_data, line_data = line_data)
    
    return(data)
  }