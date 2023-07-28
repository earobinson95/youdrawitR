#' Linear Data Generation
#'
#' \code{linearDataGen()} generates simulated linear line data (with equation info) and point data 
#' suitable for the \code{drawr()} function.
#'
#' @param y_int The y intercept of the line data.
#' @param slope The slope of the true line data
#' @param sigma The standard deviation of the line data.
#' @param N The number of points to generate. (default: 30)
#' @param x_min The minimum x value. (default: 0)
#' @param x_max The maximum x value. (default: 20)
#' @param conf_int If a 95\% confidence interval should be generated for \code{drawr()} function. (default: FALSE)
#' 
#' @return A list containing the generated point data and line data (with equation info).
#' @export
#' 
#' @importFrom tibble tibble
#' @importFrom dplyr %>% arrange
#' @importFrom stats coef lm rnorm predict
linearDataGen <- 
  function(y_int, 
           slope, 
           sigma, 
           N = 30,
           x_min = 0,
           x_max = 20,
           conf_int = F){
    
    # Set up x values
    xVals <- seq(x_min, x_max, length.out = N)
    xVals <- ifelse(xVals < x_min, x_min, xVals)
    xVals <- ifelse(xVals > x_max, x_max, xVals)
    
    errorVals <- rnorm(N, 0, sigma)
    
    
    # Simulate point data
    point_data <- tibble(data = "point_data",
                         x = xVals,
                         y = y_int + slope*x + errorVals) %>%
      arrange(x)
    
    # Obtain least squares regression coefficients
    lm.fit <- lm(y ~ x, data = point_data)
    if (conf_int) {
      predict_data <- data.frame(x = xVals)
      predict_data$y <- predict(lm.fit, newdata = predict_data, interval = "confidence")
      
      # Create line data with x and y values
      line_data <- tibble(data = "line_data",
                          x = xVals,
                          y = predict_data$y[, "fit"],
                          coef = coef(lm.fit)["x"] |> as.numeric(),
                          int = coef(lm.fit)["(Intercept)"] |> as.numeric(),
                          lower_bound = predict_data$y[, "lwr"],
                          upper_bound = predict_data$y[, "upr"])
    }
    else {
      yintercepthat <- coef(lm.fit)[1] %>% as.numeric()
      slopehat <- coef(lm.fit)[2] %>% as.numeric()
      
      # Simulate best fit line data
      line_data <- tibble(data = "line_data", 
                          x = seq(x_min, x_max, 0.25), 
                          y = yintercepthat + slopehat*x,
                          coef = coef(lm.fit)["x"] |> as.numeric(),
                          int = coef(lm.fit)["(Intercept)"] |> as.numeric())
    }
    
    data <- list(point_data = point_data, line_data = line_data)
    
    return(data)
  }