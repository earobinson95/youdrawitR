#' Custom Data Generator
#'
#' This function takes in an R data frame and processes it to generate data suitable for the `drawr` function.
#'
#' @param df An R data frame containing the input data.
#' @param xvar The name of the x variable as a string.
#' @param yvar The name of the y variable as a string.
#' 
#' @return A list containing the point data and line data processed from inputted data frame.
#'
#' @export
customDataGen <- function(df, xvar, yvar) {
  # Extract x and y variables from the dataframe
  x <- df[[xvar]]
  y <- df[[yvar]]
  
  # Obtain least squares regression coefficients
  lm.fit <- lm(y ~ x, data = df)
  yintercepthat <- coef(lm.fit)[1] |> as.numeric()
  slopehat <- coef(lm.fit)[2] |> as.numeric()
  
  # Create line data with x and y values
  line_data <- tibble(data = "line_data",
                      x = x,
                      y = yintercepthat + slopehat * x)
  
  # Create point data with x and y values
  point_data <- tibble(data = "point_data",
                       x = x,
                       y = y)
  
  # Combine the line and point data into a list
  data <- list(line_data = line_data, point_data = point_data)
  
  # Return the generated data
  return(data)
}