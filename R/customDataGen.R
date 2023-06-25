#' Custom Data Generator
#'
#' \code{customDataGen()} takes in an R data frame and processes it to generate data suitable for the
#' \code{drawr()} function.
#'
#' @param df An R data frame containing the input data.
#' @param xvar The name of the x variable as a string. If null is provided will use first column of dataframe (default: NULL)
#' @param yvar The name of the y variable as a string. If null is provided will use second column of dataframe. (default: NULL)
#' @param log_y Specify whether to apply a logarithmic transformation to y for the fitted line for when using a non-linear scale in the \code{drawr()} function. If TRUE, the fitted line is transformed as log(y) ~ x; if FALSE, the fitted line is not transformed. (default: FALSE)
#' @param log_base Log base for log transformation, only applies if log_y is true. If NULL will apply a natural log transformation. Log_base should match log_base choice in \code{drawr()} function. (default: NULL)
#'
#' @return A list containing the point data and line data (with equation info) processed from inputted data frame.
#'
#' @export
#' 
#' @importFrom tibble tibble
#' @importFrom stats lm coef na.omit
#' @importFrom dplyr mutate across
customDataGen <- function(df, xvar = NULL, yvar = NULL, log_y = F, log_base = NULL) {
  # Check if xvar is present in column names
  if (!is.null(xvar) && !(xvar %in% colnames(df))) {
    stop("Error: The specified x-variable does not exist in the column names of the data frame.")
  }

  # Check if yvar is present in column names
  if (!is.null(yvar) && !(yvar %in% colnames(df))) {
    stop("Error: The specified y-variable does not exist in the column names of the data frame.")
  }

  # Filter out non-numeric and NA values
  if (is.null(xvar)) {
    tryCatch({
    df <- df |> 
      mutate(across(1:2, as.numeric)) |> 
      na.omit() |> 
      # Suppressing any 'NAs introduced by coercion' Warnings.
      suppressWarnings()
    }, error = function(e) {
      # Handle the error here
      stop("Error: The provided data frame does not have enough columns.")
    })
  }
  else {df <- df |> 
    mutate(across(c(xvar, yvar), as.numeric)) |> 
    na.omit() |> 
    # Suppressing any 'NAs introduced by coercion' Warnings.
    suppressWarnings()
  }
  
  # Extract x and y variables from the dataframe
  if (is.null(xvar)) {
    x <- df[[1]]  # Use the first column as x-variable
  } else {
    x <- df[[xvar]]  # Use the specified x-variable
  }
  
  if (is.null(yvar)) {
    y <- df[[2]]  # Use the second column as y-variable
  } else {
    y <- df[[yvar]]  # Use the specified y-variable
  }
  
  # Sort x and y variables in ascending order
  sort_indices <- order(x)
  x <- x[sort_indices]
  y <- y[sort_indices]
  
  # Obtain least squares regression coefficients
  if (log_y) {
    if (is.null(log_base)) {
      lm.fit <- lm(log(y) ~ x, data = df)
    }
    else {
      lm.fit <- lm(log(y, base = log_base) ~ x, data = df)
    }
  }
  else {
    lm.fit <- lm(y ~ x, data = df)
  }
  yintercepthat <- coef(lm.fit)[1] |> as.numeric()
  slopehat <- coef(lm.fit)[2] |> as.numeric()
  
  # Create line data with x and y values
  line_data <- tibble(data = "line_data",
                      x = x,
                      y = yintercepthat + slopehat * x,
                      coef = coef(lm.fit)["x"] |> as.numeric(),
                      int = coef(lm.fit)["(Intercept)"] |> as.numeric())
  
  # Create point data with x and y values
  point_data <- tibble(data = "point_data",
                       x = x,
                       y = y)
  
  # Combine the line and point data into a list
  data <- list(line_data = line_data, point_data = point_data)
  
  # Return the generated data
  return(data)
}