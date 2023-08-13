#' Custom Data Generator
#'
#' \code{customDataGen()} takes in an R data frame and processes it to generate data suitable for the
#' \code{drawr()} function.
#'
#' @param df An R data frame containing the input data.
#' @param xvar The name of the x variable as a string. If null is provided will use first column of dataframe (default: NULL)
#' @param yvar The name of the y variable as a string. If null is provided will use second column of dataframe. (default: NULL)
#' @param regression_type Type of regression data to generate. Options include "linear", "polynomial", "logistic" (only for binary logistic regression), or "loess". (Default: "linear")
#' @param success_level Which level of binary categorical variable should be considered success if using "logistic" regression. If NULL uses alphabetical order. (Default: NULL)
#' @param degree The degree for a polynomial or loess regression. If chosen in regression_type. For loess 'degree' must be 0, 1 or 2. (Default: 2 for polynomial, 1 for loess)
#' @param span The span for a loess regression. (Default: 0.75)
#' @param log_y Specify whether to apply a logarithmic transformation to y for the fitted line for when using a non-linear scale in the \code{drawr()} function. Currently only for linear regression. If TRUE, the fitted line is transformed as log(y) ~ x; if FALSE, the fitted line is not transformed. (default: FALSE)
#' @param log_base Log base for log transformation, only applies if log_y is true. If NULL will apply a natural log transformation. Log_base should match log_base choice in \code{drawr()} function. (default: NULL)
#' @param conf_int If a 95\% confidence interval should be generated for \code{drawr()} function. Currently only for linear regression. (default: FALSE)
#'
#' @return A list containing the point data and line data (with equation info) processed from inputted data frame.
#'
#' @export
#' 
#' @importFrom tibble tibble
#' @importFrom stats lm coef na.omit predict glm relevel loess
#' @importFrom dplyr mutate across
customDataGen <- function(df, xvar = NULL, yvar = NULL, regression_type = "linear", success_level = NULL, degree = NULL, span = 0.75, log_y = F, log_base = NULL, conf_int = F) {
  if (!(is.data.frame(df))) {
    stop("Error: The provided 'df' must be a data frame.")
  }
  # Check if xvar is present in column names
  if (!is.null(xvar) && !(xvar %in% colnames(df))) {
    stop("Error: The specified x-variable does not exist in the column names of the data frame.")
  }
  
  # Check if yvar is present in column names
  if (!is.null(yvar) && !(yvar %in% colnames(df))) {
    stop("Error: The specified y-variable does not exist in the column names of the data frame.")
  }
  
  # Convert regression_type to lowercase
  regression_type <- tolower(regression_type)
  
  if ((regression_type == "linear") || (regression_type == "polynomial") || (regression_type == "loess")) {
    # Filter out non-numeric and NA values
    if (is.null(xvar) && is.null(yvar)) {
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
    else if (is.null(xvar)) {
      tryCatch({
        df <- df |> 
          mutate(across(1, as.numeric),
                 across(yvar, as.numeric)) |> 
          na.omit() |> 
          suppressWarnings()
      }, error = function(e) {
        stop("Error: The provided data frame does not have enough columns.")
      })
    }
    else if (is.null(yvar)) {
      tryCatch({
        df <- df |> 
          mutate(across(xvar, as.numeric),
                 across(2, as.numeric)) |> 
          na.omit() |> 
          suppressWarnings()
      }, error = function(e) {
        stop("Error: The provided data frame does not have enough columns.")
      })
    }
    else {df <- df |> 
      mutate(across(c(xvar, yvar), as.numeric)) |> 
      na.omit() |> 
      # Suppressing any 'NAs introduced by coercion' Warnings.
      suppressWarnings()
    }
  }
  else if (regression_type == "logistic") {
    # Filter out NA values and convert xvar to numeric and yvar to a factor
    if (is.null(xvar) && is.null(yvar)) {
      tryCatch({
        df <- df |> 
          mutate(across(1, as.numeric),
                 across(2, as.factor)) |> 
          na.omit() |> 
          # Suppressing any 'NAs introduced by coercion' Warnings.
          suppressWarnings()
      }, error = function(e) {
        # Handle the error here
        stop("Error: The provided data frame does not have enough columns or yvar is not a two level factor.")
      })
    }
    else if (is.null(xvar)) {
      tryCatch({
        df <- df |> 
          mutate(across(1, as.numeric),
                 across(yvar, as.factor)) |> 
          na.omit() |> 
          suppressWarnings()
      }, error = function(e) {
        stop("Error: The provided data frame does not have enough columns or yvar is not a two level factor.")
      })
    }
    else if (is.null(yvar)) {
      tryCatch({
        df <- df |> 
          mutate(across(xvar, as.numeric),
                 across(2, as.factor)) |> 
          na.omit() |> 
          suppressWarnings()
      }, error = function(e) {
        stop("Error: The provided data frame does not have enough columns or yvar is not a two level factor.")
      })
    }
    else {df <- df |> 
      mutate(across(xvar, as.numeric),
             across(yvar, as.factor)) |> 
      na.omit() |> 
      # Suppressing any 'NAs introduced by coercion' Warnings.
      suppressWarnings()
    }
    # Set the desired level as the reference category for the logistic regression model
    if (!is.null(success_level)) {
      df[[yvar]] <- relevel(df[[yvar]], ref = success_level)
    }
  }
  else {
    stop("Error: Invalid regression_type. Supported values are 'linear', 'polynomial', 'logistic', or 'loess'.")
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
  
  # Perform linear regression
  if (regression_type == "linear") {
    if (log_y) {
      if (is.null(log_base)) {
        lm.fit <- lm(log(y) ~ x, data = df)
        if (conf_int) {
          predict_data <- data.frame(x)
          predict_data$y <- predict(lm.fit, newdata = predict_data, interval = "confidence")
        }
      }
      else {
        lm.fit <- lm(log(y, base = log_base) ~ x, data = df)
        if (conf_int) {
          predict_data <- data.frame(x)
          predict_data$y <- predict(lm.fit, newdata = predict_data, interval = "confidence")
        }
      }
    }
    else {
      lm.fit <- lm(y ~ x, data = df)
      if (conf_int) {
        predict_data <- data.frame(x)
        predict_data$y <- predict(lm.fit, newdata = predict_data, interval = "confidence")
      }
    }
    
    if (conf_int) {
      # Create line data with x and y values
      line_data <- tibble(data = "line_data",
                          x = x,
                          y = predict_data$y[, "fit"],
                          coef = coef(lm.fit)["x"] |> as.numeric(),
                          int = coef(lm.fit)["(Intercept)"] |> as.numeric(),
                          lower_bound = predict_data$y[, "lwr"],
                          upper_bound = predict_data$y[, "upr"])
    }
    else {
      yintercepthat <- coef(lm.fit)[1] |> as.numeric()
      slopehat <- coef(lm.fit)[2] |> as.numeric()
      
      # Create line data with x and y values
      line_data <- tibble(data = "line_data",
                          x = x,
                          y = yintercepthat + slopehat * x,
                          coef = coef(lm.fit)["x"] |> as.numeric(),
                          int = coef(lm.fit)["(Intercept)"] |> as.numeric())
    }
  }
  # Perform logistic regression
  else if (regression_type == "logistic") {
    if (!(length(levels(y)) == 2)) {
      stop("Error: For 'logistic' regression, y-variable should be a binary factor.")
    }
    
    glm.fit <- glm(y ~ x, data = df, family = "binomial")
    coef <- coef(glm.fit)["x"] |> as.numeric()
    int <- coef(glm.fit)["(Intercept)"] |> as.numeric()
    
    # Calculate the linear predictor
    linear_pred <- int + coef * x
    
    # Calculate the predicted probabilities using the logistic function
    pred_probs <- exp(linear_pred) / (1 + exp(linear_pred))
    
    # Generate line data with predicted probabilities
    line_data <- tibble(data = "line_data",
                        x = x,
                        y = pred_probs,
                        coef = coef,
                        int = int |> as.character())
  }
  else if (regression_type == "loess") {
    if (is.null(degree)) {
      degree <- 1
    }
    if (!(degree %in% c(0, 1, 2))) {
      stop("Error: For 'loess' regression, the 'degree' must be 0, 1, or 2.")
    }
    if (span <= 0 || span > 1) {
      stop("Error: The 'span' for a loess regression must be between 0 (exclusive) and 1 (inclusive).")
    }
    loess.fit <- loess(y ~ x, data = df, degree = degree, span = span)
    line_data <- tibble(data = "line_data",
                        x = x,
                        y = predict(loess.fit),
                        coef = NULL,
                        int = NULL)
  }
  # Perform polynomial linear regression
  else {
    if (is.null(degree)) {
      degree <- 2
    } else if (!is.numeric(degree) || degree < 1) {
      stop("Error: degree for polynomial regression must be numeric and positive.")
    }
    else if (degree >= length(y)) {
      stop("Error: degree for polynomial regression must be less than the number of samples.")
    }
    
    poly.fit <- lm(y ~ poly(x, degree, raw = TRUE), data = df)
  
    # Extract the coefficients except the intercept
    poly_coefficients <- as.list(coef(poly.fit)[-1])
    poly_coeffs <- as.numeric(coef(poly.fit)[-1])
    
    # Extract the intercept
    int <- coef(poly.fit)["(Intercept)"] |> as.numeric()
    
    # Convert the coefficients into a character vector and concatenate them
    coef_str <- paste(poly_coefficients, collapse = ", ")
    
    # Create line data with x and y values
    line_data <- tibble(data = "line_data",
                        x = x,
                        y = sapply(x, function(x_val) sum((x_val^seq_along(poly_coeffs)) * poly_coeffs) + int),
                        coef = coef_str,
                        int = int)
  }
  
  # Create point data with x and y values
  if (is.factor(y)) {
    if (is.null(success_level)) {
      y <- ifelse(y == levels(y)[1], 0, 1)
    }
    else {
      y <- ifelse(y == success_level, 0, 1)
    }
  }
  point_data <- tibble(data = "point_data",
                       x = x,
                       y = y)
  
  # Combine the line and point data into a list
  data <- list(line_data = line_data, point_data = point_data)
  
  # Return the generated data
  return(data)
}