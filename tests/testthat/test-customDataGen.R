test_that("customDataGen returns correct dimensions", {
  # Create sample input dataframe
  df <- data.frame(x = c(1,2,3), y = c(2,6,6))
  
  # Call customDataGen with sample inputs
  result <- customDataGen(df = df, xvar = "x", yvar = "y")
  
  # Verify that line data has expected number of rows and columns
  # 1 for data type (line), 1 for x, 1 for y
  expect_equal(dim(result$line_data),c(3L ,3L))
  
  # Verify that point data has expected number of rows and columns 
  # 1 for data type (point), 1 for x, 1 for y
  expect_equal(dim(result$point_data),c(3L ,3L)) 
  
})

test_that("customDataGen handles missing and non-numeric values correctly", {
  # Create sample input dataframe with missing values and non-numeric correctly
  df_missing <- data.frame(x = c(1, 2, NA,3), y = c(2, 3, 4, "l"))
  
  # Call customDataGen with sample inputs containing NAs 
  result_missing <- customDataGen(df_missing,"x","y")
  
  # Verify if any NAs are present in output lists or not 
  expect_false(any(is.na(result_missing)))
})

test_that("customDataGen handles missing and non-numeric values correctly if x and y NULL", {
  # Create sample input dataframe with missing values and non-numeric correctly
  df_missing <- data.frame(x = c(1, 2, NA,3), y = c(2, 3, 4, "l"))
  
  # Call customDataGen with sample inputs containing NAs 
  result_missing <- customDataGen(df_missing)
  
  # Verify if any NAs are present in output lists or not 
  expect_false(any(is.na(result_missing)))
})

test_that("Error: The specified x-variable does not exist in the column names of the data frame.", {
  df <- data.frame(a = 1:5, b = 6:10)
  xvar <- "c"
  yvar <- "b"
  
  expect_error(customDataGen(df, xvar, yvar), "Error: The specified x-variable does not exist in the column names of the data frame.")
})

# Test for y-variable not present in column names
test_that("Error: The specified y-variable does not exist in the column names of the data frame.", {
  df <- data.frame(a = 1:5, b = 6:10)
  xvar <- "a"
  yvar <- "c"
  
  expect_error(customDataGen(df, xvar, yvar), "Error: The specified y-variable does not exist in the column names of the data frame.")
})

# Test for data frame not having enough columns
test_that("Error: The provided data frame does not have enough columns.", {
  df <- data.frame(a = 1:5)
  xvar <- NULL
  yvar <- NULL
  
  expect_error(customDataGen(df, xvar, yvar), "Error: The provided data frame does not have enough columns.")
})