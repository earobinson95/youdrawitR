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