test_that("Output is a list containing two data frames", {
  output <- linearDataGen(y_xbar = 0, slope = 1, sigma = 1)
  
  expect_type(output, "list")
  expect_length(output, 2)
  
  expect_s3_class(output[[1]], "data.frame")
  expect_s3_class(output[[2]], "data.frame")
})

test_that("point_data and line_data columns exist in both dataframes", {
  output <- linearDataGen(y_xbar = -5.5,slope=3.12,sigma=10,N=100,x_min=-10,x_max=30)
  
  names_df_1<-names(output$point_data)|>as.character()
  names_df_2<-names(output$line_data)|>as.character()
  
  # Checking if column names 'data', 'x' and 'y' exists in each dataframe.
  col_names_expected=c('data','x','y')
  
  expect_true(all(col_names_expected %in% names_df_1))
  expect_true(all(col_names_expected %in% names_df_2))
})

# Test that minimum and maximum x values are correct
test_that("Minimum and maximum x values are correct", {
  output <- linearDataGen(y_xbar = 0, slope = 1, sigma = 1, x_min = 5, x_max = 30)
  
  expect_equal(min(output$point_data$x), 5)
  expect_equal(max(output$point_data$x), 30)
})

# Test that number of points in point data is correct
test_that("Number of points in point data is correct", {
  output <- linearDataGen(y_xbar = -5.5,slope=3.12,sigma=10,N=100,x_min=-10,x_max=30)

  expect_equal(nrow(output$point_data), 100)
})