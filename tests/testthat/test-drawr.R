test_that("error should be thrown if draw_start is out outside of x_range", {
  line_data <- data.frame(x = c(1:10), y = runif(10))
  point_data <- data.frame(x = c(1, 2, 5 ,8, 10), y = runif(5))

  expect_error(drawr(data = list(line_data = line_data, 
                                 point_data = point_data),
                     draw_start = Inf), "Draw start is out of data range.")
  expect_error(drawr(data = list(line_data = line_data, 
                                 point_data = point_data),
                     draw_start = -Inf), "Draw start is out of data range.")
})

test_that("an error should be thrown when supplied y range doesn't cover data fully.",{
  line_data <- data.frame(x = c(1:10), y = runif(10))
  point_data <- data.frame(x = c(1, 2, 5 ,8, 10), y = runif(5))
  y_range = range(point_data$y) + c(1, -1)

  expect_error(drawr(data=list(line_data=line_data,
                               point_data=point_data),
                     draw_start = 2,
                     y_range = y_range),
               "Supplied y range doesn't cover data fully.")
})