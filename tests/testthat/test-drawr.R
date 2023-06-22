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

test_that("data is correct in output.",{
  line_data <- data.frame(x = c(0,2), y = c(0,2))
  point_data <- data.frame(x = c(0,2), y = c(0,2))
  
  y_range = range(point_data$y) + c(-1, 1)
  
  out <- drawr(data=list(line_data=line_data,
                         point_data=point_data),
               draw_start = 1,
               y_range = y_range)
  expect_equal(toString(out$x$data), 
               '{"line_data":[{"x":0,"y":0,"_row":1},{"x":2,"y":2,"_row":2}],"point_data":[{"x":0,"y":0,"_row":1},{"x":2,"y":2,"_row":2}]}')
  
})

test_that("x and y ranges are calculated correctly if NULL. (and x and y buffer is 0)",{
  x <- sample(0:10, 2)
  y <- sample(0:10, 2)
  line_data <- data.frame(x = x, y = y)
  point_data <- data.frame(x = x, y = y)

  
  out <- drawr(data=list(line_data=line_data,
                         point_data=point_data),
               draw_start = min(x) + 0.01,
               x_axis_buffer = 0,
               y_axis_buffer = 0)
  expect_equal(out$x$options$x_range,  c(min(x), max(x)))
  expect_equal(out$x$options$y_range,  c(min(y), max(y)))
})