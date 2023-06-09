test_that("error should be thrown if draw_start is out outside of x_range", {
  line_data <- data.frame(x = c(1:10), y = runif(10))
  point_data <- data.frame(x = c(1, 2, 5 ,8, 10), y = runif(5))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))

  expect_error(drawr(data = list(line_data = line_data, 
                                 point_data = point_data),
                     draw_start = Inf), "Draw start is out of data range.")
  expect_error(drawr(data = list(line_data = line_data, 
                                 point_data = point_data),
                     draw_start = -Inf), "Draw start is out of data range.")
})

test_that("an error should be thrown when supplied y range doesn't include 2 points from point_data.",{
  line_data <- data.frame(x = c(1:10), y = runif(10))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  point_data <- data.frame(x = c(1, 2, 5 ,8, 10), y = runif(5))

  expect_error(drawr(data=list(line_data=line_data,
                               point_data=point_data),
                     draw_start = 2,
                     y_range = c(1, 2)),
               "Error: The provided y_range does not include at least two points from point_data.")
})


test_that("an error should be thrown when supplied y range/x_range is not of length of 2 and not null.",{
  line_data <- data.frame(x = c(1:10), y = runif(10))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  point_data <- data.frame(x = c(1, 2, 5 ,8, 10), y = runif(5))
  
  expect_error(drawr(data=list(line_data=line_data,
                               point_data=point_data),
                     draw_start = 2,
                     x_range = c(0)),
               "Error: Please supply min and max x values for x_range")
})

test_that("a warning should be thrown if y_range doesn't fully cover data",{
  line_data <- data.frame(x = c(1:10), y = runif(10))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  point_data <- data.frame(x = c(1, 2, 5 ,8, 10), y = c(1, 2, 3, 4, 5))

  expect_warning(drawr(data=list(line_data=line_data,
                                 point_data=point_data),
                       draw_start = 2,
                       y_range = c(2, 4)),
                 "The provided y_range does not cover data, the line will still be fitted from entire dataset.")
})

test_that("data is correct in output.",{
  line_data <- data.frame(x = c(0,2), y = c(0,2))
  point_data <- data.frame(x = c(0,2), y = c(0,2))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  
  y_range = range(point_data$y) + c(-1, 1)
  
  out <- drawr(data=list(line_data=line_data,
                         point_data=point_data),
               draw_start = 1,
               y_range = y_range)
  expect_equal(toString(out$x$data), 
               '{"line_data":[{"x":0,"y":0,"coef":2.5,"int":2.5,"_row":1},{"x":2,"y":2,"coef":2.5,"int":2.5,"_row":2}],"point_data":[{"x":0,"y":0,"_row":1},{"x":2,"y":2,"_row":2}]}')
})

test_that("data is correct in output if max(range) is larger than actual",{
  line_data <- data.frame(x = c(0,2), y = c(0,2))
  point_data <- data.frame(x = c(0,2), y = c(0,2))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  
  out <- drawr(data=list(line_data=line_data,
                         point_data=point_data),
               x_range = c(0, 3))
  expect_equal(toString(out$x$data), 
               '{"line_data":[{"x":0,"y":0,"_row":1},{"x":2,"y":2,"_row":2},{"x":3,"y":10,"_row":3}],"point_data":[{"x":0,"y":0,"_row":1},{"x":2,"y":2,"_row":2}]}')
})

test_that("data is correct in output if min(x_range) is smaller than actual",{
  line_data <- data.frame(x = c(0,2), y = c(0,2))
  point_data <- data.frame(x = c(0,2), y = c(0,2))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  
  out <- drawr(data=list(line_data=line_data,
                         point_data=point_data),
               x_range = c(-1, 2))
  expect_equal(toString(out$x$data), 
               '{"line_data":[{"x":-1,"y":0,"_row":1},{"x":0,"y":0,"_row":2},{"x":2,"y":2,"_row":3}],"point_data":[{"x":0,"y":0,"_row":1},{"x":2,"y":2,"_row":2}]}')
})

test_that("data is correct in output if min(x_range) is smaller and max(x_range) is larger ",{
  line_data <- data.frame(x = c(0,2), y = c(0,2))
  point_data <- data.frame(x = c(0,2), y = c(0,2))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  
  out <- drawr(data=list(line_data=line_data,
                         point_data=point_data),
               x_range = c(-1, 3))
  expect_equal(toString(out$x$data), 
               '{"line_data":[{"x":-1,"y":0,"_row":1},{"x":0,"y":0,"_row":2},{"x":2,"y":2,"_row":3},{"x":3,"y":10,"_row":4}],"point_data":[{"x":0,"y":0,"_row":1},{"x":2,"y":2,"_row":2}]}')
})

test_that("data is correct in output if min(x_range) is larger and max(x_range) is smaller",{
  line_data <- data.frame(x = c(-1, 0, 2, 4), y = c(-1, 0, 2, 4))
  point_data <- data.frame(x = c(-1, 0, 2, 4), y = c(-1, 0, 2, 4))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  
  out <- suppressWarnings(drawr(data=list(line_data=line_data,
                         point_data=point_data),
               x_range = c(0, 2)))
  expect_equal(toString(out$x$data), 
               '{"line_data":[{"x":0,"y":0,"coef":2.5,"int":2.5,"_row":2},{"x":2,"y":2,"coef":2.5,"int":2.5,"_row":3}],"point_data":[{"x":0,"y":0,"_row":2},{"x":2,"y":2,"_row":3}]}')
})

test_that("data is correct in output if min(x_range) is larger and y_range is smaller",{
  line_data <- data.frame(x = c(-1, 0, 2, 4), y = c(-1, 0, 2, 4))
  point_data <- data.frame(x = c(-1, 0, 2, 4), y = c(-1, 0, 2, 4))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  
  out <- suppressWarnings(drawr(data=list(line_data=line_data,
                                          point_data=point_data),
                                x_range = c(0, 4),
                                y_range = c(0, 2)))
  expect_equal(toString(out$x$data), 
               '{"line_data":[{"x":0,"y":0,"coef":2.5,"int":2.5,"_row":2},{"x":2,"y":2,"coef":2.5,"int":2.5,"_row":3},{"x":4,"y":4,"coef":2.5,"int":2.5,"_row":4}],"point_data":[{"x":0,"y":0,"_row":2},{"x":2,"y":2,"_row":3}]}')
})

test_that("data is correct in output if only min(x_range) is larger",{
  line_data <- data.frame(x = c(-1, 0, 2), y = c(-1, 0, 2))
  point_data <- data.frame(x = c(-1, 0, 2), y = c(-1, 0, 2))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  
  out <- suppressWarnings(drawr(data=list(line_data=line_data,
                         point_data=point_data),
               x_range = c(0, 2)))
  expect_equal(toString(out$x$data), 
               '{"line_data":[{"x":0,"y":0,"coef":2.5,"int":2.5,"_row":2},{"x":2,"y":2,"coef":2.5,"int":2.5,"_row":3}],"point_data":[{"x":0,"y":0,"_row":2},{"x":2,"y":2,"_row":3}]}')
})

test_that("data is correct in output if max(x_range) is smaller and min(x_range) is smaller",{
  line_data <- data.frame(x = c(0, 2, 4), y = c(0, 2, 4))
  point_data <- data.frame(x = c(0, 2, 4), y = c(0, 2, 4))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  
  out <- suppressWarnings(drawr(data=list(line_data=line_data,
                         point_data=point_data),
               x_range = c(-1, 2)))
  expect_equal(toString(out$x$data), 
               '{"line_data":[{"x":-1,"y":0,"_row":1},{"x":0,"y":0,"_row":2},{"x":2,"y":2,"_row":3}],"point_data":[{"x":0,"y":0,"_row":1},{"x":2,"y":2,"_row":2}]}')
})

test_that("data is correct in output if min(x_range) is larger and max(x_range) is larger",{
  line_data <- data.frame(x = c(-1, 0, 2), y = c(-1, 0, 2))
  point_data <- data.frame(x = c(-1, 0, 2), y = c(-1, 0, 2))
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  
  out <- suppressWarnings(drawr(data=list(line_data=line_data,
                         point_data=point_data),
               x_range = c(0, 3)))
  expect_equal(toString(out$x$data), 
               '{"line_data":[{"x":0,"y":0,"_row":1},{"x":2,"y":2,"_row":2},{"x":3,"y":10,"_row":3}],"point_data":[{"x":0,"y":0,"_row":2},{"x":2,"y":2,"_row":3}]}')
})

test_that("x and y ranges are calculated correctly if NULL. (and x and y buffer is 0)",{
  x <- sample(0:10, 2)
  y <- sample(0:10, 2)
  line_data <- data.frame(x = x, y = y)
  point_data <- data.frame(x = x, y = y)
  line_data$coef <- rep(2.5, length(line_data$x))
  line_data$int <- rep(2.5, length(line_data$x))
  
  out <- drawr(data=list(line_data=line_data,
                         point_data=point_data),
               draw_start = min(x) + 0.01,
               x_axis_buffer = 0,
               y_axis_buffer = 0)
  expect_equal(out$x$options$x_range,  c(min(x), max(x)))
  expect_equal(out$x$options$y_range,  c(min(y), max(y)))
})

test_that("draw_start is correctly determined if NULL", {
  # Define the datasets
  datasets <- list(
    data.frame(x = c(0, 2, 3), y = c(2, 4, 6), coef = c(1,1,1), int = c(1,1,1)),
    data.frame(x = c(100000, 200000, 300000), y = c(2, 4, 6), coef = c(1,1,1), int = c(1,1,1)),
    data.frame(x = c(-100000, -200000, -300000), y = c(2, 4, 6), coef = c(1,1,1), int = c(1,1,1)),
    data.frame(x = c(0.0000001, 0.0002, 0.0003), y = c(2, 4, 6), coef = c(1,1,1), int = c(1,1,1))
  )
  
  # Perform the tests for each dataset
  for (i in seq_along(datasets)) {
    line_data <- datasets[[i]]
    point_data <- data.frame(x = c(2, 3), y = c(4, 6))
    data <- list(line_data = line_data, point_data = point_data)
    
    # Call the drawr function with draw_start set to NULL
    result <- drawr(data, draw_start = NULL)
    
    # Get the actual draw_start value
    actual_draw_start <- result$x$options$draw_start
    
    # Calculate the expected draw_start value
    expected_draw_start <- min(line_data$x) + max(.Machine$double.eps * abs(min(line_data$x)), .Machine$double.xmin)
    
    # Check if the actual draw_start matches the expected draw_start
    expect_equal(actual_draw_start, expected_draw_start)}
})