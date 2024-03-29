# youdrawitR

<!-- badges: start -->

[![R-CMD-check](https://github.com/earobinson95/youdrawitR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/earobinson95/youdrawitR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

‘You Draw It’ is a feature that allows users to interact with a chart directly by drawing a line on their computer screen with a mouse. Originally introduced by the New York Times in 2015 for the purpose of interactive reading, this package adapts the use of the 'You Draw It' method as a tool for interactive testing of graphics.

## Installation

```markdown
devtools::install_github("earobinson95/youdrawitR")
```

## Usage

First, load the youdrawitR package

```r
library(youdrawitR)
```

Then, you can generate data to use with the `drawr()` function using either the `linearDataGen()` or `customDataGen()` functions. The `linearDataGen()` function is used to simulate a linear dataset and generate a confidence interval in this example.

```r
data <- linearDataGen(y_int = -4,
                      slope  = 0.8,
                      sigma  = 2.8,
                      x_min   = 0,
                      x_max   = 20,
                      N       = 40,
                      conf_int = TRUE)
```

Lastly, the data can be entered into the `drawr()` function where the user can draw a line to predict a trend in the data. There are many visual parameters the users can change in the `drawr()` function, in this example the progress tooltip and generated confidence interval from `linearDataGen()` is shown.

```r
drawr(data, show_tooltip = TRUE, conf_int = TRUE)
```

Here is the example of using the interactive graphic. The new line functionality has been used in this example to predict the upper and lower bounds of the generated 95% confidence interval. The color palette does not need to be used when using the new line functionality, the default drawing color when not defined in `drawr()` function is "steelblue".

![](man/figures/drawr-example-1.gif)