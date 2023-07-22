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

```r
library(youdrawitR)
data <- linearDataGen(y_int = -4,
                      slope  = 0.8,
                      sigma  = 2.8,
                      x_min   = 0,
                      x_max   = 20,
                      N       = 40)

drawr(data, show_tooltip = TRUE)
```

![Example usage](man/figures/drawr-example-1.gif)