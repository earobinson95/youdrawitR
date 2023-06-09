% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drawr_app.R
\name{drawr_app}
\alias{drawr_app}
\title{Create a Shiny app for drawing}
\usage{
drawr_app(drawr_output = NULL)
}
\arguments{
\item{drawr_output}{The output from the \code{drawr()} function to render the drawing interface. If null will auto randomly generate data using \code{linearDataGen()} function. (Default: NULL)}
}
\value{
A Shiny app object.
}
\description{
\code{drawr_app()} creates a Shiny app for drawing an interactive you-draw-it plot for interactive testing of graphics
}
