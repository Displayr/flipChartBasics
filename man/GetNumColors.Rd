% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getnumcolors.R
\name{GetNumColors}
\alias{GetNumColors}
\title{Determines the number of colors required in a chart}
\usage{
GetNumColors(
  data,
  chart.type,
  scatter.colors.column = 4,
  multi.color.series = FALSE
)
}
\arguments{
\item{data}{Input data, a named vector or matrix.}

\item{chart.type}{Type of chart to plot}

\item{scatter.colors.column}{For scatterplot data, the column of data which is used to determine the colors of each point.}

\item{multi.color.series}{For bar and column charts, a logical indicating 
how colors are used. By default, this value is set to FALSE, in which case
the number of colors returned is equal to the number of data series (or columns)
in \code{data}. When \code{multi.color.series == TRUE}, multiple colors are
assigned to the categories within a single data series, so the number of 
colors returned is equal to \code{NROW(data)}.}
}
\value{
An integer specifying the number of colors used by the chart.
}
\description{
Returns the number of colors needed in a chart.
This is returned as a list because more than one color-series may be required (e.g. Pie chart)
}
