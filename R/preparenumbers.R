#' Generates all d3 formats for use by CChart.
#' See https://github.com/d3/d3-format for more information on d3.
#' Wrapper for \code{ChartNumberFormat}.
#' 
#' @param x.format.list A list of five unnamed items for formatting the x-axis as described in \code{\link{ChartNumberFormat}}.
#' @param y.format.list As per \code{x.format.list} except for y-axis formatting.
#' @param hover.format.list As per \code{x.format.list} except for hovertext formatting.
#' @param data.labels.format.list As per \code{x.format.list} except for data label formatting.
#' @export
PrepareNumbers <- function(x.format.list = NULL,
                           y.format.list = NULL,
                           hover.format.list = NULL,
                           data.labels.format.list = NULL) {
    
    x.number.format <- NULL
    y.number.format <- NULL
    hover.number.format <- NULL
    data.labels.number.format <- NULL
    
    if (!is.null(x.format.list))
        x.number.format <- ChartNumberFormat(x.format.list)
        
    if (!is.null(y.format.list))
        y.number.format <- ChartNumberFormat(y.format.list)

    if (!is.null(hover.format.list))
        hover.number.format <- ChartNumberFormat(hover.format.list)
    
    if (!is.null(data.labels.format.list))
        data.labels.number.format <- ChartNumberFormat(data.labels.format.list)
    
    
    return(list(x.number.format = x.number.format, y.number.format = y.number.format,
                hover.number.format = hover.number.format, data.labels.number.format = data.labels.number.format))    
}