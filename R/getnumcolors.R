#' Determines the number of colors required in a chart
#'
#' Returns the number of colors needed in a chart.
#' This is returned as a list because more than one color-series may be required (e.g. Pie chart)
#' @param data Input data, a named vector or matrix.
#' @param chart.type Type of chart to plot
#' @param scatter.colors.column For scatterplot data, the column of data which is used to determine the colors of each point.
#' @export

GetNumColors <- function(data, chart.type, scatter.colors.column = 4)
{
    # data is already assumed to be cleaned up by PrepareData

    # Venn and Streamgraphs?
    if (grepl("Scatter", chart.type))
    {
        # Multiple tables
        if (is.list(data) && !is.data.frame(data))
            return(nrow(data[[1]]))
        # Data frame with colors variable
        if (is.null(ncol(data)) || is.na(scatter.colors.column) || ncol(data) < scatter.colors.column || scatter.colors.column <= 0)
            return(list(num.series=1))
        return(list(num.series=length(unique(data[,scatter.colors.column]))))
    }
    if (grepl("Pie", chart.type) || grepl("Donut", chart.type))
    {
        if (!is.null(dim(data)) && length(dim(data)) > 1 && min(dim(data)) > 1)
            return(list(num.series=ncol(data), num.categories=nrow(data)))
        else
            return(list(num.series=length(data)))
    }
    if (grepl("Time", chart.type))
    {
        return(list(num.series=1))
    }
    if (grepl("Geographic", chart.type))
    {
        return(list(num.series=2))
    }
    if (!is.null(ncol(data)))
        return(list(num.series=ncol(data)))
    else
        return(list(num.series=1))
}
