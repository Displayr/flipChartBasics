#' Determines the number of colors required in a chart
#'
#' Returns the number of colors needed in a chart.
#' This is returned as a list because more than one color-series may be required (e.g. Pie chart)
#' @param data Input data, a named vector or matrix.
#' @param chart.type Type of chart to plot
#' @param scatter.colors.column For scatterplot data, the column of data which is used to determine the colors of each point.
#' @param multi.color.series For bar and column charts, a logical indicating 
#'  how colors are used.
#' @export

GetNumColors <- function(data, chart.type, scatter.colors.column = 4,
                         multi.color.series = FALSE)
{
    # data is already assumed to be cleaned up by PrepareData

    if (chart.type == "Venn" && is.list(data) && all(!sapply(data, is.atomic)))
        return(list(num.series = length(unique(unlist(sapply(data, function(s) return(unlist(s$sets))))))))
    if (grepl("Scatter", chart.type))
    {
        # Multiple tables
        if (is.list(data) && !is.data.frame(data))
            return(nrow(data[[1]]))
        # Data frame with colors variable
        if (is.null(ncol(data)) || is.null(scatter.colors.column) || is.na(scatter.colors.column) || ncol(data) < scatter.colors.column || scatter.colors.column <= 0)
            return(list(num.series = 1))
        if (is.factor(data[,scatter.colors.column]))
            return(list(num.series = nlevels(data[,scatter.colors.column])))
        else
            return(list(num.series = length(unique(data[,scatter.colors.column]))))
    }
    if (isTRUE(multi.color.series) || chart.type == "Bar Pictograph" || 
        chart.type == "Pyramid")
    {
        # Very similar but slightly different for Pie, in the case of a matrix with one column
        if (is.null(dim(data)))
            return(list(num.series = length(data)))
        return(list(num.series = nrow(data)))
    }
    if (chart.type == "Pie" || chart.type == "Donut")
    {
        # Input is a dataframe containing labels (col 1), values (col 2) and maybe groups (col 3)
        if (any(!sapply(data, is.numeric))) #
        {
            if (ncol(data) >= 3)
                return(list(num.series = length(unique(data[,3])), num.categories = nrow(data)))
            else
                return(list(num.series = nrow(data)))
        }
        if (!is.null(dim(data)) && length(dim(data)) > 1)
            return(list(num.series = ncol(data), num.categories = nrow(data)))
        return(list(num.series = length(data)))
    }
    if (chart.type == "Heat" || chart.type == "Geographic Map")
    {
        # Handled inside ChartColors because we always want a gradient (no recycling!)
        return(list(num.series = NA))
    }
    if (!is.null(ncol(data)) && !is.na(ncol(data)))
        return(list(num.series = ncol(data)))
    return(list(num.series = 1))
}
