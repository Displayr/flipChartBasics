#' Determines the number of colors required in a chart
#' 
#' Returns the number of colors needed in a chart.
#' This is returned as a list because more than one color-series may be required (e.g. Pie chart)
#' @param data Input data, which is assumed to be created by \code{\link{PrepareData}}
#' @param chart.type Type of chart to plot
#' @export

GetNumColors <- function(data, chart.type)
{
    # data is already assumed to be cleaned up by PrepareData
   
    # Venn and Streamgraphs?
    if (grepl("Scatter", chart.type))
    {
        if (is.null(ncol(data)) || ncol(data) < 4)
            return(list(num.series=1))
        return(list(num.series=length(unique(data[,4]))))
    }
    if (grepl("Pie", chart.type) || grepl("Donut", chart.type))
    {
        if (!is.null(dim(data)) && length(dim(data)) > 1)
            return(list(num.series=ncol(data), num.categories=nrow(data)))
        else
            return(list(num.series=length(data)))
    }
    if (!is.null(ncol(data)))
        return(list(num.series=ncol(data)))
    else 
        return(list(num.series=1))
}