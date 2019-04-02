#' Determine colors to be used in a visualization
#' 
#' @param palette A string naming the palette to be used.
#' @param template A list specifying color palettes and other yyvisualization options.
#' @export
GetPalette <- function(palette, template)
{
    if (is.null(palette))
        return(NULL)
    
    result <- palette
    if (palette == "Default colors")
        result <- if (is.null(template)) "Default colors" else template$colors
    if (palette == "Brand colors")
        result <- if (is.null(template)) "Default colors" else template$brand.colors
    return (result)
}

#' Identify colors to be used with brands in the data
#' 
#' @param template A list specifying color palettes and other yyvisualization options.
#' @param input.data Input data for the visualization. The is usually a vector or table. 
#'   It should be normalized from \code{flipChart::PrepareData}.
#' @param filter Optional filter which can be used with the input data. The label of the filter is used when appropriate.
#' @param chart.type The visualization which will be applied to the data.
#' @param scatter.colors.column For scatter plots, an integer specifying the data column used to specify the colors.
#' @export
GetBrandColors <- function(template, input.data, filter, chart.type, scatter.colors.column)
{
    if (is.null(template))
        return("Default colors")
    
    brand.names <- GetBrandsFromData(input.data, filter, chart.type, scatter.colors.column)
    
    # No warning if input data does not have names - Brand colors may not be used
    if (length(brand.names) > 0)
    {
        colors <- checkColors(template$brand.colors[brand.names])
        ind <- which(is.na(names(colors)))
        if (length(ind) > 0)
            colors[ind] <- "#CCCCCC"
        return(colors)
    }
        
    return("Default colors")
}

#' Identify brand names from the input data
#' 
#' Extracts the names of the data series from the input data, to be associated with different colors.
#' This usually corresponds to columns in the input table. But for charts such as 'Pyramid' or 'Pie', each 
#' entry in the vector (or each row in the table) corresponds to a new data series.
#' 
#' @param data Input data for the visualization. The is usually a vector or table. It should be normalized from \code{flipChart::PrepareData}.
#' @param filter Optional filter which can be used with the input data. The label of the filter is used when appropriate.
#' @param chart.type The visualization which will be applied to the data.
#' @param scatter.colors.column For scatter plots, an integer specifying the data column used to specify the colors.
#' @export

GetBrandsFromData <- function(data, filter, chart.type, scatter.colors.column = 4)
{
    if (chart.type %in% c("Heat", "Geographic Map"))
        return(NULL)
    if (grepl("Scatter", chart.type))
    {
        if (is.list(data) && !is.data.frame(data))
            return(rownames(data))
        if (is.null(ncol(data)) || is.na(scatter.colors.column) || ncol(data) < scatter.colors.column || scatter.colors.column <= 0)
            return(NULL)
        
        groups <- data[,scatter.colors.column]
        not.na <- which(is.finite(groups))
        groups.ord <- order(suppressWarnings(flipTransformations::AsNumeric(groups[not.na], binary = FALSE)))
        not.na <- not.na[groups.ord]

        groups <- as.character(groups)
        g.list <- unique(groups[not.na])
        return(g.list)    
    }
    if (chart.type == "Pie")
    {
        if (length(dim(data)) > 1)
            return(colnames(data))
        return(names(data))
    }
    if (chart.type %in% c("Pyramid", "Bar Pictograph", "Donut"))
    {
        if (length(dim(data)) > 1)
            return(rownames(data))
        return(names(data))
    }
    if (NCOL(data) == 1 && !is.null(attr(filter, "label")))
        return(attr(filter, "label"))
    else
        return(colnames(data))
    
}
    
    