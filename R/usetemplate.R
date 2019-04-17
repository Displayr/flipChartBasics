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
    if (palette == "Default or template settings")
        result <- if (is.null(template) || length(template$colors) == 0) "Default colors" else template$colors
    if (palette == "Brand colors")
    {
        result <- template$brand.colors
        if (length(result) == 0)
        {
            warning("Input data does not contain brand names. ",
                    "This is typically because there is only a single data series in the input. ",
                    "Alternatively, try using a pyramid or pie chart.")
            return("Default colors")
        }
        
        ind <- which(is.na(result))
        if (length(ind) > 0)
        {
            warning("Brand colors for '", paste(names(result)[ind], collapse = "', '"), "' have not been supplied.")
            result[ind] <- "#CCCCCC"
        }
    }
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
    
    # Warnings are not given until GetPalette is called - this is when the brand colors is actually used
    if (length(template$brand.colors) == 0)
    {
        colors <- rep(NA, length(brand.names))
        names(colors) <- brand.names
        return(colors)
    }
    if (length(brand.names) > 0)
    {
        colors <- template$brand.colors[brand.names]
        ind <- which(is.na(names(colors)))
        if (length(ind) > 0)
            names(colors)[ind] <- brand.names[ind]
        return(colors)
    }
    return(NULL)
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

#' Print an appearance template object
#'
#' @param  x An object of class \code{AppearanceTemplate}.
#' @param ... Not used.
#' @importFrom flipFormat ShowTemplateOptions
#' @export
print.AppearanceTemplate <- function(x, ...)
{
    tmp.colors <- x$colors
    check.col <- tryCatch(col2rgb(x$colors), error=function(cond){NA})
    if (is.na(check.col))
        tmp.colors <- ChartColors(10, x$colors)
    ShowTemplateOptions(tmp.colors, x$brand.colors, x$global.font, x$fonts)
}
    
    