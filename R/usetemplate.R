#' Determine colors to be used in a visualizatiom
#' 
#' @description This function should not called directly, but instead be called via
#'   \code{\link{GetVectorOfColors}} instead. It remains an exported function 
#'   because it is used in older versions of the Standard R pages
#' @param palette A string naming the palette to be used.
#' @param template A list specifying color palettes and other visualization options.
#' @return A palette which can be used as \code{given.colors} in \link{ChartColors}.
#'   So it can be either the name of a predefined palette (e.g. "Strong colors")
#'   or a vector of colors (e.g. from the template output)
#' @export
GetPalette <- function(palette, template)
{
    if (is.null(palette))
        return(NULL)
    
    result <- palette
    if ((palette == "Default or template settings" && !is.null(names(template$colors))) ||
         palette == "Brand colors")
    {        
        # Take the last color instead of assuming a name for missing values
        missing.color <- if (!is.null(names(template$colors))) rev(template$colors)[1] else "#CCCCCC"
        
        result <- template$brand.colors
        if (length(result) == 0)
        {
            warning("Input data does not contain categories matching the color names. ",
                    "This is typically because there is only a single data series in the input. ",
                    "Alternatively, try using a pyramid or pie chart.")
            return("Default colors")
        }
        
        ind <- which(is.na(result))
        if (length(ind) > 0)
        {
            warning("Named colors for '", paste(names(result)[ind], collapse = "', '"), "' have not been supplied.")
            result[ind] <- missing.color
        }
    } else if (palette == "Default or template settings")
        result <- if (is.null(template) || length(template$colors) == 0) "Default colors" else template$colors
    return (result)
}



#' Identify brand names from the input data
#' 
#' Extracts the names (not necessarily brands) of the data series from the input data, to be associated with different colors.
#' This usually corresponds to columns in the input table. But for charts such as 'Pyramid' or 'Pie', each 
#' entry in the vector (or each row in the table) corresponds to a new data series.
#' This function should not called directly, but instead be called via
#'   \code{\link{GetVectorOfColors}} instead. It remains an exported function 
#'   because it is used in older versions of the Standard R pages
#' 
#' @inherit GetNumColors
#' @param data Input data for the visualization. The is usually a vector or table. It should be normalized from \code{flipChart::PrepareData}.
#' @param filter Optional filter which can be used with the input data. The label of the filter is used when appropriate.
#' @param type Describes the type of data which the color vector will be applied to.
#'  One of "Series" or "Pie subslice". By default, \code{type} is set to "Series",
#'  in which case the return value assigns a different color for each data series.
#'  When \code{chart.type == "Pie"}, \code{type} can be set to "Pie subslice" and 
#'  the return value assigns a color for each subslice of the Pie.
#' @importFrom flipU TrimWhitespace
#' @export

GetBrandsFromData <- function(data, filter, chart.type, 
                        scatter.colors.column = 4, multi.color.series = FALSE,
                        type = "Series")
{
    if (chart.type %in% c("Heat", "Geographic Map"))
        return(NULL)    
    if (chart.type == "Venn" && is.list(data) && all(!sapply(data, is.atomic)))
    {
        names <- unlist(sapply(data, function(x){if (length(x$sets) == 1) x$label}))
        ord <- unlist(sapply(data, function(x){if (length(x$sets) == 1) x$sets}))
        if (length(names) > 0 && length(ord) > 0)
            return(names[(ord - min(ord) + 1)])
    }
    if (grepl("Scatter", chart.type))
    {
        if (is.list(data) && !is.data.frame(data))
            return(rownames(data))
        if (is.null(ncol(data)) || is.null(scatter.colors.column) || is.na(scatter.colors.column) || ncol(data) < scatter.colors.column || scatter.colors.column <= 0)
            return(NULL)
        
        groups <- data[,scatter.colors.column]
        if (is.factor(groups))
            g.list <- levels(groups)
        else if (is.character(groups))
            g.list <- unique(groups[!is.na(groups)])
        else
        {
            # Retain order of factors
            not.na <- which(is.finite(groups))
            groups.ord <- order(suppressWarnings(flipTransformations::AsNumeric(groups[not.na], binary = FALSE)))
            not.na <- not.na[groups.ord]
    
            groups <- as.character(groups)
            g.list <- unique(groups[not.na])
        }
        return(TrimWhitespace(g.list))    
    }
    if (chart.type == "Pie")
    {
        if (type == "Pie subslice")
            return(TrimWhitespace(rownames(data)))
        if (length(dim(data)) > 1)
            return(TrimWhitespace(colnames(data)))
        return(TrimWhitespace(names(data)))
    }
    if (multi.color.series || chart.type %in% c("Pyramid", "Bar Pictograph", "Donut"))
    {
        if (length(dim(data)) > 1)
            return(TrimWhitespace(rownames(data)))
        return(TrimWhitespace(names(data)))
    }
    if (NCOL(data) == 1 && !is.null(attr(filter, "label")) && length(filter) > 1)
        return(TrimWhitespace(attr(filter, "label")))
    else if (length(dim(data)) < 2)
        return(NULL)
    else
        return(TrimWhitespace(colnames(data)))
    
}


#' Identify colors to be used with brands in the data
#' 
#' @description This function should not called directly, but instead be called via
#'  \code{\link{GetVectorOfColors}} instead. It remains an exported function 
#'   because it is used in older versions of the Standard R pages
#' @inherit GetBrandsFromData
#' @param template A list specifying color palettes and other visualization options.
#' @param input.data Input data for the visualization. The is usually a vector or table. 
#'   It should be normalized from \code{flipChart::PrepareData}.
#' @param brand.names If a vector of names is provided then only \code{template} is used. 
#' @export
GetBrandColors <- function(template, input.data = NULL, filter = NULL, chart.type = "", 
                           scatter.colors.column = 4, multi.color.series = FALSE, brand.names = NULL)
{
    if (is.null(template))
        return("Default colors")
   
    if (is.null(brand.names))
        brand.names <- GetBrandsFromData(input.data, filter, chart.type, scatter.colors.column, multi.color.series)
  
    # In new version of the 'Create Template' Standard R Page, the brand colors is always
    # created from template$colors. But in older versions they are separate elements of the
    # template. So template$colors should not be altered in this function
    if (is.null(template$brand.colors))
        template$brand.colors <- template$colors
    
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


#' Print an appearance template object
#'
#' @param  x An object of class \code{AppearanceTemplate}.
#' @param ... Not used.
#' @importFrom flipFormat ShowTemplateOptions
#' @export
print.AppearanceTemplate <- function(x, ...)
{
    tmp.colors <- x$colors
    if (!is.null(x$colors))
    {
        check.col <- tryCatch(col2rgb(x$colors[1]), error=function(cond){NA})
        if (is.na(check.col[1]))
            tmp.colors <- ChartColors(10, x$colors)
    }
    ShowTemplateOptions(tmp.colors, x$brand.colors, x$global.font, x$fonts, x$global.number.font, x$number.fonts)
}
    
    
