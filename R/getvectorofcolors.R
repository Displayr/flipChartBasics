#' Identify colors for creating a chart with specified input data.
#'
#' @description This function takes into account a range of parameters
#'  including the chart type and other charting parameters, visualization template,
#'  and other colors specified through the \code{palette}.
#' @inherit GetBrandsFromData
#' @inherit GetBrandColors
#' @param palette Specifies the color vector to output. It can be;
#' (1) A named palette from grDevices, RColorBrewer colorspace, or colorRamps;
#' (2) "Group colors" - this option returns a value of NULL so that the charts will use
#'   defaults specified instead the charting function.
#'   It is usually only applicable for the line of best fit (i.e. apply
#'   the colors for each data series to their respective live of best fit), or
#'   Pie subslice (create a gradient for the subslices based centered on the color of
#'   the main (inner) slice. No checks are applied because this option is only
#'   made available in the GUI controls when this option is relevant.
#' (3) A vector of colors which will be recycled to length \code{number.colors.needed}; or
#' (4) one of \code{"Custom color"}, \code{"Custom gradient"} or \code{"Custom palette"}.
#' The last option gives the user greater control via additional parameters (see below).  If not specified, the colors used
#' are c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#4473C5", "#70AD46",
#'             "#255F91", "#9E480D", "#636365", "#987300", "#26408B", "#42682B")
#' @param palette.custom.color A single color provided as a hex or character string.
#'  Only used if \code{palette} is \code{"Custom color"}. The output vector will
#'  consist of \code{custom.color} repeated to the desired length
#'  (no interpolation).
#' @param palette.custom.gradient.start A color specifying the start of
#'  the gradient when \code{palette} is set to \code{"Custom gradient"}.
#' @param palette.custom.gradient.end A color specifying the end of the
#'  gradient when \code{palette} is set to \code{"Custom gradient"}.
#' @param palette.custom.palette A vector or comma separated list of colors
#'  which will be recycled to the desired length unless \code{color.values}
#'  is provided in which case the whole vector will be used to construct
#'  a color scale.
#' @param color.values An optional numeric vector or matrix which can
#'  be used with gradual palettes (either a custom gradient or one of
#'  the sequential color palettes). The names of the vector or matrix
#'  should be the same as the names of \code{input.data}, and
#'  the colorramp is constructed across the range of the values.
#' @param small.multiples Logical indicating whether the \code{color.values}
#'  are being applied to for small multiples (a matrix of colors is
#'  created instead of a vector)

#' @importFrom grDevices colorRamp
#' @importFrom flipU StopForUserError
#' @return A vector or matrix of colors (as hex codes) with the appropriate number of colors
#'  taking into account the input.data, chart.type, the template and other parameters.
#' @export
GetVectorOfColors <- function (template,
                               input.data,
                               filter,
                               chart.type,
                               scatter.colors.column = FALSE,
                               multi.color.series = FALSE,
                               palette = NULL,
                               palette.custom.color = NULL,
                               palette.custom.gradient.start = NULL,
                               palette.custom.gradient.end = NULL,
                               palette.custom.palette = NULL,
                               color.values = NULL,
                               small.multiples = FALSE,
                               type = "Series")
{
    if (is.null(palette))
        return (NULL)
    if (palette == "Group colors")
        return (NULL)
    if (type == "Pie subslice" && chart.type != "Pie")
        return (NULL)
    if (type == "Pie subslice" && length(dim(input.data)) < 2)
        return (NULL)

    # This step converts "Named colors" into a vector
    # But leaves everything else unchanged
    if (is.null(template$brand.colors) && !is.null(names(template$colors)))
        template$brand.colors <- template$colors
    tmp.palette <- GetPalette(palette, template)

    # Get vector of colors
    index <- if (type == "Pie subslice") 2 else 1
    if (!is.null(color.values) && !is.null(palette.custom.palette))
    {
        unordered.colors <- TextAsVector(palette.custom.palette)
    } else
    {
        num.colors <- GetNumColors(input.data, chart.type, scatter.colors.column, multi.color.series)[[index]]
        unordered.colors <- ChartColors(num.colors, given.colors = tmp.palette,
            custom.color = palette.custom.color,
            custom.gradient.start = palette.custom.gradient.start,
            custom.gradient.end = palette.custom.gradient.end,
            custom.palette = palette.custom.palette,
            silent = chart.type %in% c("Pie", "Donut"),
            silent.single.color = multi.color.series ||
                chart.type %in% c("Bar Pictograph", "Time Series", "Pyramid"))

    }
    series.names <- GetBrandsFromData(input.data, filter, chart.type,
        scatter.colors.column, multi.color.series, type)
    use.named.colors <- (palette %in% c("Default or template settings", "Brand colors")) &&
        (!is.null(template$brand.colors))
    use.named.vector <- grepl("Custom palette", palette) &&
         !is.null(names(palette.custom.palette))

    # Take the last color of template$brand.colors
    # instead of assuming a name for missing values
    missing.color <- if (!is.null(template$brand.colors)) rev(template$brand.colors)[1]
                     else                                 "#CCCCCC"

    if (length(series.names) == 0 && use.named.colors)
    {
        msg <- "The template contains named colors but the data series is unnamed."
        if (chart.type %in% c("Bar", "Column"))
            msg <- paste(msg, "Try selecting 'Data Series' > 'Multiple colors within a single series'. ")
        warning(msg)
        return(missing.color)
    }

    if (length(series.names) > 0 && (use.named.colors || use.named.vector))
    {
        # Return a vector of colors where the names are in the
        # same order as needed for the data
        # We can't use MatchTable because it doesn't handle
        # names which are missing from unordered.colors
        named.palette <- NULL
        if (!is.null(template$brand.colors))
            named.palette <- template$brand.colors
        else
            named.palette <- palette.custom.palette

        missing.names <- setdiff(series.names, names(named.palette))
        if (length(missing.names) > 0)
        {
            tmp.entries <- rep(missing.color, length(missing.names))
            names(tmp.entries) <- missing.names
            named.palette <- c(named.palette, tmp.entries)
        }

        result <- named.palette[series.names]
        ind <- which(is.na(names(result)))
        if (length(ind) > 0)
            names(result)[ind] <- series.names[ind]
        return(checkColors(result))
    }
    if (!is.null(color.values))
    {
        is.2d <- NCOL(color.values) > 1 && isTRUE(small.multiples)
        vals <- if (is.2d) MatchTable(color.values, ref.table = input.data, x.table.name = "Color values")
                else       MatchTable(color.values, ref.names = series.names, x.table.name = "Color values")
        if (!is.numeric(vals))
            StopForUserError("Color values must be numeric")
        if (any(is.na(vals)))
            StopForUserError("Color values cannot contain missing values")
        vals <- (vals - min(as.numeric(vals), na.rm = TRUE))
        vals <- vals/max(as.numeric(vals), na.rm = TRUE)

        color.fun <- colorRamp(unordered.colors)
        color.scale <- rgb(color.fun(as.numeric(vals)), maxColorValue = 255)
        if (NCOL(vals) > 1)
            color.scale <- matrix(color.scale, ncol = NCOL(vals))
        return(color.scale)
    }
    return(unordered.colors)
}
