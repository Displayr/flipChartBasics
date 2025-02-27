# Translates easy to read palette names to actual palette names
translatePaletteName <- function(color.palette)
{
    long.names <- c("Default colors",
    "Office colors",
    "Primary colors",
    "Rainbow",
    "Light pastels",
    "Strong colors",
    "Colorblind safe colors",
    "Spectral colors (red, yellow, blue)",
    "Spectral colors (blue, yellow, red)",
    "Reds, light to dark",
    "Greens, light to dark",
    "Blues, light to dark",
    "Greys, light to dark",
    "Reds, dark to light",
    "Greens, dark to light",
    "Blues, dark to light",
    "Greys, dark to light",
    "Heat colors (red, yellow, white)",
    "Heat colors (yellow, red)",
    "Terrain colors (green, beige, grey)")

    proper.names <- c("qColors",
    "qColors",
    "primary.colors",
    "rainbow_hcl",
    "Set3",
    "Set1",
    "Set2",
    "Spectral",
    "Spectral reverse",
    "Reds",
    "Greens",
    "Blues",
    "Greys",
    "Reds reverse",
    "Greens reverse",
    "Blues reverse",
    "Greys reverse",
    "heat_hcl",
    "heat_hcl reverse",
    "terrain_hcl")

    if (color.palette[1] %in% c("Default colors", "Office colors"))
        return(qColors)

    if (length(which(color.palette[1] == long.names)) == 0)
        return(color.palette)
    else
        return(proper.names[which(long.names == color.palette[1])])

}

#' Removes the last two characters from a vector of hexadecimal colors
#' that include an alpha channel.
#'
#' @param hex.colors A character vector of colors in hexadeximal format.
#' @param warning.msg Optional warning msg to give if alpha values were present.
#' @examples
#' c <- c("#FFFFFFFF", "#ABCDEF3D")
#' StripAlphaChannel(c)
#' @export
StripAlphaChannel <- function(hex.colors, warning.msg = NULL)
{
    if (length(hex.colors) == 0)
        return(hex.colors)
    if (any(nchar(hex.colors) == 9))
    {
        if (any(nzchar(warning.msg)))
            warning(warning.msg)
        return(substr(hex.colors, 1, 7))
    }
    else
        return(hex.colors)
}


checkColors <- function(xx)
{
    res <- sapply(xx, function(x){tryCatch({ tmp <- col2rgb(x, alpha = TRUE);
        return(rgb(t(tmp[1:3]), alpha = if (tmp[4] == 255) NULL else tmp[4], maxColorValue = 255)) },
        error=function(cond){NA})})
    ind <- which(is.na(res))
    if (length(ind) > 0)
    {
        res[ind] <- "#CCCCCC"
        for (i in ind)
            warning("Invalid color '", names(res)[i], "' replaced with '#CCCCCC'")
    }
    res
}


#' Generates a vector of colors
#'
#' Generates a vector of colors from a palette or vector of colors.
#' Alpha channels are ignored and set to 255/FF.
#'
#' @param number.colors.needed The number of colors to generate.
#' @param given.colors Specifies the color vector to output. It can be;
#' (1) A named palette from grDevices, RColorBrewer colorspace, or colorRamps;
#' (2) A vector of colors which will be recycled to length \code{number.colors.needed}; or
#' (3) one of \code{"Custom color"}, \code{"Custom gradient"} or \code{"Custom palette"}.
#' The last option gives the user greater control via additional parameters (see below).  If not specified, the colors used
#' are c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#4473C5", "#70AD46",
#'             "#255F91", "#9E480D", "#636365", "#987300", "#26408B", "#42682B")
#' @param custom.color A single color provided as a hex or character string. Only used if \code{given.colors}
#' is \code{"Custom color"}. The output vector will consist of \code{custom.color} repeated \code{number.colors.needed}
#' (no interpolation).
#' @param custom.gradient.start A color specifying the start of the gradient when \code{given.colors}
#' is set to \code{"Custom gradient"}.
#' @param custom.gradient.end A color specifying the end of the gradient when \code{given.colors}
#' is set to \code{"Custom gradient"}.
#' @param custom.palette A vector or comma separated list of colors which will be recycled to the desired length.
#' Only used if \code{given.colors} is \code{"Custom palette"}.
#' @param reverse Whether the output color vector should be reversed. Ignored when \code{given.colors}
#' is any of \code{"Custom color"}, \code{"Custom gradient"} or \code{"Custom palette"}.
#' @param palette.start A numeric in [0,1] specifying the start of the palette relative to the full spectrum
#' of \code{given.colors}. The default is 0, which means the palette starts with the first color of \code{given.colors}.
#' @param palette.end A numeric in [0,1] specifying the end of the palette relative to the full spectrum
#' of \code{given.colors}. The default is 1, which means the palette starts with the last color of \code{given.colors}.
#' @param trim.light.colors When selected, palette.start and palette.end will be set to remove light colors in the
#' monochrome palettes (\code{"Blues","Greens","Greys","Oranges","Purples","Reds"}), if this is not already achieved
#' by \code{palette.start} and \code{palette.end}.
#' @param silent Option to hide warnings about the number of colors.
#' @param silent.single.color Option to hide warnings if only a single color is provided when
#' \code{number.colors.needed > 1}. This type of warning is controlled separately from the \code{silent} because
#' there are some chart types for which a single color or a palette can be appropriately specified. In contrast
#' the times when colors need to recycled for fill the palette, or the a palette is used for a single color
#' usually indicates the user has specified some inappropriate options.
#' @examples
#' ChartColors(5, given.colors = c("blue", "orange", "green"))
#' ChartColors(5, given.colors = "blue")
#' ChartColors(5, given.colors = "#9CFF73")
#' ChartColors(5, given.colors = "Set3", reverse = TRUE)
#' plot(1:10,1:10,pch=19, col=ChartColors(10,"Reds",trim.light.colors=TRUE, reverse=TRUE))
#' plot(1:12, 1:12, pch=19, col=ChartColors(12))
#' @import colorRamps
#' @import colorspace
#' @importFrom grDevices col2rgb rgb colorRampPalette
#' @importFrom flipTransformations TextAsVector
#' @importFrom verbs Sum
#' @importFrom flipU StopForUserError
#' @export
ChartColors <- function(number.colors.needed,
                        given.colors,
                        reverse = FALSE,
                        palette.start = 0,
                        palette.end = 1,
                        trim.light.colors = TRUE,
                        custom.color = NA,
                        custom.gradient.start = NA,
                        custom.gradient.end = NA,
                        custom.palette = NA,
                        silent = FALSE,
                        silent.single.color = silent)
{
    default.colors <- FALSE
    if (missing(given.colors))
    {
        given.colors <- qColors
        default.colors <- TRUE
    }
    else if (given.colors[1] %in% c("Default colors", "Office colors"))
        default.colors <- TRUE
    palette.type <- if (default.colors) "Default colors"
                    else                given.colors[1]

    if (is.na(number.colors.needed))
        number.colors.needed <- if (given.colors[1] == "Custom palette") length(TextAsVector(custom.palette))
                                else 10
    if (number.colors.needed%%1 != 0 | number.colors.needed <= 0)
        StopForUserError("'number.colors.needed' must be a positive integer.")

    # Non-palette options
    if (given.colors[1] == "Custom color")
    {
        if (is.na(custom.color))
            StopForUserError("'custom.color' is missing.")
        if (!silent.single.color && number.colors.needed > 1)
            warning("Only a single color specified for multiple series. Consider using 'Custom palette' instead.")
        return (rep(checkColors(custom.color), number.colors.needed))
    }

    if (given.colors[1] == "Custom gradient")
    {
        if (is.na(custom.gradient.start))
            StopForUserError("'custom.gradient.start' is missing.")
        if (is.na(custom.gradient.end))
            StopForUserError("'custom.gradient.end' is missing.")
        color.ends <- c(custom.gradient.start, custom.gradient.end)
        color.ends <- StripAlphaChannel(color.ends, "Alpha values from selected colors ignored in gradient.")
        c.palette <- try(colorRampPalette(color.ends))
        if (inherits(c.palette, "try-error"))
            StopForUserError("Invalid color palette specified.")
        palette <- c.palette(number.colors.needed)
        attr(palette, "palette.type") <- palette.type
        return (palette)
    }
    if (grepl("^Custom palette", given.colors[1]) || given.colors[1] %in% c("R output", "Manual", "Numeric"))
    {
        is.named <- !is.null(names(custom.palette))
        custom.palette <- TextAsVector(custom.palette)
        ind <- which(!is.na(custom.palette) & nchar(custom.palette) > 0)
        custom.palette <- custom.palette[ind]
        if (length(custom.palette) < number.colors.needed && number.colors.needed > 1)
        {
            if (!silent && !is.named)
                warning("Custom palette does not have the number of colors required (",
                    number.colors.needed, "). Colors will be recycled to make up the required number.")
            custom.palette <- paste0(rep("", number.colors.needed), custom.palette)
        }
        if (!is.named && isTRUE(number.colors.needed >= 1))
            return(checkColors(custom.palette)[1:number.colors.needed])
        return(checkColors(custom.palette))
    }

    # The following options assume given.colors is a palette
    if (palette.start < 0 || palette.start > 1)
        StopForUserError("palette.start must be a number between 0 and 1\n")

    if (palette.end < 0 || palette.end > 1)
        StopForUserError("palette.end must be a number between 0 and 1\n")

    if (!(palette.start < palette.end))
        StopForUserError("palette.start must be smaller than palette.end\n")

    given.colors <- translatePaletteName(given.colors)

    if (length(given.colors) == 1 && endsWith(given.colors, "reverse"))
    {
        reverse <- !reverse
        given.colors <- sub(" reverse", "", given.colors)
    }

    if (trim.light.colors && all(given.colors %in% c("Blues", "Greens", "Greys", "Oranges", "Purples", "Reds")))
    {
        palette.start <- max(palette.start, 0 + (0.2 * !reverse))
        palette.end <- min(palette.end, 1 - (0.2 * reverse))
    }

    num2 <- ceiling(number.colors.needed/(palette.end - palette.start))

    # Count the number of supplied colors
    number.colors <- length(given.colors)

    # Can be a single named color, or a vector of named colors, e.g. "blue", or c("blue", "red", "green")
    all.colors <- grDevices::colors()

    # init some vars
    chart.colors <- character()
    color.type.named.R <- FALSE
    grcolor.palette <- FALSE
    brewer.palette <- FALSE
    ramp.palette <- FALSE
    space.palette <- FALSE
    hex.colors <- FALSE

    # Check that provided named colors are part of the native R colors
    valid.color.names <- sapply(given.colors, function(x) length(all.colors[all.colors == x]))

    if (Sum(valid.color.names) == number.colors)
        color.type.named.R <- TRUE

    # Can be a single named gr palette
    all.grcolor.palettes <- c("rainbow", "heat.colors", "terrain.colors", "topo.colors", "cm.colors")

    if (number.colors == 1 && length(all.grcolor.palettes[all.grcolor.palettes == given.colors[1]]) == 1)
        grcolor.palette <- TRUE

    # Can be a single named RColorBrewer color/range
    all.brewer.palettes <- rownames(RColorBrewer::brewer.pal.info)
    if (number.colors == 1 && length(all.brewer.palettes[all.brewer.palettes == given.colors[1]]) == 1)
        brewer.palette <- TRUE

    # Can be a single specified colorRamp palette
    all.ramp.palettes <- c("ygobb", "primary.colors", "matlab.like2", "matlab.like", "magenta2green",
                           "cyan2yellow", "blue2yellow", "green2red", "blue2green", "blue2red")
    if (number.colors == 1 && length(all.ramp.palettes[all.ramp.palettes == given.colors[1]]) == 1)
        ramp.palette <- TRUE

    # Can be a single specified colorSpace palette
    all.space.palettes <- c("diverge_hsv", "diverge_hcl", "terrain_hcl", "heat_hcl", "sequential_hcl",
                            "rainbow_hcl")
    if (number.colors == 1 && length(all.space.palettes[all.space.palettes == given.colors[1]]) == 1)
        space.palette <- TRUE

    # Can be a single hex color, or a vector of hex colors, or anything that returns the same
    # which is useful if the users want to have their own colour brewer, colorRamp, or colorspace palette or the like
    if (grepl("#", given.colors[1], fixed = TRUE))
        hex.colors <- TRUE

    ## usage - for colorSpace, colorRamp, and grDevices colors, use the colname(n), where the n is the number of colors needed.

    if (!color.type.named.R && !grcolor.palette && !brewer.palette && !ramp.palette && !space.palette && !hex.colors)
    {
        warning("The palette or colors you have specified are not valid.  The chart will display with default colors.")
        hex.colors <- TRUE
        given.colors <- qColors
    }

    if (grcolor.palette || ramp.palette || space.palette)
        chart.colors <- get(given.colors)(num2)

    ## For RColorBrewer, work out the number of available colors in the palette, if 1 needed then take middle, if 2 take
    ## first and last, else interploate from full palette.
    if (brewer.palette)
    {
        max.brewer.colors <- RColorBrewer::brewer.pal.info[given.colors, 1]

        ## Must have at least three colors returned from R Color Brewer, else warning message
        if (num2 <= max.brewer.colors)
            chart.colors <- RColorBrewer::brewer.pal(max(3,num2), given.colors)
        else
            chart.colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(max.brewer.colors, given.colors))(max(num2,3))
    }

    # Recycle vector of colors to the desired length
    if (hex.colors || color.type.named.R)
    {
        if (number.colors < num2)
            chart.colors <- paste0(rep("", num2), given.colors)
        else
            chart.colors <- given.colors
    }
    if (reverse)
        chart.colors <- rev(chart.colors)

    # Trim colors to the selected part of the palette
    n.discard <- round(num2 * palette.start)
    if (!hex.colors && n.discard > 0)
        chart.colors <- chart.colors[-(1:n.discard)]
    palette <- chart.colors[1:number.colors.needed]
    attr(palette, "palette.type") <- palette.type
    return(palette)
}

#' Vector of the 12 standard Q colors
#'
#' @format 12 Q colors in hex format.
#' @noRd
qColors <- c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#4473C5", "#70AD46",
             "#255F91", "#9E480D", "#636365", "#987300", "#26408B", "#42682B")
