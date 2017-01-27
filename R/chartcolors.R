# Translates easy to read palette names to actual palette names
translatePaletteName <- function(color.palette)
{
    long.names <- c("Default colors",
    "Primary colors",
    "Rainbow",
    "Light pastels",
    "Strong colors",
    "Reds, light to dark",
    "Greens, light to dark",
    "Blues, light to dark",
    "Greys, light to dark",
    "Heat colors (red, yellow, white)",
    "Terrain colors (green, beige, grey)")
    
    proper.names <- c("qColors",
    "primary.colors",
    "rainbow_hcl",
    "Set3",
    "Set1",
    "Reds",
    "Greens",
    "Blues",
    "Greys",
    "heat.colors",
    "terrain_hcl")

    if (color.palette[1] == "Default colors")
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
#' @examples
#' c <- c("#FFFFFFFF", "#ABCDEF3D")
#' StripAlphaChannel(c)
#' @export
StripAlphaChannel <- function(hex.colors)
{
    if (nchar(hex.colors[1]) == 9)
        return(gsub("([a-fA-F0-9][a-fA-F0-9]|[a-fA-F0-9][a-fA-F0-9])$", "", hex.colors))
    else
        return(hex.colors)
}

#' Generates a vector of colors
#' 
#' Generates a vector of colors for the number of rows in the passed-in
#' df or matrix chart.matrix; where colors can be either a single named
#' color or a hex color, any code that generates a vector of named colors
#' or a vector of hex colors (but not a mixed vector), or a single named
#' color palette from either of the packages grDevices, RColorBrewer,
#' colorspace, or colorRamps.  If a single color is provided, and more are
#' needed, then a gradient will be calculated towards white. If more than
#' one but fewer than needed are provided, a gradient will be calculated
#' between the provided colors.
#'
#' Alpha channels are ignored and set to 255/FF.
#'
#' @param number.colors.needed Integer; number of colors to generate.
#' @param given.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param reverse Logical; if the output color vector shour be reversed.
#' @param palette.start A numeric in [0,1] specifying the start position of the palette
#' @param palette.end A numeric in [0,1] specifying the end position of the palette
#' @param trim.light.colors When selected, palette.start and palette.end will be set to remove light colors in the monochrome palettes (\code{"Blues","Greens","Greys","Oranges","Purples","Reds"}).
#' @examples
#' ChartColors(number.colors.needed = 5, given.colors = c("blue", "orange", "green"))
#' ChartColors(number.colors.needed = 5, given.colors = "blue")
#' ChartColors(number.colors.needed = 5, given.colors = "#9CFF73")
#' ChartColors(number.colors.needed = 5, given.colors = "Set3", reverse = TRUE)
#' plot(1:10,1:10,pch=19, col=ChartColors(10,"Reds",trim.light.colors=TRUE, reverse=TRUE))
#' @export
ChartColors <- function(number.colors.needed, given.colors = qColors, reverse = FALSE, palette.start = 0, palette.end = 1, trim.light.colors = FALSE) 
{   
    if (palette.start < 0 || palette.start > 1)
        stop("palette.start must be a number between 0 and 1\n")
    
    if (palette.end < 0 || palette.end > 1)
        stop("palette.end must be a number between 0 and 1\n")
    
    if (!(palette.start < palette.end))
        stop("palette.start must be smaller than pallete.end\n")
    
    given.colors <- translatePaletteName(given.colors)
        
    if (trim.light.colors && given.colors %in% c("Blues","Greens","Greys", "Oranges","Purples","Reds"))
    {
        palette.start <- 0 + (0.2 * !reverse)
        palette.end <- 1 - (0.2 * reverse)
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

    if (sum(valid.color.names) == number.colors)
        color.type.named.R <- TRUE

    # Can be a single named gr palette
    all.grcolor.palettes <- c("rainbow", "heat.colors", "terrain.colors", "topo.colors", "cm.colors")

    if (number.colors == 1 && length(all.grcolor.palettes[all.grcolor.palettes == given.colors[1]]) == 1)
        grcolor.palette <- TRUE

    # Can be a single named RColorBrewer color/range
    all.brewer.palettes <- rownames(RColorBrewer::brewer.pal.info)

    if (number.colors == 1 && length(all.brewer.palettes[all.brewer.palettes == given.colors[1]]) == 1)
    {
        library(RColorBrewer)
        brewer.palette <- TRUE
    }
    
    # Can be a single specified colorRamp palette
    all.ramp.palettes <- c("ygobb", "primary.colors", "matlab.like2", "matlab.like", "magenta2green", "cyan2yellow", "blue2yellow", "green2red", "blue2green", "blue2red")

    if (number.colors == 1 && length(all.ramp.palettes[all.ramp.palettes == given.colors[1]]) == 1)
    {
        library(colorRamps)
        ramp.palette <- TRUE
    }
        
    # Can be a single specified colorSpace palette
    all.space.palettes <- c("diverge_hsv", "diverge_hcl", "terrain_hcl", "heat_hcl", "sequential_hcl", "rainbow_hcl")

    if (number.colors == 1 && length(all.space.palettes[all.space.palettes == given.colors[1]]) == 1)
    {
        library(colorspace)
        space.palette <- TRUE
    }
    
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

    ## for RColorBrewer, work out the number of available colors in the palette, and if less than needed, then use
    ## colorRampPalette(brewer.pal(11,"Spectral"))(100)) where the 11 is the max number of items in the RCB palette, and the 100
    ## is the number of colors needed.
    if (brewer.palette)
    {
        max.brewer.colors <- RColorBrewer::brewer.pal.info[given.colors, 1]
        
        ## Must have at least three colors returned from R Color Brewer, else warning message
        if (num2 <= max.brewer.colors)
            chart.colors <- RColorBrewer::brewer.pal(max(3,num2), given.colors)
        else
            chart.colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(max.brewer.colors, given.colors))(max(num2,3))
    }

    ## IF 1 color is specified, and more are needed, then that color is used as the starting point for a gradient,
    ## which gets progressively lighter.
    ## If more than 1 color is specified, then a grandient is created between the specified colors until we have
    ## enough to cover the number of series.
    ## If number of colors is the same as the series needed, then we just take the given colors.
    if (hex.colors || color.type.named.R)
    {
        if (number.colors == 1 && num2 > number.colors)
        {
            base.colors <- grDevices::col2rgb(given.colors[1])

            col.vector <- grDevices::rgb(base.colors[1], base.colors[2], base.colors[3], 255, maxColorValue = 255)

            for (i in 1:num2 + 1) {
                red.factor <- ((255 - base.colors[1]) / (num2 + 1)) * i
                green.factor <- ((255 - base.colors[2]) / (num2 + 1)) * i
                blue.factor <- ((255 - base.colors[3]) / (num2 + 1)) * i

                col.vector <- c(col.vector, grDevices::rgb(base.colors[1] + red.factor, base.colors[2] + green.factor, base.colors[3] + blue.factor, 255, maxColorValue = 255))
            }

            chart.colors <- col.vector[1:num2]

        }
        else if (number.colors >= 2 && num2 > number.colors)
            chart.colors <- grDevices::colorRampPalette(given.colors)(num2)
        else if (number.colors >= num2)
            chart.colors <- given.colors
    }
    if (reverse)
        chart.colors <- rev(chart.colors)
    
    # Trim colors to the selected part of the palette
    n.discard <- round(num2 * palette.start)
    if (!hex.colors && n.discard > 0)
        chart.colors <- chart.colors[-(1:n.discard)]
    res <- chart.colors[1:number.colors.needed]
    return(StripAlphaChannel(res))
}