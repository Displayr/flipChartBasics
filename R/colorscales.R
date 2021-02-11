#' Construct color scale for a numeric value
#'
#' @param x A numeric vector specifying the colors returne
#' @param min.x A numeric value representing the smallest value on the color scale. Values in 
#'  \code{x} smaller than this will be treated as \code{min.x}.
#' @param max.x A numeric value representing the largest value on the color scale. Values in 
#'  \code{x} larger than this will be treated as \code{max.x}.
#' @param mid.x A numeric value representing the midpoint on the color scale.
#' @param min.color The color representing the smallest value.
#' @param mid.color The color representing the mid point on the color scale.
#' @param max.color The color representing the max point on the color scale.
#' @export 

MapToColors <- function(x, # A vector of values or an integer indicating the number of values
                        min.x = if (length(x) == 1) 1 else min(x), # The value represented by min.color
                        max.x = max(x), 
                        mid.x = NULL, # Specify a value for two color divergent scales
                        min.color = "white",
                        mid.color = "Displayr grey",
                        max.color = "Displayr blue")                       
{    
    if (length(x) == 1)
        x <- 1:x
    else
    {
        x[x < min.x] <- min.x
        x[x > max.x] <- max.x
    }
            
    if (is.null(mid.x)) 
    {
        scaled.x <- (x - min.x) / (max.x - min.x)
        min.color <- asColorspace(min.color)[[1]]
        mid.color <- asColorspace(mid.color)[[1]]
        max.color <- asColorspace(max.color)[[1]]
        scaled.x <- c(0, scaled.x, 1) #Ensuing all range is scaled
        cols <- diverging.colormap(scaled.x, min.color, max.color)
        cols <- cols[2:(length(scaled.x) - 1), ]
        colors <- rgb(cols[,1], cols[,2], cols[, 3])
        if (!is.null(names(x)))
            names(colors) <- names(x)
        return(colors)
    }
    
    if (mid.x > min(x))
    {
        lower.colors <- MapToColors(x[x <= mid.x], min.x = min.x, max.x = mid.x, min.color = min.color, max.color = mid.color)
        if (mid.x >= max(x)) # 
            return(lower.colors)
    }
    upper.colors <- MapToColors(x[x >= mid.x], min.x = mid.x, max.x = max.x, min.color = mid.color, max.color = max.color)
    if (mid.x <= min(x))
        return(upper.colors)
    colors <- rep(NA, length(x))
    colors[which(x <= mid.x)] <- lower.colors
    colors[which(x >= mid.x)] <- upper.colors
    if (!is.null(names(x)))
        names(colors) <- names(x)
    colors
}

# This function is based on Kenneth Moreland's code for creating Diverging Colormaps.
# Matlab code created by Andy Stein. Translated to R by Jose Gama.
# s is a vector that goes between zero and one
# rgb1,rgb2 are objects from the colorspace package
# RGB, sRGB, HLS, HSV, LAB, LUV, PolarLAB, PolarLUV, XYZ
# outColorspace is the color space for the output

#' @importFrom colorspace LAB
#' @importFrom methods as
#' @importFrom verbs Sum
diverging.colormap <- function(s, rgb1, rgb2, outColorspace = 'sRGB')
{
    LabToMsh<-function(Lab) 
    {
        L<-Lab@coords[1]
        a<-Lab@coords[2]
        b<-Lab@coords[3]
        M <- sqrt(L*L + a*a + b*b)
        s <- (M > 0.001) * acos(L/M)
        h <- (s > 0.001) * atan2(b,a)
        if (!is.finite(s))
            s <- 0
        if (!is.finite(h))
            h <- 0
        c(M,s,h)
    }
    MshToLab<-function(Msh)
    {
        M<-Msh[1]
        s<-Msh[2]
        h<-Msh[3]
        L <- M*cos(s)
        a <- M*sin(s)*cos(h)
        b <- M*sin(s)*sin(h)
        LAB(L,a,b)
    }

    AngleDiff<-function(a1, a2)
    {
        # Given two angular orientations, returns the smallest angle between the two.
        v1<-matrix(c(cos(a1), sin(a1)),1,2,byrow=TRUE)
        v2<-matrix(c(cos(a2), sin(a2)),1,2,byrow=TRUE)
        x<-acos(Sum(v1 * v2, remove.missing = FALSE))
        x
    }
    AdjustHue<-function(msh, unsatM)
    {
        # For the case when interpolating from a saturated color to an unsaturated
        # color, find a hue for the unsaturated color that makes sense.
        if (msh[1] >= unsatM-0.1  ) {
            # The best we can do is hold hue constant.
            h <- msh[3]
        } else {
            # This equation is designed to make the perceptual change of the interpolation to be close to constant.
            hueSpin <- (msh[2]*sqrt(unsatM^2 - msh[1]^2)/(msh[1]*sin(msh[2])))
            # Spin hue away from 0 except in purple hues.
            if (msh[3] > -0.3*pi) h <- msh[3] + hueSpin else h <- msh[3] - hueSpin
        }
        h
    }
    diverging.map.1val<-function(s, rgb1, rgb2, outColorspace='sRGB')
    {
        # Interpolate a diverging color map
        # s is a number between 0 and 1
        msh1 <- LabToMsh(as(rgb1, "LAB"))
        msh2 <- LabToMsh(as(rgb2, "LAB"))
        # If the endpoints are distinct saturated colors, then place white in between them
        if (msh1[2] > 0.05 & msh2[2] > 0.05 & AngleDiff(msh1[3],msh2[3]) > pi/3)
        {
            # Insert the white midpoint by setting one end to white and adjusting the scalar value.
            Mmid <- max(88.0, msh1[1], msh2[1])
            #Mmid <- max(Mmid)
            if (s < 0.5)
            {
                msh2[1] <- Mmid;  msh2[2] <- 0.0;  msh2[3] <- 0.0;s <- 2.0*s
            } else {
                msh1[1] <- Mmid;  msh1[2] <- 0.0;  msh1[3] <- 0.0; s <- 2.0*s - 1.0
            }
        }
        # If one color has no saturation, then its hue value is invalid.  In this
        # case, we want to set it to something logical so that the interpolation of hue makes sense.
        if ((msh1[2] < 0.05) & (msh2[2] > 0.05)) {
            msh1[3] <- AdjustHue(msh2, msh1[1]) 
        } else if ((msh2[2] < 0.05) & (msh1[2] > 0.05)) {
            msh2[3] <- AdjustHue(msh1, msh2[1])
        }
        mshTmp<-msh1
        mshTmp[1] <- (1-s)*msh1[1] + s*msh2[1]
        mshTmp[2] <- (1-s)*msh1[2] + s*msh2[2]
        mshTmp[3]<- (1-s)*msh1[3] + s*msh2[3]
        # Now convert back to the desired color space
        as(MshToLab(mshTmp),outColorspace)
    }
    dvmap<-matrix(0,length(s),3)
    for (n in 1:length(s)) 
        dvmap[n,]<-diverging.map.1val(s[n], rgb1, rgb2, outColorspace)@coords
    # Modifications by Tim Bock 7 May 2018
    dvmap[dvmap < 0] <- 0
    dvmap[dvmap > 1] <- 1
    dvmap
}

#' @importFrom colorspace RGB
asColorspace <- function(x)
{
    convertColors  <- Vectorize(function(a) {
        switch(a,
               "Displayr red" = colorspace::RGB(250/255, 97/255, 75/255),
                "Displayr blue" = colorspace::RGB(62/255, 125/255, 204/255),
                "Displayr green" = colorspace::RGB(0, 200/255, 200/255),
                "Displayr grey" = colorspace::RGB(.865,.865, .865),
                {
                     cols <- col2rgb(a) / 255
                     RGB(cols[1], cols[2], cols[3])
                 })
     })
    convertColors(x) 
}

