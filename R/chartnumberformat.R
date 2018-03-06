#' Generates a d3 number format for use by charting functions.
#' 
#' See https://github.com/d3/d3-format for more information on d3.
#' @param number.format A list of five unnamed items in the following order:
#' 1) The type of number formatting. One of \code{"Automatic"}, \code{"Category"}, \code{"Number"},
#'  \code{"Percentage"}, \code{"Date/Time"}, \code{"Currency"}, \code{"Metric units suffix"}, 
#'  \code{"Scientific"} and \code{"Custom"}.
#' 2) A d3 date format. If specified, this will be used provided type is neither \code{"Automatic"}
#' nor \code{"Category"} nor a custom format is not specified.
#' 3) A custom d3 number format. If specifed this will be used in preference to all other inputs
#' apart from \code{"Automatic"} and \code{"Category"}.
#' 4) A boolean indicating whether to separate thousands with commas.
#' 5) An integer specifying the number of decimal places, or for \code{"Metric units suffix"}, the number
#' of significant digits.
#' @param as.percentages Whether the \code{"Automatic"} formatting should be as percentages.
#' @details Defaults to \code{"Automatic"} if no type is specified.
#' @export
ChartNumberFormat <- function(number.format, as.percentages = FALSE) {
    
    if (is.null(number.format))
        return(NULL)
    
    number.type <- number.format[[1]]
    date.type <- number.format[[2]]
    custom.type <- number.format[[3]]
    separate.thousands <- number.format[[4]]
    decimal.places <- number.format[[5]]

    if (is.null(number.type))
        number.type <- "Automatic"
    
    if (!number.type %in% c("Number", "Percentage", "Date/Time", "Currency",
                            "Metric units suffix","Scientific", "Custom",
                            "Automatic", "Category"))
        stop("Number format not recognized.")

    if (number.type == "Automatic") {
        if (as.percentages && is.null(decimal.places))
            return(".0%")
        if (as.percentages && !is.null(decimal.places))
            return(paste0(".", decimal.places, "%"))
        if (is.null(decimal.places) || decimal.places < 0)
            return("")   # formatting will be handled by chart function depending on data type
        else
            return(paste0(".", decimal.places, "f"))
    }

    if (number.type == "Category")
        return("Category")

    if (!is.null(custom.type))
        return(custom.type)

    if (!is.null(date.type))
        return(switch(date.type,
                      "YY (Year, 2 digit)" = "%y",
                      "DD Mon YY" = "%d %b %y",
                      "DD Month YY" = "%d %B %y",
                      "DD MM YY" = "%d %m %y",
                      "YYYY (Year, 4 digit)" = "%Y",
                      "DD Mon YYYY" = "%d %b %Y",
                      "DD Month YYYY" = "%d %B %Y",
                      "DD MM YYYY" = "%d %m %Y",
                      "Mon DD YY" = "%b %d %y",
                      "Month DD YY" = "%B %d %y",
                      "MM DD YY" = "%m %d %y",
                      "Mon DD YYYY" = "%b %d %Y",
                      "Month DD YYYY" = "%B %d %Y",
                      "MM DD YYYY" = "%m %d %Y",
                      "YY Mon DD" = "%y %b %d",
                      "YY Month DD" = "%y %B %d",
                      "YY MM DD" = "%y %m %d",
                      "YYYY Mon DD" = "%Y %b %d",
                      "YYYY Month DD" = "%Y %B %d",
                      "YYYY MM DD" = "%Y %m %d",
                      "Weekday DD Mon YY" = "%A %d %b %y",
                      "Day DD Mon YY" = "%a %d %b %y",
                      "Weekday Mon DD YY" = "%A %b %d %y",
                      "Day Mon DD YY" = "%a %b %d %y",
                      "HH:MM (24 hr)" = "%H:%M",
                      "HH:MM AM/PM" = "%I:%M %p",
                      "MM DD YY HH:MM" = "%m %d %y %H:%M",
                      "DD MM YY HH:MM" = "%d %m %y %H:%M"))
    
    comma <- if (!is.null(separate.thousands) && separate.thousands == T) {
        "," 
    } else {
        ""
    }
    
    d3.format <- ""
    if (!is.null(decimal.places))
        d3.format <- paste0(comma, ".", decimal.places)
    d3.type <- switch(number.type,
                      "Number" = "f",
                      "Currency" = "f",                      
                      "Percentage" = "%",
                      "Scientific" = "e",
                      "Metric units suffix" = "s")
    return(paste0(d3.format, d3.type))
}

