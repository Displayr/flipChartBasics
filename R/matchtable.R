#' Match table to elements in the reference table
#'
#' @param x A vector or table containing values to be matched against \code{ref.table}
#' @param ref.table The reference table which determines the order of the output vector
#' @param ref.maindim Row or column.
#' @param x.table.name Name used in the error messages.
#' @param ref.table.name Name used in the error messages.
#' @export

MatchTable <- function(x,
                        ref.table,
                        ref.maindim = "rows",
                        x.table.name = "color values",
                        ref.table.name = "the input data") 
{
    # Extract names from x and reference table
    if (grepl("col", ref.maindim, fixed = TRUE) && length(dim(ref.table)) == 2)
    {
        ref.names <- colnames(ref.table)
        ref.len <- NCOL(ref.table)
    }
    else
    {
        ref.names <- rownames(ref.table)
        ref.len <- NROW(ref.table)
    }
    x.names <- rownames(x)
    if (is.null(x.names) && !is.null(names(x)))
        x.names <- names(x)
    if (is.null(ref.names) && !is.null(names(ref.table)))
        ref.names <- names(ref.table)
    x <- as.numeric(unlist(x))
   
    # If no names are provided just use vector length
    if (is.null(x.names) || is.null(ref.names))
    {
        if (NROW(x) != ref.len)
            stop("The length of ", x.table.name, "(", NROW(x), ") does not match the number of ", 
                ref.maindim, " in ", ref.table.name, "(", ref.len, ").")
        return(x)
    }
    if (any(duplicated(x.names)))
        stop("There are duplicate names in ", x.table.name, ".")

    # Sorting color values to match the row names of the reference table.
    order = match(ref.names, x.names)
    if (any(is.na(order)))
        stop(x.table.name, " is missing values for '", paste(ref.names[which(is.na(order))], collapse = "', '"), "'")
    x = x[order]
    return(x)
}
