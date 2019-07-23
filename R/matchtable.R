#' Match table to elements in the reference table
#'
#' @param x A vector or table containing values to be matched against \code{ref.table}
#' @param ref.table The reference table which determines the order of the output vector
#' @param ref.maindim Row or column.
#' @param ref.names Alternatively, a vector of names can be provide instead of \code{ref.table} and \code{ref.maindim}.
#' @param x.table.name Name used in the error messages.
#' @param ref.table.name Name used in the error messages.
#' @export

MatchTable <- function(x,
                        ref.table = NULL,
                        ref.maindim = "rows",
                        ref.names = NULL,
                        x.table.name = "",
                        ref.table.name = "input data") 
{
    if (is.null(ref.names) && !is.null(ref.table))
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
            ref.len <- NROW(ref.table) # this works even if rownames is null
        }
        if (is.null(ref.names) && !is.null(names(ref.table)))
            ref.names <- names(ref.table)
    } else
        ref.len <- length(ref.names)
    
    ref.len <- max(1, ref.len)
    x.names <- rownames(x)
    if (is.null(x.names) && !is.null(names(x)))
        x.names <- names(x)

    # Check length and names 
    if (nchar(x.table.name) > 0)
        x.table.name <- paste0(x.table.name, ": ")
    if (length(ref.names) == 0 && !is.null(x.names))
    {
        warning(x.table.name, "Names were ignored as input data is unnamed")
        x.names <- NULL
    }
    if (is.null(x.names) && NCOL(x) == 1)
    {
        if (length(x) > ref.len)
            warning(x.table.name, "Values (", length(x), ") were truncated to match input data (", ref.len, ").")
        if (length(x) < ref.len)
            warning(x.table.name, "Values (", length(x), ") were recycled to match input data (", ref.len, ").")
        return(rep(x, length = ref.len))
    }
    
    if (!is.null(x.names))
    {
        if (any(duplicated(x.names)))
            stop(x.table.name, "Names must be unique.")

        # Sorting color values to match the row names of the reference table.
        ref.names <- TrimWhitespace(ref.names)
        x.names <- TrimWhitespace(x.names)
        order = match(ref.names, x.names)
        if (any(is.na(order)))
            stop(x.table.name, "Missing values for '", paste(ref.names[which(is.na(order))], collapse = "', '"), "'")

    } else
        order <- 1:ref.len

    if (length(dim(x)) < 2)
        x <- x[order]
    else
        x <- x[order,]

    if (length(dim(x)) == 2 && !is.null(ref.table) && ref.maindim == "rows")
    {
        # match columns of x as well
        if (!is.null(colnames(ref.table)) && !is.null(colnames(x)))
        {
            order <- match(colnames(ref.table), colnames(x))
            x <- x[,order]
        } else
            x <- x[,1:NCOL(ref.table)]
    }
    return(x)
}
