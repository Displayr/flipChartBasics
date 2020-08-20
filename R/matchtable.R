#' Match table to elements in the reference table
#'
#' @param x A vector or table containing values to be matched against \code{ref.table}
#' @param ref.table The reference table which determines the order of the output vector
#' @param ref.maindim Row or column.
#' @param ref.names Alternatively, a vector of names can be provide instead of \code{ref.table} and \code{ref.maindim}.
#' @param x.table.name Name used in the error messages.
#' @param ref.table.name Name used in the error messages.
#' @param as.matrix Converts \code{x} to a matrix. This will force all values to be of the same type.
#' @param trim.whitespace Whether to trim leading and trailing whitespace when matching row or column names.
#' @param ignore.case Logical; whether matching of names should ignore upper/lower case differences.
#' @param silent.remove.duplicates Removes duplicates with giving warnings. This is particulary useful when dealing with banners.
#' @importFrom flipFormat ExtractChartData
#' @export

MatchTable <- function(x,
                        ref.table = NULL,
                        ref.maindim = "rows",
                        ref.names = NULL,
                        x.table.name = "",
                        ref.table.name = "input data",
                        as.matrix = TRUE,
                        trim.whitespace = TRUE,
                        ignore.case = TRUE,
                        silent.remove.duplicates = FALSE) 
{
    if (nchar(x.table.name) > 0)
        x.table.name <- paste0(x.table.name, ": ")

    # Convert x into a table-like structure
    x <- ExtractChartData(x)
    if (as.matrix)
    {
        x <- as.matrix(x)
        if (!is.matrix(x))
            stop(x.table.name, "Values should be supplied as a table.")
    }


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
        # Sorting color values to match the row names of the reference table.
        if (trim.whitespace)
        {
            ref.names <- TrimWhitespace(ref.names)
            x.names <- TrimWhitespace(x.names)
        }
        ref.names.with.case <- ref.names
        x.names.with.case <- x.names
        if (ignore.case)
        {
            ref.names <- tolower(ref.names)
            x.names <- tolower(x.names)
        }
        if (any(duplicated(x.names)))
        {
            if (!silent.remove.duplicates)
            {
                dup.names <- unique(x.names.with.case[duplicated(x.names)])
                dup.n <- length(dup.names)
                if (dup.n == 1)
                    warning(x.table.name, "Only the value from the first duplicate of '",
                        dup.names, "' was used. ")
                else if (dup.n > 1)
                    warning(x.table.name, "Only the values from the first duplicate of '",
                        paste(dup.names[-dup.n], collapse = "', '"),
                        "' and '", dup.names[dup.n], "' were used.")
            }
            x.names <- make.unique(x.names)
        }
        order = match(ref.names, x.names)
        ind.na <- which(is.na(order))
        n.na <- length(ind.na)
        if (n.na == 1)
            stop(x.table.name, "The value for '", ref.names.with.case[ind.na], "' is missing.")
        if (n.na > 1)
            stop(x.table.name, "The values for '", paste(ref.names.with.case[ind.na[-n.na]], collapse = "', '"), 
                "' and '", ref.names.with.case[ind.na[n.na]], "' are missing.")
    } else
        order <- 1:ref.len

    if (length(dim(x)) < 2)
        x <- x[order]
    else
        x <- x[order,,drop=FALSE]

    # Optionally match columns (NOT main dimension) if both x and ref.table contain enough info
    if (NCOL(x) > 1 && !is.null(ref.table) && ref.maindim == "rows")
    {
        if (ncol(x) < ncol(ref.table))
        {
            # if the number of columns is x less than ref.table then we can only extract a single column
            # take the last column as this will usually be NET
            column.used.name <- if (!is.null(colnames(x))) paste0("column '", colnames(x)[ncol(x)], "'") else "the last column"
            warning(x.table.name, "Only ", column.used.name, " was used.")
            x <- x[,ncol(x)]
        }
        else if(is.null(colnames(ref.table)) || is.null(colnames(x)))
            x <- x[,1:NCOL(ref.table),drop = FALSE]
        else
        {
            col.ref.names <- colnames(ref.table)
            col.x.names <- colnames(x)
            if (trim.whitespace)
            {
                col.ref.names <- TrimWhitespace(col.ref.names)
                col.x.names <- TrimWhitespace(col.x.names)
            }
            col.ref.names.with.case <- col.ref.names
            if (ignore.case)
            {
                col.ref.names <- tolower(col.ref.names)
                col.x.names <- tolower(col.x.names)
            }
            order <- match(col.ref.names, col.x.names)
            if (any(is.na(order)))
                stop(x.table.name, "Values should either be a single-column table or have the same column names as the input data. ",
                     if (sum(is.na(order)) == 1) "Column for '" else "Columns for '",
                     paste(col.ref.names.with.case[is.na(order)], collapse = "', '"), "' ",
                     if (sum(is.na(order)) == 1) "is" else "are",
                    " missing.")
            x <- x[,order,drop=FALSE]
        }   
    }
    return(x)
}
