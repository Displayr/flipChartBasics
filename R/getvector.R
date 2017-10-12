#' GetVector
#' 
#' A convenience function that will return a vector of a specified length
#' @param var.names The name of a variable, or a vector of variable names as characters.
#' @param length The desired length of the vector. 
#' @param value The default value of the vector.
#' @return A vector, or list of vectors. If the variable does not exist or is \code{NULL}, the return value will be a vector of form \code{rep(value, length)}.
#' @examples 
#' a <- 1
#' GetVector(c("a","b", "c"), 5, 0)
#' @export

GetVector <- function(var.names, length = NA, value = NA)
{
    res <- list()
    for (i in 1:length(var.names))
    {
        var <- get0(var.names[i])
        if (is.null(var) && is.finite(length) && length > 0)
            var <- rep(value, length)
        res[[var.names[i]]] <- var
    }
    if (length(var.names) == 1)
        return(res[[1]])
    else
        return(res)
}
