#' Tidy Data For Use With Displayr Charting Functions
#'
#' For use before calling a Displayr charting function, ensures the
#' data to be used is in a convenient format for plotting
#' @param formChartType character; chart type to be plotted
#' @param subset subset An optional vector specifying a subset of
#'     observations to be used in the fitting process, or, the name of
#'     a variable in \code{data}. It may not be an expression.
#' @param weights An optional vector of sampling weights, or, the name
#'     of a variable in \code{data}. It may not be an expression.
#' @param formTable array; assumed to be a Qtable and will be
#'     processed using \code{\link[flipTables]{AsTidyTabularData}}
#' @param formTables list of array; each component is assumed to be a
#'     Qtable and will be processed using
#'     \code{\link[flipTables]{AsTidyTabularData}}
#' @param formBinary a PickAny Multi Q variable
#' @param pasted list of length six; the first component of which is
#'     assumed to be from a user-entered/pasted table; will be
#'     processed by \code{\link{ParseUserEnteredTable}}
#' @param raw.data data.frame or list, containing Variables from a Q/Displayr
#'     Data Set
#' @param formTranspose logical; should the supplied data be
#'     transposed?
#' @param missing character; One of \code{"Error if missing data"},
#'     \code{"Exclude cases with missing data"} (the default, which is
#'     equivalent to 'complete.cases'), and \code{"Use partial data"},
#'     which removes no data; ignored if raw data is not supplied
#' @param row.names.to.remove character vector of row labels
#'     specifying rows to remove from the returned table; default is
#'     \code{c("NET", "SUM")}
#' @param col.names.to.remove character vector of column labels
#'     specifying columns to remove from the returned table; default
#'     is \code{c("NET", "SUM")}
#' @param show.labels logical; If \code{TRUE}, labels are used for
#'     names in the data output if raw data is supplied
#' @param as.percentages logical; If \code{TRUE}, aggregate values in the output table
#'     are given as percentages summing to 100. If \code{FALSE}, column sums are given.
#' @details It is assumed that only one of \code{pasted},
#'     \code{formTable}, \code{formTables}, \code{formBinary},
#'     \code{raw.data} is non-NULL.  They are checked for nullity in
#'     that order
#' @importFrom flipTransformations ParseUserEnteredTable
#' @importFrom flipTables TidyTabularData RemoveRowsAndOrColumns
#' @importFrom flipData TidyRawData
#' @importFrom flipFormat Labels Names
#' @return If possible, a named vector or matrix, or if that is not
#'     posible or a data.frame is requested, a data.frame
#' @export
#' @seealso \code{\link[flipTables]{AsTidyTabularData}},
#'     \code{\link[flipData]{TidyRawData}},
#'     \code{\link[flipTransformations]{ParseUserEnteredTable}}
PrepareData <- function(formChartType, subset = TRUE, weights = NULL,
                        formTable = NULL, formTables = NULL, formBinary = NULL,
                        pasted = list(NULL),
                        raw.data = NULL,
                        formTranspose = FALSE,
                        missing = "Exclude cases with missing data",
                        row.names.to.remove = c("NET", "SUM"), col.names.to.remove = c("NET", "SUM"),
                        as.percentages = FALSE,
                        show.labels = TRUE)
{
    is.pasted <- !is.null(pasted[[1L]])

    scatter.x.column <- 1
    scatter.y.column <- 2
    scatter.sizes.column <- 3
    scatter.colors.column <- 4
    y.title <- ""

    # Handles a list of variables (not dataframes as some of them may may be null)
    # This case needs to be distinguished from when a list of multiple tables is provided
    if (!is.null(raw.data) && !is.data.frame(raw.data) && (!is.null(raw.data$X) || !is.null(raw.data$Y)))
    {
        labels <- raw.data$labels
        raw.data$labels <- NULL
        if (formChartType == "Scatter Plot")
        {
            scatter.x.column <- 0 + (!is.null(raw.data$X))
            scatter.y.column <- 0 + (!is.null(raw.data$Y)) * (1 + (!is.null(raw.data$X)))
            scatter.sizes.column <- 0 + (!is.null(raw.data$Z)) * (1 + (!is.null(raw.data$X)) + (!is.null(raw.data$Y)))
            scatter.colors.column <- 0 + (!is.null(raw.data$Z2)) * (1 + (!is.null(raw.data$X)) + (!is.null(raw.data$Y)) + (!is.null(raw.data$Z)))
        }
        raw.data <- as.data.frame(Filter(Negate(is.null), raw.data), stringsAsFactors=F)

        if (is.null(labels) && nrow(raw.data) == length(labels))
            rownames(raw.data) <- make.unique(as.character(labels), sep="")
    }
    data <- processDataArgs(pasted = pasted, formTable = formTable, formTables = formTables,
                            formBinary = formBinary, raw.data = raw.data,
                            is.pasted = is.pasted)
    is.raw.data <- attr(data, "raw.data")

    # Processing pasted or manually inputted data.
    if (is.pasted)
    {
        data <- ParseUserEnteredTable(data[[1]],
                                      want.data.frame = data[[2]],
                                      want.factors = data[[3]],
                                      want.col.names = data[[4]],
                                      want.row.names = data[[5]],
                                      us.format = data[[6]])
        if (is.data.frame(data)) # Raw data aka data.frame
            is.raw.data <- TRUE
    } else if (is.raw.data)
    {  # formBinary is non-NULL or raw.data is non-NULL
        data <- flipData::TidyRawData(data, subset = subset, weights = weights,
                                          missing = missing)
        weights <- attr(data, "weights")
    }

    ## Aggregate if raw data or convert to tidy table(s)
    data <- if (is.raw.data)
            {
                data <- RemoveRowsAndOrColumns(data, row.names.to.remove = row.names.to.remove,
                                               column.names.to.remove = col.names.to.remove)
                if (show.labels)
                    names(data) <- Labels(data)
                else
                    names(data) <- Names(data)

                if (!grepl("Scatter", formChartType))
                    y.title <- "Counts"
                aggregateDataForCharting(data, weights, formChartType)
            }
            else if(inherits(data, "list"))
            {  # user provided multiple existing tables
                lapply(data, TidyTabularData, row.names.to.remove = row.names.to.remove,
                       col.names.to.remove = col.names.to.remove)
            }
            else
                TidyTabularData(data, row.names.to.remove = row.names.to.remove,
                             col.names.to.remove = col.names.to.remove)

    ## Switching rows and columns
    if (isTRUE(formTranspose))
        data <- t(data)

    # Convert to percentages - this must happen AFTER transpose and RemoveRowsAndOrColumns
    if (as.percentages)
    {
        ind.negative <- which(data < 0)
        if (length(ind.negative) > 0)
        {
            warning("Percentages calculated ignoring negative values.")
            data[ind.negative] <- 0
        }

        if (is.matrix(data))
            data <- prop.table(data, 2) * 100
        else
            data <- prop.table(data) * 100

        y.title <- "% Share"
    }

    list(data = data,
         weights = weights,
         y.title = y.title,
         scatter.x.column = scatter.x.column,
         scatter.y.column = scatter.y.column,
         scatter.sizes.column = scatter.sizes.column,
         scatter.colors.column = scatter.colors.column)
}

#' Get Data for charting
#'
#' Processes form variables to get first non-NULL
#' @param ... named components of data components
#' @details currently recognized components are formTable,
#' formTables, formBinary, pasted, raw.data
#' @return the first non-NULL component \code{...} with an added attribute
#' indicating if the data is raw data
#' @noRd
processDataArgs <- function(..., is.pasted = FALSE)
{  #
    args <- list(...)
    non.null.idx <- which(!vapply(args, function(x) is.null(unlist(x)), FALSE))[1]
    if (is.na(non.null.idx))
        stop("no data supplied", call. = FALSE)
    data <- args[[non.null.idx]]
    raw.data <- (is.pasted && inherits(data, "list") && isTRUE(data[[2]])) ||
        names(args)[non.null.idx] %in% c("raw.data", "formBinary")
    attr(data, "raw.data") <- raw.data
    data
}

#' Aggregrate Raw Data For Charting
#' @param data \code{data.frame} containing raw data
#' @param weights numeric vector of weights
#' @param chart.type character; type of chart to be plotted
#' @return aggregated data
#' @noRd
#' @importFrom flipStatistics Table WeightedTable
aggregateDataForCharting <- function(data, weights, chart.type)
{
    out <- data
    if (!chart.type %in% c("Scatter Plot", "Bubble Chart"))
    {
        # In tables that show aggregated tables, only the x-axis title is
        # taken from dimnames. But both names should be set in case
        # the table is transposed
        if (NCOL(data) == 1)
        {
            out <- flipStatistics::WeightedTable(data[[1]]) #, weights)
            d.names <- list(names(out), NULL)
            names(d.names) <- c(names(data)[1], "")
            out <- matrix(out, dimnames=d.names)
        }
        else if (ncol(data) == 2)
        {
            tmp.names <- names(data)
            names(data) <- c("x", "y") # temporarily set names for formula
            data$w <- if (is.null(weights)) rep.int(1L, nrow(data)) else weights
            out <- flipStatistics::Table(w  ~  x + y, data = data, FUN = sum)
            names(dimnames(out)) <- tmp.names
         }
    }
    out
}

