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
#'     processed using \code{\link[flipTables]{AsBasicTable}}
#' @param formTables list of array; each component is assumed to be a
#'     Qtable and will be processed using
#'     \code{\link[flipTables]{AsBasicTable}}
#' @param formBinary a PickAny Multi Q variable
#' @param pasted list of length six; the first component of which is
#'     assumed to be from a user-entered/pasted table; will be
#'     processed by \code{\link{ParseUserEnteredTable}}
#' @param raw.data data.frame, containing Variables from a Q/Displayr
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
#' @details It is assumed that only one of \code{pasted},
#'     \code{formTable}, \code{formTables}, \code{formBinary},
#'     \code{raw.data} is non-NULL.  They are checked for nullity in
#'     that order
#' @importFrom flipTransformations ParseUserEnteredTable
#'     RemoveRowsAndOrColumns
#' @importFrom flipTables BasicTable
#' @importFrom flipData TidyRawData
#' @importFrom flipFormat Labels Names
#' @return If possible, a named vector or matrix, or if that is not
#'     posible or a data.frame is requested, a data.frame
#' @export
#' @seealso \code{\link[flipTables]{AsBasicTable}},
#'     \code{\link[flipData]{TidyRawData}},
#'     \code{\link[flipTransformations]{ParseUserEnteredTable}}
PrepareData <- function(formChartType, subset = TRUE, weights = NULL,
                        formTable = NULL, formTables = NULL, formBinary = NULL,
                        pasted = list(NULL),
                        raw.data = NULL,
                        formTranspose = FALSE,
                        missing = "Exclude cases with missing data",
                        row.names.to.remove = c("NET", "SUM"), col.names.to.remove = c("NET", "SUM"),
                        show.labels = TRUE)
{
    is.pasted <- !is.null(pasted[[1L]])
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
    }else if (is.raw.data)
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
                aggregateDataForCharting(data, weights, formChartType)
            }
            else if(inherits(data, "list"))
            {  # user provided multiple existing tables
                lapply(data, BasicTable, row.names.to.remove = row.names.to.remove,
                       col.names.to.remove = col.names.to.remove)
            }
            else
                BasicTable(data, row.names.to.remove = row.names.to.remove,
                             col.names.to.remove = col.names.to.remove)

    ## Switching rows and columns
    if (isTRUE(formTranspose))
        data <- t(data)

    list(data = data,
         weights = weights,
         chart.type = switch(formChartType,
                                   "Venn Diagram" = "Venn",
                                   "Stream Graph" = "Streamgraph",
                                   "Column Chart" = "ColumnChart",
                                   "Stacked Column Chart" = "StackedColumnChart",
                                   "Pie Chart" = "PieChart",
                                   "Scatter Plot" = "LabeledScatterChart"

                             ))
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
        if (NCOL(data) == 1)
        {
            out <- flipStatistics::WeightedTable(data[[1]]) #, weights)
            out <- prop.table(out) * 100
            out <- as.matrix(out)
        }
        else if (ncol(data) == 2)
        {
            names(data) <- c("x", "y")
            data$w <- if (is.null(weights)) rep.int(1L, nrow(data)) else weights
            out <- flipStatistics::Table(w  ~ x + y, data = data, FUN = sum)
            if (chart.type == "Pie") # Total %
                out = prop.table(out, 1:2) * 100
            else # Column %
                out <- prop.table(out, 2) * 100
         }
    }
    out
}

