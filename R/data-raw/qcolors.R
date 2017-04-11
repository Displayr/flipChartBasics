#qColors <- c(grDevices::rgb(91, 155, 213, 255, maxColorValue = 255), # blue
#             grDevices::rgb(237, 125, 49, 255, maxColorValue = 255), # orange
#             grDevices::rgb(165, 165, 165, 255, maxColorValue = 255), # grey
#             grDevices::rgb(30, 192, 0, 255, maxColorValue = 255), # yellow
#             grDevices::rgb(68, 114, 196, 255, maxColorValue = 255), # darker blue
#             grDevices::rgb(112, 173, 71, 255, maxColorValue = 255), # green
#             grDevices::rgb(37, 94, 145, 255, maxColorValue = 255), # even darker blue
#             grDevices::rgb(158, 72, 14, 255, maxColorValue = 255), # blood
#             grDevices::rgb(99, 99, 99, 255, maxColorValue = 255), # dark grey
#             grDevices::rgb(153, 115, 0, 255, maxColorValue = 255), # brown
#             grDevices::rgb(38, 68, 120, 255, maxColorValue = 255), # very dark blue
#             grDevices::rgb(67, 104, 43, 255, maxColorValue = 255), # darker green
#             grDevices::rgb(0, 0, 0, 255, maxColorValue = 255), # black
#             grDevices::rgb(255, 35, 35, 255, maxColorValue = 255)) # red

qColors <- c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#4473C5", "#70AD46", 
             "#255F91", "#9E480D", "#636365", "#987300", "#26408B", "#42682B")

devtools::use_data(qColors, internal = FALSE, overwrite = TRUE)
devtools::use_data(qColors, internal = TRUE, overwrite = TRUE)

make.table <- function(x, y, label = "Series", row = TRUE)
{
    output <- table(x,y)
    if (row)
        rownames(output) <- paste("Series", 1:length(x))
    else
        colnames(output) <- paste("Series", 1:length(y))

    output
}

x.data = c("A","B","C","D","E")

y.data = c(1, 2, 3, 4, 5)

var1 <- c(1, 2, 3, 4, 5)

var2 <- c(5, 4, 3, 2, 1)

var3 <- c(3, 3, 3, 3, 3)

var4 <- c(3, 4, 5, 2, 1)

var5 <- c(5, 4, 2, 3, 1)

alpha.five <- LETTERS[1:5]

logic.vector <- c(TRUE, FALSE, TRUE, TRUE, FALSE)

logic.vector.named <- c(A = TRUE, B = FALSE, C = TRUE, D = TRUE, E = FALSE)

named.vector.a <- c("A" = 1, "B" = 2, "C" = 3)

named.vector.b <- c("D" = 3, "E" = 2, "F" = 1)

factor.a <- factor(x.data)

factor.b <- factor(y.data)

weight5 <- c(2, 1.5, 1, 0.5, 0.1)
weight3 <- c(2, 1, 0.1)

x.dates <- c(1440236400000,1450236400000,1460236400000,1470236400000,1480236400000)
x.dates <- as.POSIXct(x.dates/1000, origin = "1970-01-01")

z <- matrix(1:5, ncol = 1, dimnames = list(x = LETTERS[1:5], series = "Series 1"))

character.matrix <- matrix(LETTERS[1:20], ncol = 2)
colnames(character.matrix) <- LETTERS[1:2]
rownames(character.matrix) <- LETTERS[1:10]

good.examples <- list("1 A named vector becomes a ChartMatrix" = list(Y = c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5), X = NULL, transpose = FALSE, n.row = 1, n.columns = 5),
                      "2 A single column table becomes a chart matrix" = list(X = NULL, Y = make.table(x.data, var3), transpose = FALSE, n.row = 1, n.columns = 5),
                      "3 A numeric matrix with one column becomes a chart matrix" = list(X = NULL, Y = z, transpose = FALSE, n.row = 1, n.columns = 5),
                      "4 A table with one row becomes a chart matrix" = list(X = NULL, Y = make.table(var3, x.data, row = FALSE), transpose = TRUE, n.row = 1, n.columns = 5),
                      "5 One numeric or integer vector and one character vector become a chart matrix" = list(X = x.data, Y = y.data, transpose = TRUE, n.row = 1, n.columns = 5),
                      "6 One numeric or integer vector and one factor vector become a chart matrix" = list(Y = y.data, X = factor(x.data), transpose = TRUE, n.row = 1, n.columns = 5),
                      "7 One numeric or integer vector and one ordered factor vector become a chart matrix" = list(Y = y.data, X = as.ordered(x.data), transpose = TRUE, n.row = 1, n.columns = 5),
                      "8 One or more numeric or integer vector(s) in a list and one character vector become a chart matrix" = list(Y = list(A = var1, B = var1, C = var1, D = var1, E = var1), X = alpha.five, transpose = TRUE, n.row = 5, n.columns = 5),
                      "9 One or more numeric or integer vector(s) in a data frame and one character vector become a chart matrix" = list(Y = cbind(var1, var1, var1, var1, var1), X = alpha.five, transpose = TRUE, n.row = 5, n.columns = 5),
                      "10 One or more factors with the same levels become a chart matrix" = list(X = factor.a, Y = factor.a, transpose = TRUE, n.row = 5, n.columns = 5),
                      "11 One numeric or integer variable and one date variable become a chart matrix" = list(Y = var1, X = x.dates, transpose = TRUE, n.row = 1, n.columns = 5, aggregate.period = "month"),
                      "12 X can take a logic vector" = list(Y = var1, X = logic.vector, n.row = 1, n.columns = 2),
                      "13 Filtered: A named vector becomes a ChartMatrix" = list(Y = c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5), X = NULL, transpose = FALSE, n.row = 1, n.columns = 3, subset = logic.vector),
                      "14 Filtered: A single column table becomes a chart matrix" = list(X = NULL, Y = make.table(x.data, var3), transpose = FALSE, n.row = 1, n.columns = 3, subset = logic.vector),
                      "15 Filtered: A numeric matrix with one column becomes a chart matrix" = list(X = NULL, Y = z, transpose = FALSE, n.row = 1, n.columns = 3, subset = logic.vector),
                      "16 Filtered: One numeric or integer vector and one character vector become a chart matrix" = list(X = x.data, Y = y.data, transpose = TRUE, n.row = 1, n.columns = 3, subset = logic.vector),
                      "17 Filtered: One numeric or integer vector and one factor vector become a chart matrix" = list(Y = y.data, X = factor(x.data), transpose = TRUE, n.row = 1, n.columns = 3, subset = logic.vector),
                      "18 Filtered: One numeric or integer vector and one ordered factor vector become a chart matrix" = list(Y = y.data, X = as.ordered(x.data), transpose = TRUE, n.row = 1, n.columns = 3, subset = logic.vector),
                      "19 Filtered: One or more numeric or integer vector(s) in a list and one character vector become a chart matrix" = list(Y = list(A = var1, B = var1, C = var1, D = var1, E = var1), X = alpha.five, transpose = TRUE, n.row = 5, n.columns = 3, subset = logic.vector),
                      "20 Filtered: One or more numeric or integer vector(s) in a data frame and one character vector become a chart matrix" = list(Y = cbind(var1, var1, var1, var1, var1), X = alpha.five, transpose = TRUE, n.row = 5, n.columns = 3, subset = logic.vector),
                      "21 Filtered: One or more factors with the same levels become a chart matrix" = list(X = factor.a, Y = factor.a, transpose = TRUE, n.row = 5, n.columns = 5, subset = logic.vector),
                      "22 Filtered: One numeric or integer variable and one date variable become a chart matrix" = list(Y = var1, X = x.dates, transpose = TRUE, n.row = 1, n.columns = 3, aggregate.period = "month", subset = logic.vector))

bad.examples <- list("Y cannot take an unnamed numeric vector without an X input" = list(X = NULL, Y = y.data, transpose = FALSE, n.row = 1, n.columns = 5),
                     "Y cannot take an unnamed logic vector regardless of X-value" = list(X = NULL, Y = logic.vector, transpose = FALSE, n.row = 1, n.columns = 5),
                     "Y cannot take a list of logic vectors (unnamed)" = list(X = NULL, Y = list(logic.vector, logic.vector, logic.vector), transpose = FALSE, n.row = 1, n.columns = 3),
                     "Y cannot take a list of logic vectors (named)" = list(X = NULL, Y = list(logic.vector.named, logic.vector.named, logic.vector.named), transpose = FALSE, n.row = 1, n.columns = 3),
                     "Y cannot take a list of differently named vectors" = list(X = NULL, Y = list(named.vector.a, named.vector.b), transpose = FALSE, n.row = 1, n.columns = 2),
                     "Y cannot take a list of character and integer vectors" = list(X = NULL, Y = list(var1, x.data), transpose = FALSE, n.row = 1, n.columns = 2),
                     "Y cannot take a data frame of mixed character and integer vectors" = list(X = NULL, Y = cbind(var1, x.data), transpose = FALSE, n.row = 2, n.columns = 5),
                     "Y cannot take a list of multiple factors" = list(X = NULL, Y = list(factor.a, factor.b), transpose = FALSE, n.row = 1, n.columns = 2),
                     "Y cannot take a list of mixed integer vectors and factors" = list(X = NULL, Y = list(factor.a, y.data), transpose = FALSE, n.row = 1, n.columns = 2),
                     "Y cannot take a list of mixed character vectors and factors" = list(X = NULL, Y = list(factor.a, x.data), transpose = FALSE, n.row = 1, n.columns = 2),
                     "Y cannot take a character matrix" = list(X = NULL, Y = matrix(LETTERS[1:3]), transpose = FALSE, n.row = 1, n.columns = 3),
                     "Y cannot take a logic matrix" = list(X = NULL, Y = matrix(rep(c(TRUE,FALSE),3)), transpose = FALSE, n.row = 1, n.columns = 6),
                     "Y cannot take a character vector" = list(X = NULL, Y = LETTERS[1:5], transpose = FALSE, n.row = 1, n.columns = 5),
                     "Y cannot take a list of multiple character vectors" = list(X = NULL, Y = list(LETTERS[1:5], LETTERS[6:10], LETTERS[11:15]), transpose = FALSE, n.row = 1, n.columns = 3))

errorAsChartMatrix.examples <- list("Y cannot take a data frame of logic vectors (unnamed)" = list(X = NULL, Y = data.frame(cbind(logic.vector, logic.vector, logic.vector))),
                                    "Y cannot take a data frame of multiple character vectors" = list(X = NULL, Y = data.frame(cbind(LETTERS[1:5], LETTERS[6:10], LETTERS[11:15]))),
                                    "Y cannot take a data frame of mixed integer vectors and factors" = list(X = NULL, Y = data.frame(cbind(factor.a, x.data))),
                                    "Y cannot take a data frame of mixed integer vectors and factors" = list(X = NULL, Y = data.frame(cbind(factor.a, y.data))),
                                    "Y cannot take a data frame of multiple factors" = list(X = NULL, Y = data.frame(factor.a, factor.b)),
                                    "Y cannot take a data frame of differently named vectors" = list(X = NULL, Y = data.frame(cbind(named.vector.a, named.vector.b))),
                                    "Y cannot take a data frame of logic vectors (named)" = list(X = NULL, Y = data.frame(cbind(logic.vector.named, logic.vector.named, logic.vector.named))),
                                    "X cannot take a data frame" = list(Y = var1, X = data.frame(cbind(var1, var2, var3))),
                                    "X cannot take a list" = list(Y = var1, X = list(var1, var2, var3)))

errorIsChartMatrix.examples <- list("Y cannot take a named logic vector" = list(X = NULL, Y = logic.vector.named, transpose = FALSE, n.row = 1, n.columns = 5))


devtools::use_data(character.matrix, qColors, x.data, y.data, var1, var2, var3, var4, var5, alpha.five, logic.vector, logic.vector.named, named.vector.a, named.vector.b, factor.a, factor.b, x.dates, z, good.examples, bad.examples, errorAsChartMatrix.examples, errorIsChartMatrix.examples, internal = FALSE, overwrite = TRUE)
devtools::use_data(qColors, internal = TRUE, overwrite = TRUE)
