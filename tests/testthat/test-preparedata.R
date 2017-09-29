context("PrepareData")

test_that("multiple existing tables case works",
{
    formTables <- list(structure(c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12), .Names = c("Blueberry",
                      "Cherry", "Apple", "Boston Cream", "Other", "Vanilla Cream")),
                        structure(c(42.625, 11.125, 17.875, 9, 2.5, 14.875, 0.75,
                        1.25, 100), .Dim = 9L, statistic = "%", .Dimnames = list(
                            c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
                              "Pepsi Max", "Dislike all cola", "Don't care", "NET")), name = "Preferred cola",
                        questions = c("Preferred cola",
                                      "SUMMARY")))
    out <- PrepareData(formTable = NULL, raw.data = NULL, formTables = formTables, formBinary = NULL,
                       formChartType = "Scatter")
    expect_length(out$data, 2)
    expect_equal(attr(out$data[[2]], "statistic"), "%")
})

test_that("works with pasted data when ask for data.frame", {
    ## list(get0("formPastedData"), get0("formPastedRawData"), get0("formPastedFactor"), get0("formPastedColumnNames"),
    ##      get0("formPastedRowNames"), get0("formPastedDateConvention"))
    dat <- rbind(c("", LETTERS[1:4]), cbind(letters[1:3], matrix(as.character(1:12), 3, 4)))
    pasted <- list(dat, TRUE, TRUE, TRUE, TRUE, TRUE)
    out <- PrepareData(formTable = NULL, raw.data = NULL, formTables = NULL, formBinary = NULL,
                       pasted = pasted, formChartType = "Column")
    expect_is(out$data, "data.frame")
    expect_equal(colnames(out$data), LETTERS[1:4])
}
