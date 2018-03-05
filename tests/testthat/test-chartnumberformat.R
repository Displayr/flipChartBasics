context("ChartNumberFormat")

types <- c("Number", "Percentage", "Date/Time", "Currency", "Metric units suffix",
           "Scientific", "Custom", "Category") # removed Automatic for DS-1858
expected <- c(".3f", ".3%", ".3", ".3f", ".3s", ".3e", ".3", "Category")

test_that("ChartNumberFormat", {
    
    for (i in seq(length(types))) {
        expect_equal(ChartNumberFormat(list(types[i], NULL, NULL, FALSE, 3)), expected[i])
    }
    
    expect_equal(ChartNumberFormat(list("Custom", NULL, "%B %d, %Y", FALSE, 0)), "%B %d, %Y")
    expect_equal(ChartNumberFormat(list("Date/Time", "DD Mon YYYY", NULL, FALSE, 0)), "%d %b %Y")
    expect_equal(ChartNumberFormat(list("Number", NULL, NULL, TRUE, 0)), ",.0f")
    expect_equal(ChartNumberFormat(list("Number", NULL, NULL, FALSE, 5)), ".5f")
    expect_error(ChartNumberFormat(list("Unknown", NULL, NULL, FALSE, 5)), "Number format not recognized.")
    expect_equal(ChartNumberFormat(list("Automatic", NULL, NULL, NULL, NULL)), "")
    expect_equal(ChartNumberFormat(list(NULL, NULL, NULL, NULL, NULL)), "")
    expect_equal(ChartNumberFormat(list("Category", NULL, NULL, NULL, NULL)), "Category")
    expect_equal(ChartNumberFormat(list("Automatic", NULL, NULL, NULL, NULL), TRUE), ".0%")
    expect_equal(ChartNumberFormat(list("Scientific", NULL, NULL, NULL, 4), TRUE), ".4e")
    expect_equal(ChartNumberFormat(list("Metric units suffix", NULL, NULL, NULL, 3)), ".3s")
})

