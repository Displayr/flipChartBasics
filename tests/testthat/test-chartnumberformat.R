context("ChartNumberFormat")

types <- c("Number", "Percentage", "Date/Time", "Currency", "Metric units suffix", "Scientific", "Custom")
expected <- c(".3f", ".3%", ".3", ".3f", ".3s", ".3e", ".3")
    
test_that("ChartNumberFormat", {
    
    for (i in seq(length(types))) {
        expect_equal(ChartNumberFormat(list(types[i], NULL, NULL, FALSE, 3)), expected[i])
    }

    expect_equal(ChartNumberFormat(list("Custom", NULL, "%B %d, %Y", FALSE, 0)), "%B %d, %Y")
    expect_equal(ChartNumberFormat(list("Date/Time", "DD Mon YYYY", NULL, FALSE, 0)), "%d %b %Y")
    expect_equal(ChartNumberFormat(list("Number", NULL, NULL, TRUE, 0)), ",.0f")
    expect_equal(ChartNumberFormat(list("Number", NULL, NULL, FALSE, 5)), ".5f")
    expect_error(ChartNumberFormat(list("Unknown", NULL, NULL, FALSE, 5)), "Number format not recognized.")
})

