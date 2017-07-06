context("ChartColors")

res2 <- ChartColors(2)
test_that("ChartColors handles arguments", {
    res2 <- ChartColors(2)
    res2b <- ChartColors(2, "Default colors")
    expect_equal(length(res2), 2)
    expect_equal(res2, res2b)
    expect_equal(nchar(res2[1]), 7)
    
    red10 <- ChartColors(10, "Reds")
    red10trim <- ChartColors(10, "Reds", trim=TRUE)
    expect_equal(red10[1] != red10trim[1], TRUE)
    
    expect_equal(ChartColors(7, "Blues"),
                 c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"))
    expect_equal(ChartColors(7, "primary.colors"),
                 c("#000000", "#808000", "#FFFF00", "#808080", "#FFFF80", "#0080FF", "#80FFFF"))
    expect_equal(ChartColors(7, "rainbow_hcl"),
                 c("#E495A5", "#CEA472", "#9CB469", "#56BD96", "#46BAC8", "#99A9E2", "#D497D3"))
    expect_equal(ChartColors(5), c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#4473C5"))
    expect_equal(ChartColors(5, "Default colors"), c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#4473C5"))
    expect_equal(ChartColors(5, "Custom gradient", custom.gradient.start = "#5C9AD3", custom.gradient.end = "#ED7D31"),
                 c("#5C9AD3", "#8092AA", "#A48B82", "#C88459", "#ED7D31"))
    expect_equal(ChartColors(5, "Custom color", custom.color = "#5C9AD3"), rep("#5C9AD3", 5))
    expect_equal(suppressWarnings(ChartColors(9, "Custom palette", custom.palette = "#5C9AD3, #ED7D31  , #A5A5A5,#FFC000")),
                 c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#5C9AD3"))
    expect_equal(ChartColors(5, "Custom palette", custom.palette = "red,orange,green,blue,purple"),
                 c("red", "orange", "green", "blue", "purple"))
})