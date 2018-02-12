context("ChartColors")

res2 <- ChartColors(2)
test_that("ChartColors handles arguments", {
    res2 <- ChartColors(2)
    res2b <- ChartColors(2, "Default colors")
    expect_equal(length(res2), 2)
    expect_equal(res2, res2b)
    expect_equal(nchar(res2[1]), 7)
    
    red10 <- ChartColors(10, "Reds", trim=FALSE)
    red10trim <- ChartColors(10, "Reds", trim=TRUE)
    expect_equal(red10[1] != red10trim[1], TRUE)
    
    expect_equal(ChartColors(7, "Blues", trim=FALSE),
                 c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"))
    expect_equal(ChartColors(7, "primary.colors"),
                 c("#000000", "#808000", "#FFFF00", "#808080", "#FFFF80", "#0080FF", "#80FFFF"))
    expect_equal(ChartColors(7, "rainbow_hcl"),
                 c("#E495A5", "#CEA472", "#9CB469", "#56BD96", "#46BAC8", "#99A9E2", "#D497D3"))
    expect_equal(ChartColors(5), c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#4473C5"))
    expect_equal(ChartColors(5, "Default colors"), c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#4473C5"))
    expect_equal(ChartColors(5, "Custom gradient", custom.gradient.start = "#5C9AD3", custom.gradient.end = "#ED7D31"),
                 c("#5C9AD3", "#8092AA", "#A48B82", "#C88459", "#ED7D31"))
    expect_equal(suppressWarnings(unname(ChartColors(5, "Custom color", custom.color = "#5C9AD3"))), rep("#5C9AD3", 5))
    expect_equal(suppressWarnings(unname(ChartColors(9, "Custom palette", custom.palette = "#5C9AD3, #ED7D31  , #A5A5A5,#FFC000"))),
                 c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#5C9AD3"))
    expect_equal(unname(ChartColors(5, "Custom palette", custom.palette = "red,orange,green,blue,purple")),
                 c("#FF0000", "#FFA500", "#00FF00", "#0000FF", "#A020F0"))
    expect_warning(ChartColors(5, "Custom color", custom.color = "abc"), "Invalid color")
    expect_warning(ChartColors(3, "Custom palette", custom.palette="red,blue,abc"), "Invalid color")
})

test_that("GetNumColors gives correct output", {
    z1 <- 1:50
    z2 <- matrix(1:50, 50, dimnames = list(1:50, "Column"))
    z3 <- matrix(1:12, 3, 4)
    z4 <- table(1:10)
    
    expect_equal(GetNumColors(z1, "Column")$num.series, 1)
    expect_equal(GetNumColors(z1, "Pie")$num.series, 50)
    expect_equal(GetNumColors(z2, "Pie")$num.series, 1)
    expect_equal(GetNumColors(z2, "Column")$num.series, 1)
    expect_equal(GetNumColors(z2, "Bar Pictograph")$num.series, 50)
    expect_equal(GetNumColors(z3, "Column")$num.series, 4)
    expect_equal(GetNumColors(z3, "Pie")$num.categories, 3)
    expect_equal(GetNumColors(z4, "Column")$num.series, 1)
    expect_equal(GetNumColors(z4, "Pie")$num.series, 10)
    
})