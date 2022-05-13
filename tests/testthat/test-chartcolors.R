context("ChartColors")

res2 <- ChartColors(2)

test_that("Alpha color values", 
{
    c0 <- c(`#3E7DCC53` = "#3E7DCC53", `#04B5AC` = "#04B5AC", `#F5C524` = "#F5C524", 
        `#C44E41` = "#C44E41", `#8CC0FF` = "#8CC0FF", `#FF905A` = "#FF905A", 
        `#345E8C` = "#345E8C", `#04827B` = "#04827B", `#967F47` = "#967F47", 
        `#96362F` = "#96362F", `#2C4374` = "#2C4374", `#4D525A` = "#4D525A")
    c1 <- c(`#3E7DCC53` = "#3E7DCC", `#04B5AC` = "#04B5AC", `#F5C524` = "#F5C524", 
        `#C44E41` = "#C44E41", `#8CC0FF` = "#8CC0FF", `#FF905A` = "#FF905A", 
        `#345E8C` = "#345E8C", `#04827B` = "#04827B", `#967F47` = "#967F47", 
        `#96362F` = "#96362F", `#2C4374` = "#2C4374", `#4D525A` = "#4D525A")
    expect_equal(StripAlphaChannel(c0), c1)

    
    warn.txt <- "Alpha values were ignored"
    expect_equal(ChartColors(12), ChartColors(12, "Office colors"))
    expect_error(StripAlphaChannel(c("#FF0000", "#0000FF"), warn.txt), NA)
    expect_warning(res <- StripAlphaChannel(c("#FF000033", "#0000FF33"), warn.txt), 
                   warn.txt)
    expect_equal(res, c("#FF0000", "#0000FF"))
    
    expect_warning(ChartColors(5, "Custom gradient", custom.gradient.start = "#FF000033", 
        custom.gradient.end = "#0000FF33"), "Alpha values from selected colors ignored")
    expect_equal(ChartColors(2, "Custom palette", custom.palette="#FF000033, #0000FF33"),
        c("#FF000033", "#0000FF33"), check.attributes = FALSE)
    
    expect_warning(GetVectorOfColors(NULL, 1:10, chart.type = "Column", multi.color.series = T, 
        palette = "Custom gradient", palette.custom.gradient.start = "#FF000033", 
        palette.custom.gradient.end = "#0000FF33"), "Alpha values from selected colors ignored")
    expect_equal(GetVectorOfColors(NULL, cbind(A=1:10, B=10:1), NULL, chart.type = "Column", 
        palette = "Custom palette", palette.custom.palette = "#FF000033,#00FF00"),
        c("#FF000033", "#00FF00"), check.attributes = FALSE)
    expect_equal(GetVectorOfColors(NULL, 1:10, NULL, chart.type = "Column", 
        palette = "Custom color", palette.custom.color = "#FF000033"),
        "#FF000033", check.attributes = FALSE)
})

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
                 c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"), 
                 check.attributes = FALSE)
    expect_equal(ChartColors(7, "primary.colors"),
                 c("#000000", "#808000", "#FFFF00", "#808080", "#FFFF80", "#0080FF", "#80FFFF"),
                 check.attributes = FALSE)
    expect_equal(ChartColors(7, "rainbow_hcl"),
                 c("#E495A5", "#CEA472", "#9CB469", "#56BD96", "#46BAC8", "#99A9E2", "#D497D3"),
                 check.attributes = FALSE)
    expect_equal(ChartColors(5), c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#4473C5"), 
                 check.attributes = FALSE)
    expect_equal(ChartColors(5, "Default colors"), c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#4473C5"),
                 check.attributes = FALSE)
    expect_equal(ChartColors(5, "Custom gradient", custom.gradient.start = "#5C9AD3", 
                             custom.gradient.end = "#ED7D31"), 
                 c("#5C9AD3", "#8092AA", "#A48B82", "#C88459", "#ED7D31"), check.attributes = FALSE)
    expect_equal(suppressWarnings(unname(ChartColors(5, "Custom color", custom.color = "#5C9AD3"))), rep("#5C9AD3", 5))
    expect_equal(suppressWarnings(unname(ChartColors(9, "Custom palette", custom.palette = "#5C9AD3, #ED7D31  , #A5A5A5,#FFC000"))),
                 c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#5C9AD3"))
    expect_equal(unname(ChartColors(5, "Custom palette", custom.palette = "red,orange,green,blue,purple")),
                 c("#FF0000", "#FFA500", "#00FF00", "#0000FF", "#A020F0"))
    expect_warning(ChartColors(5, "Custom color", custom.color = "abc"), "Invalid color")
    expect_warning(ChartColors(3, "Custom palette", custom.palette="red,blue,abc"), "Invalid color")
    expect_equal(ChartColors(NA, "Custom palette", custom.palette="red,white,blue"), structure(c("#FF0000", "#FFFFFF", "#0000FF"), .Names = c("red", "white", "blue")))
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
    expect_equal(GetNumColors(z2, "Geographic Map")$num.series, NA)
    expect_equal(GetNumColors(z2, "Bar Pictograph")$num.series, 50)
    expect_equal(GetNumColors(z3, "Column")$num.series, 4)
    expect_equal(GetNumColors(z3, "Pie")$num.categories, 3)
    expect_equal(GetNumColors(z4, "Column")$num.series, 1)
    expect_equal(GetNumColors(z4, "Pie")$num.series, 10)
    
    venn.input <- list(list(sets = list(0), label = "Like", size = 100), list(sets = list(
        1), label = "Love", size = 50), list(sets = list(2), label = "Dislike", 
        size = 100), list(sets = list(3), label = "Hate", size = 50), 
        list(sets = list(0, 1), size = 50), list(sets = list(0, 2), 
        size = 0), list(sets = list(2, 3), size = 50))
    expect_equal(GetNumColors(venn.input, "Venn")$num.series, 4)
    expect_equal(GetNumColors(z3, "Venn")$num.series, 4)
    expect_equal(GetBrandsFromData(venn.input, chart.type = "Venn"), 
        c("Like", "Love", "Dislike", "Hate"))
})

test_that("Template with custom colors",
{
    template <- structure(list(colors = c("#5C9AD3", "#ED7D31"),
        global.font = list(family = "Arial", color = "#2C2C2C", size = 7.5, 
        units = "pt"), fonts = list(`Data labels` = list(family = "Arial", 
        color = "#2C2C2C", size = 7.5), Legend = list(family = "Arial", 
        color = "#2C2C2C", size = 7.5), Title = list(family = "Arial", 
        color = "#2C2C2C", size = 12), Subtitle = list(family = "Arial", 
        color = "#2C2C2C", size = 9), Footer = list(family = "Arial", 
        color = "#2C2C2C", size = 6), `Panel title` = list(family = "Arial", 
        color = "#2C2C2C", size = 9), `Categories axis tick labels` = list(
        family = "Arial", color = "#2C2C2C", size = 7.5), `Categories axis title` = list(
        family = "Arial", color = "#2C2C2C", size = 9), `Categories axis tick labels` = list(
        family = "Arial", color = "#2C2C2C", size = 7.5), `Values axis title` = list(
        family = "Arial", color = "#2C2C2C", size = 9), `Values axis tick labels` = list(
        family = "Arial", color = "#2C2C2C", size = 7.5), `Hover text` = list(
        family = "Arial", color = "#2C2C2C", size = 8.625)), global.number.font = list(
        units = "pt"), number.fonts = list(`Data label` = list(family = "Arial", 
        color = "#808080", size = 24, weight = "Normal"), `Gauge labels` = list(
        family = "Arial", color = "#B3B3B3", size = 9), `Text above` = list(
        family = "Arial", color = "#808080", size = 10, weight = "Normal"), 
        `Text below` = list(family = "Arial", color = "#808080", 
            size = 10, weight = "Normal"), `Hover text` = list(family = "Arial", 
            color = "#FFFFFF", size = 11, bg.color = "#808080"))), class = "AppearanceTemplate")
    
    pdat <- structure(c(Alabama = 1.46, Alaska = 0.13, Arizona = 2.47, Arkansas = 0.99, 
        California = 11.8, Colorado = 1.69, Connecticut = 1.14, Delaware = 0.31, 
        `Washington D.C.` = 0.37, Florida = 7.29, Georgia = 3.44, Hawaii = 0.34, 
        Idaho = 0.36, Illinois = 4.24, Indiana = 1.99, Iowa = 0.89, Kansas = 1.07, 
        Kentucky = 1.44, Louisiana = 1.23, Maine = 0.42, Maryland = 1.62, 
        Massachusetts = 1.59, Michigan = 3.23, Minnesota = 1.74, Mississippi = 0.9, 
        Missouri = 2.11, Montana = 0.23, Nebraska = 0.63, Nevada = 1.17, 
        `New Hampshire` = 0.4, `New Jersey` = 2.76, `New Mexico` = 0.49, 
        `New York` = 7.02, `North Carolina` = 2.49, `North Dakota` = 0.24, 
        Ohio = 3.81, Oklahoma = 1.03, Oregon = 1.52, Pennsylvania = 4.17, 
        `Rhode Island` = 0.26, `South Carolina` = 1.17, `South Dakota` = 0.22, 
        Tennessee = 1.49, Texas = 8.97, Utah = 0.74, Vermont = 0.25, 
        Virginia = 2.33, Washington = 1.89, `West Virginia` = 0.48, Wisconsin = 1.88, 
        Wyoming = 0.08), statistic = "%")
    
chart.types <- c("Table", "Area", "Bar", "Bar Pictograph", "Bean", "Box", 
        "Column", "Density", "Donut", "Funnel", "Geographic Map", "Heat", 
        "Histogram", "Line", "Palm", "Pie", "Radar", "Stream", "Scatter", 
        "Time Series", "Venn", "Violin")

    for (ch in chart.types)
        expect_error(GetVectorOfColors(template, pdat, filter=TRUE, ch,
            palette = "Default or template settings"), NA)
})