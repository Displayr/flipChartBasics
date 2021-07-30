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
    expect_equal(GetVectorOfColors(NULL, 1:10, NULL, chart.type = "Column", 
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