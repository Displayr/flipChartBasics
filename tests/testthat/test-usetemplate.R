context("Use template")
default.template <- template <- list(global.font = list(family = "Arial", color = "#2C2C2C", 
        size = 7.5, units = "pt"), fonts = list(`Data labels` = list(
        family = "Arial", color = "#2C2C2C", size = 7.5), Legend = list(
        family = "Arial", color = "#2C2C2C", size = 7.5), Title = list(
        family = "Arial", color = "#2C2C2C", size = 12), Subtitle = list(
        family = "Arial", color = "#2C2C2C", size = 9), Footer = list(
        family = "Arial", color = "#2C2C2C", size = 6), `Categories axis title` = list(
        family = "Arial", color = "#2C2C2C", size = 9), `Categories axis tick labels` = list(
        family = "Arial", color = "#2C2C2C", size = 7.5), `Values axis title` = list(
        family = "Arial", color = "#2C2C2C", size = 9), `Values axis tick labels` = list(
        family = "Arial", color = "#2C2C2C", size = 7.5), `Panel title` = list(
        family = "Arial", color = "#2C2C2C", size = 9), `Hover text` = list(
        family = "Arial", color = "#2C2C2C", size = 8.625)), colors = "Default colors", 
        brand.colors = NULL)

tb.tidy <- structure(c(`My friends would describe me as cultured, and refined` = 2.98165137614679, 
`I think it is important to be honest when giving complements` = 4.11009174311927, 
`I can be a little naive at times` = 3.07339449541284, `I am the life of the party` = 2.63302752293578, 
`I am relaxed most of the time and not easily worried` = 3.34862385321101, 
`Living in a big city is important to me` = 2.45565749235474, 
`I think it is important to follow and maintain traditions` = 3.40366972477064, 
`I enjoy being attractive to the opposite sex` = 3.52905198776758, 
`I am young at heart` = 4.02752293577982, `I follow all the latest fashions` = 2.28440366972477
), statistic = "Average", name = "Q25. Respondent image (number multi)", questions = c("Q25. Respondent image (number multi)", 
"SUMMARY"), assigned.rownames = TRUE)

tb.untidy <- structure(c(`My friends would describe me as cultured, and refined` = 2.98165137614679, 
`I think it is important to be honest when giving complements` = 4.11009174311927, 
`I can be a little naive at times` = 3.07339449541284, `I am the life of the party` = 2.63302752293578, 
`I am relaxed most of the time and not easily worried` = 3.34862385321101, 
`Living in a big city is important to me` = 2.45565749235474, 
`I think it is important to follow and maintain traditions` = 3.40366972477064, 
`I enjoy being attractive to the opposite sex` = 3.52905198776758, 
`I am young at heart` = 4.02752293577982, `I follow all the latest fashions` = 2.28440366972477
), .Dim = 10L, .Dimnames = list(c("My friends would describe me as cultured, and refined", 
"I think it is important to be honest when giving complements", 
"I can be a little naive at times", "I am the life of the party", 
"I am relaxed most of the time and not easily worried", "Living in a big city is important to me", 
"I think it is important to follow and maintain traditions", 
"I enjoy being attractive to the opposite sex", "I am young at heart", 
"I follow all the latest fashions")), statistic = "Average", name = "Q25. Respondent image (number multi)", questions = c("Q25. Respondent image (number multi)", 
"SUMMARY"), assigned.rownames = TRUE)

tb.spaces <- structure(c(49.5601173020528, 50.4398826979472, 100, 38.2022471910112, 
61.7977528089888, 100, 41.2587412587413, 58.7412587412587, 100, 
62.5, 37.5, 100, 35, 65, 100, 59.6638655462185, 40.3361344537815, 
100, 50, 50, 100, 70, 30, 100, 49.375, 50.625, 100), statistic = "Column %", .Dim = c(3L, 
9L), .Dimnames = list(c("Male", "Female", "NET"), c("Coca-Cola", 
"Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi", "Pepsi Max", 
"Dislike all cola", "Don't care", "NET")), name = "table.Gender.by.Preferred.cola", questions = c("Gender", 
"Preferred cola"))

tb.no.colnames <- structure(c(64.4035265949313, 60.1413096290206, 58.5872463087025, 
57.7853477448339, 47.8652428890802, 43.6618216267763, NA), .Dim = c(7L, 
1L), .Dimnames = list(c("Lexus", "Land Rover", "BMW", "Volvo", 
"Mercedes Benz", "Audi", "Jaguar"), NULL))

ff <- rep(1, 10)
attr(ff, "label") <- "Pepsi "

test_that("Get brand names",
{
    expect_equal(GetBrandsFromData(tb.untidy, TRUE, "Area") , NULL)
    expect_equal(GetBrandsFromData(tb.untidy, TRUE, "Area") , NULL)
    expect_equal(GetBrandsFromData(tb.untidy, TRUE, "Pyramid"), names(tb.tidy))
    
    expect_equal(GetBrandsFromData(tb.spaces, chart.type = "Column"), c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", 
"Pepsi Max", "Dislike all cola", "Don't care", "NET"))
    expect_equal(GetBrandsFromData(1:10, ff, "Column"), "Pepsi")
})

test_that("Scatter plot brand names",
{
    scatter.dat <- structure(list(X = c(1.2, 2.3, 0.6, 2.1), Y = c(3.7, 3.5, 5, 
        3), Size = c(1, 1, 1, 1), Color = c("Coke", "Pepsi", "Fanta", 
        "Sprite")), row.names = c("Coke", "Pepsi", "Fanta", "Sprite"), 
        assigned.rownames = TRUE, scatter.variable.indices = c(x = 1, 
        y = 2, sizes = 3, colors = 4, groups = 4), class = "data.frame")
    expect_equal(GetBrandsFromData(scatter.dat, TRUE, "Scatter"),
       c("Coke", "Pepsi", "Fanta", "Sprite"))
    
    # Use GetVectorOfColors instead of PrepareColors
    res <- GetVectorOfColors(template, scatter.dat, NULL, "Scatter", 4,
            palette = "Reds")
    expect_equal(res, structure(c("#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15"), 
                palette.type = "Reds"))
    
    # For use with old versions of the template
    col.vec <- c(Coke = "red", Pepsi = "blue", Fanta = "orange", Sprite = "green")
    tmp.template <- template
    tmp.template$brand.colors = rev(col.vec)
    res <- GetVectorOfColors(tmp.template, scatter.dat, NULL, "Scatter", 4,
                palette = "Default or template settings")
    expect_equal(res, checkColors(col.vec))
    
    # Current version of template
    tmp.template <- template
    tmp.template$colors = rev(col.vec) 
    res <- GetVectorOfColors(tmp.template, scatter.dat, NULL, "Scatter", 4,
                palette = "Default or template settings")
    expect_equal(res, checkColors(col.vec))
    
    # Named template with no names
    tmp.template <- template
    tmp.template$colors = c(col.vec, "Other" = "grey") 
    expect_warning(res <- GetVectorOfColors(tmp.template, tb.no.colnames, NULL, "Column",
                palette = "Default or template settings"), 
                "The template contains named colors but the data series is unnamed. Try selecting 'Data Series' > 'Multiple colors within a single series'")
    expect_equal(res, c(Other = "grey"))
    expect_warning(res <- GetVectorOfColors(tmp.template, 1:10, NULL, "Column",
                palette = "Default or template settings"),
                "The template contains named colors but the data series is unnamed. Try selecting 'Data Series' > 'Multiple colors within a single series'")
    expect_equal(res, c(Other = "grey"))
      
    # Using Custom palette instead of template 
    res <- GetVectorOfColors(NULL, scatter.dat, NULL, "Scatter", 4, 
        palette = "Custom palette", palette.custom.palette = rev(col.vec)[1:3])
    expect_equal(res, c(Coke = "#CCCCCC", Pepsi = "#0000FF", Fanta = "#FFA500", 
                Sprite = "#00FF00"))
    res <- GetVectorOfColors(NULL, 1:10, NULL, "Scatter", 4, 
        palette = "Custom palette", palette.custom.palette = rev(col.vec)[1:3])
})

test_that("Named colors for Pie inner and outer ring",
{
    col.vec <- c(`Coca-Cola` = "#FF0000", Pepsi = "#0000FF", `Coke Zero` = "#000000", 
        `Diet Pepsi` = "#008822", Female = "#FFC0CB", Male = "#00BFFF", Unknown = "#E6E6E6")
    tmp.template <- template
    tmp.template$colors <- col.vec
    res.inner <- GetVectorOfColors(tmp.template, tb.spaces, NULL, "Pie",
        palette = "Default or template settings")
    expect_equal(res.inner, c(`Coca-Cola` = "#FF0000", `Diet Coke` = "#E6E6E6", 
        `Coke Zero` = "#000000", Pepsi = "#0000FF", `Diet Pepsi` = "#008822", 
        `Pepsi Max` = "#E6E6E6", `Dislike all cola` = "#E6E6E6", 
        `Don't care` = "#E6E6E6", NET = "#E6E6E6"))
    res.outer <- GetVectorOfColors(tmp.template, tb.spaces, NULL, "Pie",
        palette = "Default or template settings", type = "Pie subslice")
    expect_equal(res.outer, c(Male = "#00BFFF", Female = "#FFC0CB", NET = "#E6E6E6"))
    
    res <- GetVectorOfColors(tmp.template, tb.untidy, NULL, "Pie", 
        palette = "Strong colors", type = "Pie subslice")
    expect_equal(res, NULL)
    
    tb.15 <- structure(c(0.2, 0.3, 0.5), .Dim = c(3L, 1L), .Dimnames = list(
    c("Cat", "Dog", "Pigeon"), "Score"), statistic = "%", assigned.rownames = TRUE)
    res <- GetVectorOfColors(template, tb.15, NULL, "Pie", palette = "Reds", type = "Pie subslice")
    expect_equal(res, structure(c("#FCAE91", "#FB6A4A", "#CB181D"), palette.type = "Reds"))
})

test_that("Named custom palette",
{    
    col.vec <- c(`Coca-Cola` = "#FF0000", Pepsi = "#0000FF", `Coke Zero` = "black", 
        `Diet Pepsi` = "#008822", Female = "#FFC0CB", Male = "#00BFFF", Unknown = "#E6E6E6")
    res <- GetVectorOfColors(template, t(tb.spaces), NULL, "Column", multi.color.series = TRUE,
        palette = "Custom palette (R output)", palette.custom.palette = col.vec)
    expect_equal(res,  c(`Coca-Cola` = "#FF0000", `Diet Coke` = "#CCCCCC", 
        `Coke Zero` = "#000000", Pepsi = "#0000FF", `Diet Pepsi` = "#008822", 
        `Pepsi Max` = "#CCCCCC", `Dislike all cola` = "#CCCCCC", 
        `Don't care` = "#CCCCCC", NET = "#CCCCCC"))
})

test_that("Color values",
{
    res <- GetVectorOfColors(template, tb.spaces, NULL, "Column", 
        palette = "Reds, light to dark", multi.color.series = TRUE, 
        color.values = tb.spaces, small.multiples = TRUE)
    expect_equal(res, structure(c("#FB8769", "#FB8667", "#CB181D", "#FB9E80", 
        "#FB7050", "#CB181D", "#FB987A", "#FB7656", "#CB181D", "#FB6E4F", 
        "#FB9F81", "#CB181D", "#FBA486", "#FB6A4A", "#CB181D", "#FB7454", 
        "#FB997C", "#CB181D", "#FB8768", "#FB8768", "#CB181D", "#F45E43", 
        "#FCAE91", "#CB181D", "#FB8869", "#FB8567", "#CB181D"), .Dim = c(3L, 9L)))
})

test_that("Pre-defined palettes",
{
    tmp.template <- template
    tmp.template$brand.colors <- NULL
    tmp.template$colors <- "Strong colors"
    res <- GetVectorOfColors(tmp.template, tb.tidy, NULL, "Column",
            palette = "Default or template settings")
    expect_equal(res, structure("#E41A1C", palette.type = "Strong colors"))
    
    res <- GetVectorOfColors(tmp.template, tb.spaces, NULL, "Column", 
            palette = "Default or template settings")
    expect_equal(res, structure(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
            "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"), 
            palette.type = "Strong colors"))
})

venn.data <- structure(c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 
0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1), .Dim = c(8L, 4L
), .Dimnames = list(NULL, c("Coca-Cola", "Pepsi", "Coke Zero", 
"Diet Pepsi")))

test_that("Venn data",
{
    expect_equal(GetBrandsFromData(venn.data, chart.type = "Venn"),
       c("Coca-Cola", "Pepsi", "Coke Zero", "Diet Pepsi"))
    expect_equal(GetVectorOfColors(template, venn.data, NULL, "Venn",
        palette = "Strong colors"), structure(c("#E41A1C", "#377EB8", 
        "#4DAF4A", "#984EA3"), palette.type = "Strong colors"))
})
                                                                                                                                                                                                                                                                                                                                                                