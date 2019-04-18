context("Use template")
default.template <-     template <- list(global.font = list(family = "Arial", color = "#2C2C2C", 
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

test_that("Get brand names",
{
    expect_equal(GetBrandsFromData(tb.untidy, TRUE, "Area") , NULL)
    expect_equal(GetBrandsFromData(tb.untidy, TRUE, "Area") , NULL)
    expect_equal(GetBrandsFromData(tb.untidy, TRUE, "Pyramid"), names(tb.tidy))
})
    