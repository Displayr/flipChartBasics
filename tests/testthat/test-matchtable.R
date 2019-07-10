context("Match Table")

ref.table <- matrix(1:24, 6, 4, dimnames = list(letters[1:6], LETTERS[1:4]))
v10 <- structure(1:10, .Names = letters[10:1])

cols <- c(Feminine = "#FF0000", `Health-conscious` = "#E87D85", Innocent = "#E87D85", 
    Older = "#BED6E3", `Open to new experiences` = "#F2A09A", Rebellious = "#F3A9A1", 
    Sleepy = "#E87D85", Traditional = "#87BAE7", `Weight-conscious` = "#0000FF")
xmat <- structure(c(6.125, 2, 10.5, 64.625, 22.375, 25.5, 9.5, 91.25, 
    0.5, 57.125, 57.75, 21.625, 22.5, 8.875, 4.75, 23.25, 14.625, 
    76.125, 22.375, 53.5, 11.375, 5.375, 50.625, 64, 9.75, 3, 63.875, 
    8.875, 2.5, 10, 39, 16.75, 17.75, 13.5, 54.75, 0, 61.5, 57.875, 
    44.625, 9.875, 16.625, 3.75, 29.75, 3.75, 76.625, 9.375, 30.625, 
    6.875, 6.75, 49.25, 44.75, 5.5, 4.375, 40.375, 9.25, 17.375, 
    29.875, 7.25, 12.875, 15.25, 38.875, 2.5, 5.75), .Dim = c(9L, 
    7L), .Dimnames = list(c("Feminine", "Health-conscious", "Innocent", 
    "Older", "Open to new experiences", "Rebellious", "Sleepy", "Traditional", 
    "Weight-conscious"), c("Coke", "Diet Coke", "Coke Zero", "Pepsi", 
    "Diet Pepsi", "Pepsi Max", "None of these")), statistic = "%", name = "q5 - duplicate", questions = c("q5 - duplicate", 
    "SUMMARY"))
xnames <- rev(rownames(xmat))

custom.palette <- structure(c("#FF0000", "#2E2E2E", "#464646", "#5D5D5D", "#747474", 
    "#8B8B8B", "#A2A2A2", "#B9B9B9", "#D1D1D1", "#E8E8E8"), .Names = c("Coca-Cola", 
    "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi", "Pepsi Max", 
    "Dislike all cola", "Don't care", "NET", NA))
brand.names <- c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", 
    "Pepsi Max", "Dislike all cola", "Don't care")

test_that("Match table",
{
   res <- MatchTable(v10, ref.table)
   expect_equal(res, 10:5)
   
   expect_error(MatchTable(v10[7:10], ref.table, x.table.name = "Color values"), "Color values: Missing values for 'e', 'f'")
   expect_error(MatchTable(1:6, ref.table), NA)
   expect_equal(MatchTable(cols, xmat), cols, check.attributes = FALSE)
   
   expect_error(res <- MatchTable(cols, ref.names = xnames), NA)
   expect_equal(res[1], "#0000FF")
   
   expect_error(MatchTable(custom.palette, ref.names = brand.names), NA)
   expect_warning(MatchTable(unname(custom.palette), ref.names = letters[1:10]), NA)
   expect_warning(res <- MatchTable(custom.palette, ref.names = c()), "Names were ignored")
   expect_equal(length(res), 1)
})

    