context("Match Table")

ref.table <- matrix(1:24, 6, 4, dimnames = list(letters[1:6], LETTERS[1:4]))
v10 <- structure(1:10, .Names = letters[10:1])

test_that("Match table",
{
   res <- MatchTable(v10, ref.table)
   expect_equal(res, 10:5)
   
   expect_error(MatchTable(v10[7:10], ref.table), "color values is missing values for 'e', 'f'")
   expect_error(MatchTable(1:6, ref.table), NA)
})
    