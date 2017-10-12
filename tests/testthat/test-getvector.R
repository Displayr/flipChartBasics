context("GetVector")

test_that("GetVector", {
    
    # assume all turn into NULL
    res1 <- GetVector(c("a","b", "d"), 5, 0)
    len1 <- unlist(lapply(res1, length))
    expect_equal(sum(len1), 15)
    
    res2 <- GetVector(c("a","b", "d"))
    len2 <- unlist(lapply(res2, length))
    expect_equal(sum(len2), 0)
    
    res2b <- Filter(Negate(is.null), res2)
    expect_equal(length(res2b), 0)
})