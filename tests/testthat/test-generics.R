testthat::test_that("nlines produces correct output with `max_width` defined", {
  testthat::expect_equal(nlines("1234567890", max_width = 5), 2)
})
