testthat::test_that("nlines string is broken down in 2 lines with `max_width` defined", {
  testthat::expect_equal(nlines("1234567890", max_width = 5), 2)
})

testthat::test_that("nlines list argument is broken down in 3 lines with `max_width` defined", {
  testthat::expect_equal(nlines(list("1234567890", "12345"), max_width = 5), 3)
})

testthat::test_that("nlines small string produces 1 line as output", {
  testthat::expect_equal(nlines("1234567890"), 1)
})
