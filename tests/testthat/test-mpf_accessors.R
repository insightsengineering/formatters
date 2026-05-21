test_that("mf_col_widths getter and setter are available and work", {
  mf <- basic_matrix_form(mtcars)

  # getter returns numeric vector
  cw <- mf_col_widths(mf)
  expect_true(is.numeric(cw))

  # setter updates widths
  new <- rep(10L, NCOL(mf_strings(mf)))
  mf_col_widths(mf) <- new
  expect_equal(mf_col_widths(mf), new)
})
