test_that("Default horizontal separator works", {
  expect_true(is.null(getOption("formatters_default_hsep")))
  expect_error(set_default_hsep("foo"))
  expect_silent(set_default_hsep("a"))
  expect_equal(default_hsep(), "a")
  expect_silent(set_default_hsep(NULL))
  expect_true(default_hsep() %in% c("\u2014", "-"))
})

test_that("Default rounding type setting and retrieval work", {
  expect_true(is.null(getOption("formatters_default_rounding")))
  expect_error(set_default_rounding("foo"))
  expect_equal(default_rounding(), "iec")
  expect_silent(set_default_rounding("sas"))
  expect_equal(default_rounding(), "sas")
  expect_silent(set_default_rounding(NULL))
  expect_equal(default_rounding(), "iec")
})

test_that("Changing rounding type works", {
  # optional sas-style rounding
  tricky_val <- 0.845

  expect_identical(
    format_value(tricky_val, "xx.xx"),
    "0.84"
  )

  set_default_rounding("sas")
  expect_identical(
    format_value(tricky_val, "xx.xx"),
    "0.85"
  )

  format_fun_rtype <- function(x, output) default_rounding()
  expect_identical(format_value(tricky_val, format_fun_rtype), "sas")

  # passing down na_str
  format_fun_na_str <- function(x, na_str) na_str
  expect_identical(format_value(tricky_val, format_fun_na_str), "NA")
  expect_identical(format_value(tricky_val, format_fun_na_str, na_str = "-"), "-")
})
