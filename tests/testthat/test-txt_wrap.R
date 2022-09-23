context("titles and footers indentation")

test_that("indentation happens correctly if called for", {
  tit <- paste0(rep(paste0(rep("t", 5), collapse = ""), 3), collapse = " ")
  subtit <- paste0(rep(paste0(rep("s", 5), collapse = ""), 3), collapse = " ")
  mn_ft <- paste0(rep(paste0(rep("f", 5), collapse = ""), 3), collapse = " ")
  lyt <- basic_table(
    title = tit,
    subtitles = subtit,
    main_footer = mn_ft
  ) %>%
    analyze("Sepal.Width", afun = mean, format = "xx.")

  tbl <- build_table(lyt, iris)

  matform <- matrix_form(tbl)

  # As it was before
  expect_identical(matform$subtitles, subtit)
  expect_identical(matform$main_title, tit)
  expect_identical(matform$main_footer, mn_ft)

  printform <- toString(matform, wrap = TRUE, max_width = "auto")
  expected <- paste0(
    "ttttt ttttt\nttttt\nsssss sssss\nsssss\n\n--------------\n ",
    "      all obs\n--------------\nmean      3   \n--------------\n",
    "\nfffff fffff\nfffff\n"
  )
  expect_identical(printform, expected)

  printform <- toString(matform, wrap = TRUE, max_width = 10)
  expected <- paste0(
    "ttttt\nttttt\nttttt\nsssss\nsssss\nsssss\n\n--------------\n ",
    "      all obs\n--------------\nmean      3   \n--------------\n",
    "\nfffff\nfffff\nfffff\n"
  )
  expect_identical(printform, expected)
})

test_that("indentation fails when too long words are provided", {
  lyt <- basic_table(
    title = paste0(rep("ohno", 10), collapse = ""),
  ) %>%
    analyze("Sepal.Width", afun = mean, format = "xx.")

  tbl <- build_table(lyt, iris)
  matform <- matrix_form(tbl)

  expect_error(toString(matform, indent_titles = TRUE, indent_footers = TRUE))
})
