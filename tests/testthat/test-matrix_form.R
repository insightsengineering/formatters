test_that("matrix_form works with and without indentation", {
  indented_rnms <- letters[seq(5)]
  df <- data.frame("C1" = seq(5), "C2" = 1)
  rownames(df) <- indented_rnms

  what_to_indent <- seq(2, 5, by = 2)
  indented_rnms[what_to_indent] <- paste0("  ", indented_rnms[what_to_indent])

  mf2 <- basic_matrix_form(df, indent_rownames = TRUE)
  mf2 <- basic_matrix_form(df, indent_rownames = FALSE)
  mf1$strings
  mf2$strings
  mf1$formats
  mf2$formats
  expect_equal(mf1, mf2)
  lyt <- basic_table() %>%
    split_rows_by("Species") %>%
    analyze("Sepal.Length")


  tbl <- build_table(lyt, iris)
  tbl
})
