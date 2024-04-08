test_that("toString works with and without indentation", {

  library(rtables)
  tbl <- df_to_tt(mtcars[seq(5), seq(2)])
  mf1 <- matrix_form(tbl, indent_rownames = TRUE)
  mf2 <- matrix_form(tbl, indent_rownames = FALSE)
  mf2 <- basic_matrix_form(mtcars[seq(5), seq(2)])
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
