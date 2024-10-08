test_that("toString works with and without indentation", {
  require("dplyr", quietly = TRUE)

  skip_if_not_installed("dplyr")

  set.seed(1)
  iris_output <- iris %>%
    mutate(my_cols = sample(c("A", "B", "C"), nrow(iris), replace = TRUE)) %>%
    group_by(Species, my_cols) %>%
    summarize("mean_petal_length" = round(mean(Petal.Length), 1), .groups = "drop") %>%
    as.data.frame() %>%
    reshape(
      timevar = "my_cols",
      idvar = "Species",
      direction = "wide",
      v.names = "mean_petal_length"
    ) %>%
    # identical to tidyr::pivot_wider(names_from = my_cols, values_from = mean_petal_length) %>%
    rename(
      A = `mean_petal_length.A`,
      B = `mean_petal_length.B`,
      C = `mean_petal_length.C`
    ) %>%
    mutate("Petal.Length" = "Mean")

  mf <- basic_matrix_form(iris_output,
    indent_rownames = TRUE,
    split_labels = "Species", data_labels = "Petal.Length"
  )
  mf_no_indent <- basic_matrix_form(iris_output,
    indent_rownames = FALSE,
    split_labels = "Species", data_labels = "Petal.Length"
  )
  out_mf <- c(
    "              A     B     C ",
    "----------------------------",
    "setosa                      ",
    "  Mean       1.5   1.4   1.5",
    "versicolor                  ",
    "  Mean       4.2   4.4   4.2",
    "virginica                   ",
    "  Mean       5.3   5.8   5.5"
  )

  expect_identical(out_mf, strsplit(toString(mf, hsep = "-"), "\n")[[1]])
  expect_error(
    toString(mf, widths = c(2, 1, 1, 1)),
    "row label column is not wide enough"
  )

  out_mf_no_indent <- c(
    "              A     B     C ",
    "----------------------------",
    "setosa                      ",
    "Mean         1.5   1.4   1.5",
    "versicolor                  ",
    "Mean         4.2   4.4   4.2",
    "virginica                   ",
    "Mean         5.3   5.8   5.5"
  )
  expect_identical(out_mf_no_indent, strsplit(toString(mf_no_indent, hsep = "-"), "\n")[[1]])
  expect_silent(argh <- toString(mf_no_indent, widths = c(1, 1, 2, 2)))
})
