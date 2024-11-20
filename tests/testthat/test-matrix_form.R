test_that("matrix_form works with and without indentation", {
  require("dplyr", quietly = TRUE)

  skip_if_not_installed("dplyr")

  iris_output <- iris %>%
    group_by(Species) %>%
    summarize("all obs" = round(mean(Petal.Length), 2)) %>%
    mutate("Petal.Length" = "Mean")

  mf <- basic_matrix_form(iris_output,
    indent_rownames = TRUE,
    split_labels = "Species", data_labels = "Petal.Length"
  )
  mf_no_indent <- basic_matrix_form(iris_output,
    indent_rownames = FALSE,
    split_labels = "Species", data_labels = "Petal.Length"
  )

  expect_equal(mf$formats[, 2], c("", "-", "xx", "-", "xx", "-", "xx"))
  expect_equal(mf_no_indent$formats[, 2], mf$formats[, 2])
  expect_equal(mf_no_indent$formats[, 1], rep("", nrow(mf_no_indent$formats)))
  expect_equal(strwrap(mf$strings[, 1]), mf_no_indent$strings[, 1])
  expect_equal(grepl("\\s{2,}", mf$strings[-1, 1]), mf_rinfo(mf)$node_class == "DataRow")
  expect_equal(grepl("\\s{2,}", mf$formats[-1, 1]), mf_rinfo(mf)$node_class == "DataRow")
  expect_equal(
    mf$row_info[!colnames(mf$row_info) == "indent"],
    mf_no_indent$row_info[!colnames(mf_no_indent$row_info) == "indent"]
  )
  expect_equal(mf_no_indent$row_info$indent, rep(0, nrow(mf$row_info)))
  expect_equal(mf$row_info$indent, c(0, 1, 0, 1, 0, 1))
})

test_that("matrix_form is prepared correctly for printing when there is topleft information and new lines", {
  # Regression test for #942 in {rtables}
  ex_adsl_tmp <- ex_adsl[ex_adsl$ARM %in% c("A: Drug X", "B: Placebo"), ]

  # Convert ARM to a factor with the desired levels
  ex_adsl_tmp$ARM <- factor(ex_adsl_tmp$ARM, levels = c("A: \n\nDrug X", "B: \nPlacebo"))

  test_tbl <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("STRATA1", split_fun = keep_split_levels("A")) %>%
    summarize_row_groups() %>%
    build_table(ex_adsl_tmp)
  top_left(test_tbl) <- "aaaaaaaaaa"

  expect_silent(printed_out <- strsplit(toString(matrix_form(test_tbl, TRUE)), "\n")[[1]])
  expect_equal(
    printed_out,
    c(
      "               A:             ",
      "                         B:   ",
      "aaaaaaaaaa   Drug X    Placebo",
      "——————————————————————————————",
      "A            0 (NA%)   0 (NA%)"
    )
  )
})
