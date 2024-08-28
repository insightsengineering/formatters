test_that("basic_listing_mf respect core modifications from table matrix_form", {
  skip_if_not_installed("dplyr")

  mtcars_rn <- dplyr::mutate(mtcars, "rnms" = rownames(mtcars))
  expect_silent(lmf <- basic_listing_mf(mtcars_rn, keycols = c("vs", "gear")))

  # strings
  cols_test <- mf_strings(lmf)[, c(1, 2)]
  expect_identical(colnames(cols_test), c("vs", "gear"))
  expect_identical(cols_test[seq_len(4), 1], c("vs", "0", "", ""))
  expect_identical(cols_test[seq_len(4) + 10, 2], c("", "", "", "4"))
  expect_identical(mf_strings(lmf)[1, "rnms"], c(rnms = "rnms"))

  # formats
  expect_true(all(mf_formats(lmf) == "1"))

  # aligns
  expect_true(all(mf_aligns(lmf) == "center"))

  # rowinfo
  ri <- lmf$row_info
  expect_true(all(ri$node_class == "listing_df"))
  expect_true(all(ri$label == ""))
  expect_true(all(ri$name == ""))
  expect_true(all(ri$abs_rownumber == rownames(ri)))
  expect_true(all(is.na(ri$path)))

  # col_widths
  expect_identical(colnames(mf_strings(lmf)), names(lmf$col_widths))

  # snapshot
  tmp_mtcars <- mtcars_rn[c(1, 2), c("vs", "gear", "rnms", "mpg")]
  expect_silent(lmf <- basic_listing_mf(tmp_mtcars, keycols = c("vs", "gear")))
  print_out <- c(
    "main title",
    "sub",
    "titles",
    "",
    "-------------------------------",
    "vs   gear       rnms        mpg",
    "-------------------------------",
    "0     4       Mazda RX4     21 ",
    "            Mazda RX4 Wag   21 ",
    "-------------------------------",
    "",
    "main",
    "  footer",
    "",
    "prov footer"
  )
  expect_identical(
    strsplit(toString(lmf, hsep = "-"), "\n")[[1]],
    print_out
  )
})

test_that("regression test for oneline listing pagination", {
  lmf <- basic_listing_mf(mtcars[c(1, 2), ], keycols = c("vs")) # with only one it fails

  expect_silent(toString(lmf)) # fix one keycols

  mf_strings(lmf) <- mf_strings(lmf)[-3, ]
  mf_spans(lmf) <- mf_spans(lmf)[-3, ]
  lmf$display <- lmf$display[-3, ]
  mf_lgrouping(lmf) <- mf_lgrouping(lmf)[-3]
  mf_rinfo(lmf) <- mf_rinfo(lmf)[1, , drop = FALSE]
  mf_aligns(lmf) <- mf_aligns(lmf)[-3, ]
  mf_formats(lmf) <- mf_formats(lmf)[-3, ]

  expect_silent(export_as_txt(lmf))
})

test_that("listings are correctly paginated when a wrapping happens on non-dominant keycol", {
  # Regression test for #243 wrong number is calculated for pagination due to keycols is ""
  # when wrapping happens.

  # pre-proc
  iris2 <- iris[1:10, 3:5]
  iris2$Species <- "SOMETHING VERY LONG THAT BREAKS PAGINATION"
  rownames(iris2) <- NULL

  # test to get rid of the rownames
  tmp_bmf <- basic_matrix_form(iris2, ignore_rownames = TRUE)
  expect_true(all.equal(
    ncol(tmp_bmf$strings), ncol(tmp_bmf$formats), ncol(iris2), length(tmp_bmf$col_widths)
  ))
  tmp_bmf <- basic_matrix_form(iris2, ignore_rownames = FALSE)
  expect_true(all.equal(
    ncol(tmp_bmf$strings),
    ncol(tmp_bmf$formats),
    ncol(iris2) + 1,
    length(tmp_bmf$col_widths)
  ))

  lst <- basic_listing_mf(iris2,
    keycols = c("Species", "Petal.Width"), add_decoration = FALSE
  )

  expect_silent(
    pgs <- paginate_to_mpfs(lst, colwidths = c(30, 11, 12), lpp = 5)
  )

  expect_true(all(
    sapply(pgs, function(x) length(strsplit(toString(x), "\n")[[1]]) <= 5)
  ))
  expect_silent(sapply(pgs, function(x) toString(x)))

  # for the reprinting of the start lines (keycols) when paginated: debug(formatters:::.mf_subset_core_mats)

  # Errors
  expect_error(
    expect_message(
      pgs <- paginate_to_mpfs(lst, colwidths = c(30, 11, 12), lpp = 3, verbose = TRUE)
    )
  )
  expect_error(
    expect_message(
      pgs <- paginate_to_mpfs(lst, colwidths = c(30, 11, 12), lpp = 8, cpp = 5, verbose = TRUE)
    )
  )

  # Test 2 with double wrapping
  tmp_fct <- factor(iris2$Petal.Width)
  levels(tmp_fct) <- paste0("Very long level name ", levels(tmp_fct))
  iris2$Petal.Width <- as.character(tmp_fct)

  lst <- basic_listing_mf(iris2, keycols = c("Species", "Petal.Width"), add_decoration = FALSE)

  pgs <- paginate_to_mpfs(lst, colwidths = c(30, 15, 12), lpp = 8)

  expect_true(all(
    sapply(pgs, function(x) {
      length(strsplit(toString(x), "\n")[[1]]) <= 8
    })
  ))
  expect_silent(null <- sapply(pgs, function(x) toString(x)))
})
