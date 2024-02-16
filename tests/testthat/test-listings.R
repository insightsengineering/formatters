test_that("basic_listing_mf respect core modifications from table matrix_form", {
  expect_silent(lmf <- basic_listing_mf())

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
  expect_silent(lmf <- basic_listing_mf(mtcars[c(1, 2), c("vs", "gear", "mpg")]))
  expect_snapshot(cat(toString(lmf)))
})

test_that("regression test for oneline listing pagination", {
  lmf <- basic_listing_mf(mtcars[c(1, 2), ]) # with only one it fails

  mf_strings(lmf) <- mf_strings(lmf)[-3, ]
  mf_spans(lmf) <- mf_spans(lmf)[-3, ]
  lmf$display <- lmf$display[-3, ]
  mf_lgrouping(lmf) <- mf_lgrouping(lmf)[-3]
  mf_rinfo(lmf) <- mf_rinfo(lmf)[1, , drop = FALSE]
  mf_aligns(lmf) <- mf_aligns(lmf)[-3, ]
  mf_formats(lmf) <- mf_formats(lmf)[-3, ]

  expect_error(export_as_txt(lmf))
})
