context("table_inset and titles and footers word wrapping")

test_that("tf_wrap and table inset work (including together)", {
  tit <- paste0(rep(paste0(rep("t", 5), collapse = ""), 3), collapse = " ")
  subtit <- paste0(rep(paste0(rep("s", 5), collapse = ""), 3), collapse = " ")
  mn_ft <- paste0(rep(paste0(rep("f", 5), collapse = ""), 3), collapse = " ")
  prv_ft <- paste0(rep(paste0(rep("p", 5), collapse = ""), 3), collapse = " ")

  fakedf <- data.frame(all_obs = 3, row.names = "mean")
  matform <- basic_matrix_form(fakedf)
  main_title(matform) <- tit
  subtitles(matform) <- subtit
  main_footer(matform) <- mn_ft
  prov_footer(matform) <- prv_ft

  printform <- toString(matform, tf_wrap = TRUE, max_width = "auto", hsep = "-")
  printform <- strsplit(printform, "\n")[[1]]
  expected <- c(
    "ttttt ttttt",
    "ttttt",
    "sssss sssss",
    "sssss",
    "",
    "--------------",
    "       all_obs",
    "--------------",
    "mean      3   ",
    "--------------",
    "",
    "fffff fffff",
    "fffff",
    "",
    "ppppp ppppp",
    "ppppp"
  )
  expect_identical(printform, expected)

  printform2 <- toString(matform, tf_wrap = TRUE, max_width = 10, hsep = "-")
  printform2 <- strsplit(printform2, "\n")[[1]]
  expected2 <- c(
    "ttttt",
    "ttttt",
    "ttttt",
    "sssss",
    "sssss",
    "sssss",
    "",
    "--------------",
    "       all_obs",
    "--------------",
    "mean      3   ",
    "--------------",
    "",
    "fffff",
    "fffff",
    "fffff",
    "",
    "ppppp",
    "ppppp",
    "ppppp"
  )
  expect_identical(printform2, expected2)

  matform2 <- matform
  table_inset(matform2) <- 2
  matform2$ref_footnotes <- c("ref ftnote")
  ## make sure interaction with inset works
  printform3 <- toString(matform2, tf_wrap = TRUE, max_width = 11, hsep = "-")
  printform3 <- strsplit(printform3, "\n")[[1]]
  expected3 <- c(
    "ttttt ttttt",
    "ttttt",
    "sssss sssss",
    "sssss",
    "",
    "  --------------",
    "         all_obs",
    "  --------------",
    "  mean      3   ",
    "  --------------",
    "",
    "  ref",
    "  ftnote",
    "  --------------",
    "",
    "  fffff",
    "  fffff",
    "  fffff",
    "",
    "ppppp ppppp",
    "ppppp"
  )
  expect_identical(printform3, expected3)

  ## coverage for allfooter
  expect_identical(
    all_footers(matform),
    c(main_footer(matform), prov_footer(matform))
  )
})

test_that(paste(
  "toString() is silent when newline is in decorations (footnotes, titles)",
  "as matrix_form constructor expands all newlines"
), {
  bmf <- basic_matrix_form(iris)
  main_title(bmf) <- "some\nvery\nspacious\ntitle"
  prov_footer(bmf) <- "some\nvery\nspacious\nfooter"
  bmf$ref_footnotes <- "some\nvery\nspacious\nreference"
  expect_silent(toString(bmf, tf_wrap = FALSE))
  expect_silent(toString(bmf, tf_wrap = TRUE))
})

test_that("wrapping works with words that are too big (no warning)", {
  bmf <- basic_matrix_form(mtcars[1:2, 1:2])
  main_title(bmf) <- "TITLE"
  subtitles(bmf) <- "SUB TITLE IS"
  page_titles(bmf) <- "PAGE TITLE"
  main_footer(bmf) <- "FOOTER"
  prov_footer(bmf) <- c("", "PROV FOOTER")
  bmf$ref_footnotes <- "REFERENCE"
  result <- toString(bmf, tf_wrap = TRUE, max_width = 4, hsep = "-")
  res_vec <- strsplit(result, "\n")[[1]]
  exp_vec <- c(
    "TITL", "E",
    "SUB", "TITL", "E IS",
    "PAGE", "TITL", "E",
    "",
    "-------------------------",
    "                mpg   cyl",
    "-------------------------",
    "Mazda RX4       21     6 ",
    "Mazda RX4 Wag   21     6 ",
    "-------------------------",
    "",
    "REFE", "RENC", "E",
    "-------------------------",
    "",
    "FOOT", "ER",
    "",
    "",
    "PROV", "FOOT", "ER"
  )
  expect_identical(res_vec, exp_vec)
})

test_that("auto wrapping works with inset and col_gap", {
  bmf <- basic_matrix_form(mtcars[1:2, 1:2])
  table_inset(bmf) <- 1
  main_title(bmf) <- strrep("a", 30)
  subtitles(bmf) <- strrep("b", 30)
  page_titles(bmf) <- strrep("c", 30)
  main_footer(bmf) <- strrep("d", 30)
  prov_footer(bmf) <- strrep("e", 30)
  bmf$col_gap <- 3
  bmf$ref_footnotes <- strrep("f", 30)
  result <- toString(bmf, tf_wrap = TRUE, max_width = "auto", hsep = "=")
  res_vec <- strsplit(result, "\n")[[1]]
  exp_vec <- c(
    "aaaaaaaaaaaaaaaaaaaaaaaaaa",
    "aaaa",
    "bbbbbbbbbbbbbbbbbbbbbbbbbb",
    "bbbb",
    "cccccccccccccccccccccccccc",
    "cccc",
    "",
    " =========================",
    "                 mpg   cyl",
    " =========================",
    " Mazda RX4       21     6 ",
    " Mazda RX4 Wag   21     6 ",
    " =========================",
    "",
    " fffffffffffffffffffffffff",
    " fffff",
    " =========================",
    "",
    " ddddddddddddddddddddddddd",
    " ddddd",
    "",
    "eeeeeeeeeeeeeeeeeeeeeeeeee",
    "eeee"
  )
  expect_identical(res_vec, exp_vec)
})

test_that("regression tests for rtables empty title underlying issue", {
  expect_identical(nlines("", max_width = 6), 1L)
  expect_identical(wrap_string("", width = 6), "")
})

test_that("row label wrapping has identical indentation", {
  # NB: this can be also verified with complete tables
  fakedf <- data.frame(all_obs = c(3, 4), row.names = c("Something to wrap", "Also here it is"))
  matform <- basic_matrix_form(fakedf)

  # Inset because why not
  table_inset(matform) <- 1

  # Adding indentation -> it is done before in $strings
  mf_rinfo(matform)$indent <- c(1, 2)
  matform$strings[2, 1] <- paste0(" ", matform$strings[2, 1]) # Does not respect indent_size
  matform$strings[3, 1] <- paste0("  ", matform$strings[3, 1]) # Does not respect indent_size
  expect_error(
    catform <- toString(matform, widths = c(15, 5), hsep = "-"), # Broken indentation, check fails
    "indentation mismatches"
  )

  matform$strings[2, 1] <- paste0("  ", matform$strings[2, 1]) # Does respect indent_size
  matform$strings[3, 1] <- paste0("    ", matform$strings[3, 1]) # Does respect indent_size
  catform <- toString(matform, widths = c(15, 5), hsep = "-") # This reindent (correctly) the rows
  res_vec <- strsplit(catform, "\n")[[1]]
  exp_vec <- c(
    "                   all_o",
    "                    bs  ",
    " -----------------------",
    "    Something to     3  ",
    "   wrap                 ",
    "       Also here     4  ",
    "     it is              "
  )
  expect_identical(res_vec, exp_vec)
})

test_that("wrap_strings work", {
  # \t needs to be escaped -> it should be an error xxx
  str <- list("  , something really  \\tnot  very good", "  but I keep it12   ")

  # size is smaller than bigger word -> dealing with empty spaces
  expect_identical(
    wrap_string(str, 5, collapse = "\n"),
    c(
      "  ,\nsomet\nhing\nreall\ny  \\t\nnot \nvery\ngood",
      " \nbut I\nkeep\nit12"
    )
  )

  # wrap_txt: deprecated, just to test how it behaves
  expect_identical(
    wrap_string(str, 5, collapse = "\n"),
    wrap_txt(str, 5, collapse = "\n")
  )
  expect_identical(
    unlist(wrap_string(str, 5, collapse = NULL)),
    wrap_txt(str, 5, collapse = NULL)
  )

  # Now a string that needs smarter wrapping # Where to start word split?
  str <- "A very long content to_be_wrapped_and_splitted and then something"
  expect_identical(
    length(wrap_string(str, 18)), # more compact
    4L
  )

  # Check for avoidance of infinite loops - C stack exceeding
  expect_identical(wrap_string("6.5", 1), c("6", ".", "5"))
  expect_silent(wrap_string("6.5 and something else. 4.3", 1))

  # Second case of loop (different length - check breaks)
  expect_identical(wrap_string("10. 1 6.5", 2), c("10", " .", "1", "6.", "5"))
  expect_identical(
    wrap_string("10  . 1 6.5 5 . 4", 2),
    c("10", " .", "1", "6.", "5", "5 ", " .", "4")
  )
})

test_that("toString wrapping avoid trimming whitespaces", {
  testdf <- iris[seq_len(5), seq_len(2)]

  # The following will have i = indent size and general col width = 16
  rownames(testdf) <- c(
    "   A pretty long line", # i1 - only line will be in the second line (it has 3 whitespaces)
    "Barbars", # nothing
    "    Continuously long line", # i2 - long line in second line -> matching spacing
    "       D              ", # i0 - empty first line (it has the first piece), second starts D
    "Oltragious" # nothing
  )
  wanna_be_indent <- c(1, 0, 2, 0, 0)
  testdf[, 1] <- wanna_be_indent # so to see the expected indentation
  # NB: multiple spaces are considered as a word in stringi with two spaces around

  bmf <- basic_matrix_form(testdf)

  # Correcting indentation
  mfi <- mf_rinfo(bmf)
  mfi$indent <- wanna_be_indent
  mf_rinfo(bmf) <- mfi
  expect_silent(.check_indentation(bmf)) # internal check for correct indentation setting

  # Reducing the colwidth to force wrapping
  cw <- cw2 <- propose_column_widths(bmf)
  cw[1] <- 16

  bmf_ts <- toString(bmf, widths = cw, hsep = "-")
  res <- strsplit(bmf_ts, "\\n")[[1]]

  expect_identical(
    c(
      "                   Sepal.Length   Sepal.Width",
      "---------------------------------------------",
      "   A pretty long        1             3.5    ",
      "  line                                       ",
      "Barbars                 0              3     ",
      "    Continuously        2             3.2    ",
      "    long line                                ",
      "                        0             3.1    ",
      "D                                            ",
      "Oltragious              0             3.6    "
    ),
    res
  )

  # wrapping this with split words (also white spaces count as a word, we drop multiples)
  cw2[1] <- 9
  bmf_ts <- toString(bmf, widths = cw2, hsep = "-")
  res <- strsplit(bmf_ts, "\\n")[[1]]

  expect_identical(
    c(
      "            Sepal.Length   Sepal.Width",
      "--------------------------------------",
      "   A             1             3.5    ",
      "  pretty                              ",
      "  long                                ",
      "  line                                ",
      "Barbars          0              3     ",
      "    Conti        2             3.2    ",
      "    nuous                             ",
      "    ly                                ",
      "    long                              ",
      "    line                              ",
      "                 0             3.1    ",
      "D                                     ",
      "Oltragiou        0             3.6    ",
      "s                                     "
    ),
    res
  )
})

test_that("toString and wrapping cooperates well with separator divisors", {
  # Fixes #221
  testdf <- iris[seq_len(5), seq_len(2)]
  rownames(testdf) <- paste("State ", LETTERS[seq_len(nrow(testdf))])
  bmf <- basic_matrix_form(testdf)

  # Adding topleft to wrap
  bmf$has_topleft <- TRUE # no setter atm
  mf_strings(bmf)[1, 1] <- "LETTERS"

  sec_seps_df <- mf_rinfo(bmf)[, c("abs_rownumber", "trailing_sep"), drop = FALSE]
  mf_rinfo(bmf)$trailing_sep[c(1, 3, 4)] <- " "
  expect_silent(toString(bmf, widths = c(4, 4, 4)))
})

test_that("max_width is handled correctly as expected", {
  tmp_width <- getOption("width")
  options("width" = 150)
  expect_equal(.handle_max_width(tf_wrap = TRUE, max_width = NULL), 150)
  options("width" = tmp_width)
  expect_null(.handle_max_width(FALSE, NULL))
  suppressMessages(
    expect_warning(
      expect_null(.handle_max_width(FALSE, "asd"))
    )
  )
  expect_equal(.handle_max_width(tf_wrap = TRUE, max_width = 100), 100)
  expect_equal(.handle_max_width(tf_wrap = TRUE, max_width = 100, cpp = 150), 100)
  suppressMessages(
    expect_error(.handle_max_width(tf_wrap = TRUE, max_width = "no"))
  )
  suppressMessages(
    expect_error(.handle_max_width(tf_wrap = TRUE, max_width = "auto"))
  )
  expect_equal(.handle_max_width(
    tf_wrap = TRUE, max_width = "auto",
    inset = 1, colwidths = c(10, 20, 30), col_gap = 2
  ), 65)
})

test_that("wrapping works with truetype font", {
  fspec <- font_spec("Times", 9, 1.2)
  expect_true(is_monospace(font_family = "Courier"))
  open_font_dev(fspec)
  expect_false(is_monospace(fspec))

  ## do the basics work
  orig1 <- "NATIVE"
  wrp1a <- wrap_string(orig1, 4, fontspec = fspec)
  expect_equal(
    wrp1a,
    c("N", "A", "TI", "V", "E")
  )


  ## will it break within a word if it needs to?
  wrp2a <- wrap_string(orig1, 10, fontspec = fspec)
  expect_equal(
    wrp2a,
    c(
      "NATI", # 8.968 spaces wide
      "VE"
    )
  ) # 5.332 spaces wide

  ## will it break within a word if there are some spaces, too

  orig2 <- "some SPACESAREHEREHAHAHAHAHAH"
  wrp2a <- wrap_string(orig2, 20, fontspec = fspec)
  expect_equal(
    wrp2a,
    c(
      "some SPAC",
      "ESAREHE",
      "REHAHAH",
      "AHAHAH"
    )
  )
  nctt2a <- nchar_ttype(wrp2a, fspec, raw = TRUE)
  expect_true(all(head(nctt2a, -1) > 15) && ## none of the wraps are too small
    all(nctt2a <= 20)) ## all wraps are good in the end # nolint
})


test_that("font device plays nice(ish) with other graphics devices", {
  fspec <- font_spec("Times", 8, 1.2)
  expect_silent(open_font_dev(fspec))
  expect_silent(close_font_dev())

  open_font_dev(fspec)
  ## a challenger approaches!!
  filpdf <- tempfile("ohnoes", fileext = ".pdf")
  pdf(filpdf)
  my_pdf_dev <- dev.cur()
  expect_warning(open_font_dev(fspec),
                 "formatters is switching to the font state graphics device ")
  expect_silent(open_font_dev(fspec))
  expect_true(my_pdf_dev %in% dev.list())
  close_font_dev()
  expect_true(my_pdf_dev %in% dev.list())
  dev.off(my_pdf_dev)
})
