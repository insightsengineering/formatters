context("table_inset and titles and footers word wrapping")

test_that("tf_wordwrap and table inset work (including together)", {
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
  expected <- paste0(
    "ttttt ttttt\nttttt\nsssss sssss\nsssss\n\n--------------\n ",
    "      all_obs\n--------------\nmean   3      \n--------------\n",
    "\nfffff fffff\nfffff\n",
    "\nppppp ppppp\nppppp\n"
  )
  expect_identical(printform, expected)

  printform2 <- toString(matform, tf_wrap = TRUE, max_width = 10, hsep = "-")
  expected2 <- paste0(
    "ttttt\nttttt\nttttt\nsssss\nsssss\nsssss\n\n--------------\n ",
    "      all_obs\n--------------\nmean   3      \n--------------\n",
    "\nfffff\nfffff\nfffff\n",
    "\nppppp\nppppp\nppppp\n"
  )
  expect_identical(printform2, expected2)

  matform2 <- matform
  table_inset(matform2) <- 2
  matform2$ref_footnotes <- c("ref ftnote")
  ## make sure interaction with inset works
  printform3 <- toString(matform2, tf_wrap = TRUE, max_width = 11, hsep = "-")
  expected3 <- paste0(
    "ttttt ttttt\nttttt\nsssss sssss\nsssss\n\n  --------------\n ",
    "        all_obs\n  --------------\n  mean   3      \n  --------------\n",
    "\n  ref\n  ftnote\n  --------------\n\n  fffff\n  fffff\n  fffff\n",
    "\nppppp ppppp\nppppp\n"
  )
  expect_identical(printform3, expected3)

  ## coverage for allfooter
  expect_identical(all_footers(matform),
                   c(main_footer(matform),
                     prov_footer(matform)))

})

test_that("toString() throws a warning when newline is in string", {
  bmf <- basic_matrix_form(iris)
  main_title(bmf) <- "some\nvery\nspacious\ntitle"
  prov_footer(bmf) <- "some\nvery\nspacious\nfooter"
  bmf$ref_footnotes <- "some\nvery\nspacious\nreference"
  expect_silent(toString(bmf, tf_wrap = FALSE))
  expect_warning(toString(bmf, tf_wrap = TRUE))
})

test_that("works with words that are too big (no warning)", {
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
    "Mazda RX4       21    6  ",
    "Mazda RX4 Wag   21    6  ",
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

test_that("auto works with inset and col_gap", {
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
    " Mazda RX4       21    6  ",
    " Mazda RX4 Wag   21    6  ",
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
  expect_identical(wrap_string("", max_width = 6), "")
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
  catform <- toString(matform, widths = c(15, 5)) # This reindent (correctly) the rows
  res_vec <- strsplit(catform, "\n")[[1]]
  exp_vec <- c(
    "                   all_o",
    "                    bs  ",
    " ———————————————————————",
    "   Something to    3    ",
    "   wrap                 ",
    "     Also here     4    ",
    "     it is              "
  )
  expect_identical(res_vec, exp_vec)
})
