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
        "\nppppp ppppp\nppppp\n")
    expect_identical(printform, expected)

    printform2 <- toString(matform, tf_wrap = TRUE, max_width = 10, hsep = "-")
    expected2 <- paste0(
        "ttttt\nttttt\nttttt\nsssss\nsssss\nsssss\n\n--------------\n ",
        "      all_obs\n--------------\nmean   3      \n--------------\n",
        "\nfffff\nfffff\nfffff\n",
        "\nppppp\nppppp\nppppp\n")
    expect_identical(printform2, expected2)

    matform2 <- matform
    table_inset(matform2) <- 2
    matform2$ref_footnotes <- c("ref ftnote")
    ## make sure interaction with inset works
    printform3 <- toString(matform2, tf_wrap = TRUE, max_width = 11, hsep = "-")
    expected3 <-  paste0(
        "ttttt ttttt\nttttt\nsssss sssss\nsssss\n\n  --------------\n ",
        "        all_obs\n  --------------\n  mean   3      \n  --------------\n",
        "\n  ref\n  ftnote\n  --------------\n\n  fffff\n  fffff\n  fffff\n",
        "\nppppp ppppp\nppppp\n")
    expect_identical(printform3, expected3)

})

test_that("toString() throws a warning when newline is in string", {
    bmf <- basic_matrix_form(iris)
    bmf$main_title <- "some\nvery\nspacious\ntitle"
    bmf$prov_footer <- "some\nvery\nspacious\nfooter"
    bmf$ref_footnotes <- "some\nvery\nspacious\nreference"
    expect_silent(toString(bmf, tf_wrap = FALSE))
    expect_warning(toString(bmf, tf_wrap = TRUE))
})

test_that("works with words that are too big (no warning)", {
    bmf <- basic_matrix_form(mtcars[1:2, 1:2])
    bmf$main_title <- "TITLE"
    bmf$subtitles <- "SUB TITLE IS"
    bmf$page_titles <- "PAGE TITLE"
    bmf$main_footer <- "FOOTER"
    bmf$prov_footer <- c("", "PROV FOOTER")
    bmf$ref_footnotes <- "REFERENCE"
    result <- toString(bmf, tf_wrap = TRUE, max_width = 4, hsep = "-")
    res_vec <- strsplit(result, "\n")[[1]]
    exp_vec <- c("TITL", "E",
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
                 "PROV", "FOOT", "ER")
    expect_identical(res_vec, exp_vec)
})



test_that("auto works with inset and col_gap", {
  bmf <- basic_matrix_form(mtcars[1:2, 1:2])
  bmf$col_gap <- 3
  bmf$table_inset <- 1
  bmf$main_title <- strrep("a", 30)
  bmf$subtitles <- strrep("b", 30)
  bmf$page_titles <- strrep("c", 30)
  bmf$main_footer <- strrep("d", 30)
  bmf$prov_footer <- strrep("e", 30)
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
    expect_identical(nlines("", 6), 1L)
    expect_identical(wrap_string("", 6), "")
})
