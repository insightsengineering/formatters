test_that("pagination works", {
  ## https://github.com/insightsengineering/formatters/issues/18
  dfmf <- basic_matrix_form(mtcars)
  expect_identical(main_footer(dfmf), "")
  ftmsg <- "my footer is here"
  main_footer(dfmf) <- ftmsg
  expect_identical(
    main_footer(dfmf),
    ftmsg
  )

  ## min_siblings following siblings test
  pagindices <- pag_indices_inner(
    mf_rinfo(dfmf),
    25,
    min_siblings = 10,
    verbose = TRUE
  )
  expect_equal(
    length(pagindices[[1]]),
    21
  )

  ## min siblings preceding test
  expect_error(pag_indices_inner(
    mf_rinfo(dfmf),
    25,
    min_siblings = 30,
    verbose = TRUE
  ))

  ## silly coverage for bizarre (/impossible) error cases
  dfmf_b <- basic_matrix_form(mtcars, parent_path = c("root_split", "level1"))
  ## expect_error(pag_indices_inner(mf_rinfo(dfmf_b),
  ##                                8,
  ##                                min_siblings = 0,
  ##                                nosplitin = "root_split",
  ##                                verbose = TRUE))
  expect_error(
    paginate_to_mpfs(
      dfmf_b,
      lpp = 8 + 2,
      min_siblings = 0,
      nosplitin = "root_split",
      verbose = TRUE
    ),
    "Unable to find any valid pagination"
  )

  expect_error(mf_rinfo(dfmf) <- mtcars[1:3, ])
  dfmf_sillytopleft <- dfmf
  ## XXXX no setter for this so we're doing something terrible. sadface
  dfmf_sillytopleft$has_topleft <- TRUE
  strs <- mf_strings(dfmf_sillytopleft)
  strs[1, 1] <- "ha\nha\nha\nha\nha\nha\n"
  mf_strings(dfmf_sillytopleft) <- strs
  expect_silent(mform_handle_newlines(dfmf_sillytopleft))

  dfmf_cont <- dfmf
  mf_rinfo(dfmf_cont)$node_class <- "ContentRow"
  ## expect_error(pag_indices_inner(mf_rinfo(dfmf_cont),
  ##                                8,
  ##                                min_siblings = 0,
  ##                                verbose = TRUE))

  expect_identical(
    length(paginate_to_mpfs(
      dfmf_cont,
      lpp = 8 + 2,
      min_siblings = 0,
      verbose = TRUE
    )),
    7L
  )

  ## https://github.com/insightsengineering/rtables/issues/318
  dfmf2 <- dfmf
  dfmf2$strings[1, 2] <- "m\npg"
  dfmf2$strings[1, 1] <- "tleft mats"
  dfmf2$has_topleft <- TRUE
  dfmf2 <- mform_handle_newlines(dfmf2)
  expect_identical(
    dfmf2$strings[1:2, 1:2],
    matrix(c("", "tleft mats", "m", "pg"), nrow = 2, ncol = 2)
  )

  ## https://github.com/insightsengineering/formatters/issues/77

  dfmf3 <- dfmf
  mf_rinfo(dfmf3)$trailing_sep[31] <- "-"
  str <- toString(dfmf3)
  expect_true(grepl("Volvo 142E", str))

  strout <- toString(dfmf)
  expect_true(any(grepl(ftmsg, strout)))

  df2 <- mtcars
  df2$extra <- paste("row", seq_len(NROW(df2)), "haha", sep = "\n")

  df2mf <- basic_matrix_form(df2)

  expect_identical(
    basic_pagdf(row.names(df2),
      extents = 3L
    ),
    mf_rinfo(df2mf)
  )

  ## hpaginds <- pag_indices_inner(mf_rinfo(df2mf),
  ##   8,
  ##   min_siblings = 0,
  ##   verbose = TRUE
  ## )

  ## expect_true(all(lengths(hpaginds) == 2L))

  hpagmpfs <- paginate_to_mpfs(
    df2mf,
    lpp = 8 + 2,
    min_siblings = 0,
    verbose = TRUE
  )
  expect_true(all(sapply(hpagmpfs, function(x) max(mf_lgrouping(x)) - mf_nrheader(x)) == 2L))

  ## vpaginds <- vert_pag_indices(df2mf, cpp = 40, verbose = TRUE)

  ## expect_identical(
  ##   lengths(vpaginds),
  ##   c(3L, 3L, 3L, 2L, 1L)
  ## )

  vpagmpfs <- paginate_to_mpfs(
    df2mf,
    cpp = 40,
    verbose = TRUE,
    pg_height = 100000
  )

  expect_identical(
    sapply(vpagmpfs, ncol),
    c(3L, 3L, 3L, 2L, 1L)
  )

  vpagmpfs2 <- paginate_to_mpfs(
    df2mf,
    cpp = 39,
    verbose = TRUE,
    pg_height = 100000
  )

  expect_identical(
    sapply(vpagmpfs2, ncol),
    c(2L, 2L, 2L, 3L, 2L, 1L)
  )

  vpaginds2 <- vert_pag_indices(df2mf, cpp = 39, verbose = TRUE, fontspec = NULL)

  ## expect_identical(
  ##   lengths(vpaginds2),
  ##   c(2L, 2L, 2L, 3L, 2L, 1L)
  ## )

  vpaginds3 <- vert_pag_indices(df2mf, cpp = 44, verbose = TRUE, rep_cols = 1L, fontspec = NULL)

  ## make sure it was repeated as requested
  expect_identical(
    sapply(vpaginds3, function(x) x[1]),
    rep(1L, 5)
  )

  ## make sure it doesn't appear more than once
  expect_identical(
    sapply(vpaginds3, function(x) sum(x == 1L)),
    rep(1L, 5)
  )

  ## they all appear
  expect_equal(
    sort(unique(unlist(vpaginds3))),
    1:12
  )

  expect_identical(
    lengths(vpaginds3),
    c(3L, 3L, 3L, 4L, 3L)
  )

  df3 <- data.frame(
    x = 1:5, y = c(1:3, 8, 9),
    row.names = c("spna", "spnb", "spnc", "sep1", "sep2")
  )

  df3mf <- basic_matrix_form(df3)

  spnmat <- mf_spans(df3mf)

  spnmat[2:3, 2:3] <- 2

  df3mf$spans <- spnmat
  df3mf$display[2:3, 3] <- FALSE
  df3mf$aligns[2:6, 2:3] <- "center"
  strout <- toString(df3mf)
  expect_false(grepl("1[[:space:]]*1", strout))
  expect_true(grepl("3[[:space:]]*3", strout))

  expect_identical(
    spread_integer(7, 3),
    c(3, 2, 2)
  )

  expect_error(spread_integer(3.5, 2))

  expect_error(
    {
      table_inset(df3mf) <- -1
    },
    "invalid value for table_inset"
  )

  expect_error(mf_spans(df3mf) <- matrix(1, nrow = 2, ncol = 3))

  ## matrix_form on a matrix form a no op
  expect_identical(df3mf, matrix_form(df3mf))
  expect_identical(divider_height(df3mf), 1L)
  expect_identical(subtitles(df3mf), character())
  expect_identical(page_titles(df3mf), character())
  prov_footer(df3mf) <- "file: myfile.txt"
  expect_identical(prov_footer(df3mf), "file: myfile.txt")
  expect_identical(nlines(NULL), 0L)
  expect_identical(nlines("hi\nthere"), 2L)

  ## coverage for handling of ref footnotes in pagination machinery
  ## also covered partially in closely related export test

  dfmf <- make_basemf_fnotes()
  expect_error(
    paginate_indices(dfmf, lpp = 15, cpp = 10000, verbose = TRUE),
    "Unable to find any valid pagination"
  )

  expect_error(
    paginate_indices(dfmf, lpp = 2, cpp = 10000, verbose = TRUE),
    "Lines of repeated context .* larger than specified lines per page"
  )

  res <- paginate_to_mpfs(
    dfmf,
    pg_width = 4, pg_height = 4, margins = rep(0, 4),
    min_siblings = 0, verbose = TRUE
  )

  ## coverage for forced pagination support
  dfmf2 <- structure(dfmf, class = c("fakeclass", class(dfmf)))

  setOldClass(class(dfmf2))
  setMethod(
    "do_forced_paginate",
    "fakeclass",
    function(obj) {
      pt1 <- mpf_subset_rows(obj, 1)
      class(pt1) <- setdiff(class(obj), "fakeclass")
      pt2 <- mpf_subset_rows(obj, 2:32)
      class(pt2) <- setdiff(class(obj), "fakeclass")
      list(pt1, pt2)
    }
  )

  res <- paginate_to_mpfs(
    dfmf2,
    pg_width = 4, pg_height = 4,
    margins = rep(0, 4),
    min_siblings = 0, verbose = TRUE
  )

  expect_identical(page_lcpp(pg_width = 4, pg_height = 4, margins = rep(0, 4)), list(cpp = 60, lpp = 36))

  ## first vertical pagination is "forced" after row 1,
  ## 2 additional vertical paginations within second "forced page" (3 total)
  ## 3 horizontal paginations
  ## 9 pages
  expect_identical(length(res), 9L)
  nrs <- vapply(res, mf_nrow, 1)
  expect_true(all(nrs[1:3] == 1))
  expect_true(all(nrs[4:6] == 19))
  expect_true(all(nrs[7:9] == 12))
  resnls <- vapply(res, function(x) nlines(toString(x)), 1L)
  expect_true(all(resnls <= 36))
  expect_equal(resnls[4], 36)

  expect_error(paginate_indices(dfmf2), "forced pagination is required for this object")
  ## diagnose_pagination smoke test coverage
  ## actual functionality cannot be tested because it relies on capturing
  ## the message stream which testthat is already doing and
  ## cannot be piggybacked on.

  ## the ;TRUE is a hack becasue expect_success didn't do what it seems like it should
  ## this will fail if the first part befoer the ; throws an error.
  expect_true({
    diagnose_pagination(dfmf, lpp = NULL, cpp = 60)
    TRUE
  })
  expect_true({
    dgnostic <- diagnose_pagination(dfmf, lpp = 60, cpp = NULL)
    TRUE
  })
  expect_true({
    dgnostic <- diagnose_pagination(dfmf2, pg_width = 4, pg_height = 4, margins = rep(0, 4), min_siblings = 0)
    TRUE
  })

  ## diagnose_pagination when no valid pagination is found
  expect_true({
    dgnostic <- diagnose_pagination(dfmf_b, lpp = 8 + 2, min_siblings = 0, nosplitin = "root_split")
    TRUE
  })
})

test_that("page to lcpp stuff works", {
  expect_identical(
    page_lcpp(margins = c(
      top = .5, bottom = .5,
      left = .65, right = .65
    ), font_size = 12),
    list(cpp = 72, lpp = 60)
  )

  expect_identical(
    calc_lcpp(fontspec = NULL),
    calc_lcpp(page_type = "letter", fontspec = NULL)
  )
})


test_that("Page type dictates page dims", {
  expect_identical(
    page_lcpp("a4"),
    page_lcpp(
      pg_width = pg_dim_names$a4[[1]],
      pg_height = pg_dim_names$a4[[2]]
    )
  )
})

test_that("spans and string matrix match after pagination when table has single column", {
  df <- as.data.frame(mtcars[, 1])
  test <- basic_matrix_form(df)
  pag_test <- paginate_to_mpfs(test)

  expect_identical(
    dim(pag_test[[1]]$spans),
    dim(pag_test[[1]]$strings)
  )
})



test_that("pag_num works in paginate_to_mpfs and export_as_txt", {
  tst <- basic_matrix_form(mtcars, add_decoration = TRUE)
  pg_tst <- paginate_to_mpfs(tst, page_num = TRUE)

  print_pg_tst <- lapply(pg_tst, function(x) strsplit(toString(x), "\n")[[1]])

  expect_equal(nchar(print_pg_tst[[1]][46]), 105) # 105 seems the default

  pg_tst <- paginate_to_mpfs(tst, cpp = 50, lpp = 20, page_num = "Non fixed Paging {i} of {n}")

  print_pg_tst <- lapply(pg_tst, function(x) strsplit(toString(x), "\n")[[1]])

  # lpp is respected
  expect_true(all(sapply(lengths(print_pg_tst), function(x) x <= 20)))

  # cpp is respected exactly for last line (pages)
  expect_true(all(sapply(print_pg_tst, function(x) nchar(tail(x, 1)) == 50)))

  # page_num is respected
  expect_identical(
    print_pg_tst[[1]][20],
    "                          Non fixed Paging 1 of 18"
  )
  expect_identical(
    print_pg_tst[[4]][20],
    "                          Non fixed Paging 4 of 18"
  )

  # lets go to the minimum cpp and break it -> propose_column_widths(tst)
  expect_error(
    pg_tst_off <- paginate_to_mpfs(tst,
      cpp = 19 + 5 + 3, # rownames + max colwidths + 3 (extra colgap)
      lpp = 20,
      page_num = "This is too long, it is breaking"
    ),
    "Page numbering string \\(page_num\\) is too wide to fit the desired page size width \\(cpp\\)."
  )

  # Very stringent test with export_as_txt
  pg_tst_exp <- export_as_txt(tst,
    cpp = 50, lpp = 20,
    page_num = "Non fixed Paging {i} of {n}",
    page_break = "OoOoO"
  )
  pages_tst_exp <- lapply(strsplit(pg_tst_exp, "OoOoO")[[1]], function(x) strsplit(x, "\n")[[1]])
  expect_equal(pages_tst_exp, print_pg_tst)
})

test_that("colgap is applied correctly during pagination with and without row labels ", {
  df <- mtcars

  test <- basic_matrix_form(df, ignore_rownames = TRUE)
  res <- paginate_to_mpfs(test, cpp = 10, col_gap = 50)
  expect_equal(length(res),
               ncol(test))
  test2 <- basic_matrix_form(df, ignore_rownames = FALSE)
  expect_error(paginate_to_mpfs(test2, cpp = 50, col_gap = 50),
               ".*Unable to find any valid pagination split for page 1.*")
})

test_that("colwidths and num_rep_cols work when using lists of tables and listings", {
  bmf <- basic_matrix_form(mtcars, ignore_rownames = TRUE)
  blmf <- basic_listing_mf(mtcars, keycols = c("vs", "gear"))
  expect_equal(num_rep_cols(bmf), 0L)
  expect_equal(num_rep_cols(blmf), 2L)

  l_mf <- list(bmf, blmf)

  output <- export_as_txt(l_mf, page_num = "page {i} of {n}", cpp = 90, colwidths = rep(8, 11))
  nchar_lines <- sapply(strsplit(output, "\n")[[1]], nchar)

  expect_equal(max(nchar_lines), 90)
  expect_true(grepl(names(nchar_lines)[length(nchar_lines)], pattern = "page 4 of 4"))
  expect_equal(unname(nchar_lines[length(nchar_lines)]), 90) # last line is full (page number)

  sorted_tnl <- sort(table(nchar_lines), decreasing = TRUE)

  expect_equal(unname(sorted_tnl[names(sorted_tnl) == "90"]), 4) # there are 4 pages (with page number)
  expect_equal(names(sorted_tnl[c(1, 2)]), c("85", "52"))

  expect_error(
    export_as_txt(l_mf, colwidths = rep(8, 10)),
    "non-null colwidths argument must have length ncol"
  )

  expect_silent(
    output <- export_as_txt(l_mf, page_num = "page {i} of {n}", cpp = 90, colwidths = rep(8, 11), num_rep_cols = 2)
  )
})

test_that("rep_cols works as intended for listings and tables", {
  bmf <- basic_matrix_form(mtcars, ignore_rownames = FALSE)
  blmf <- basic_listing_mf(mtcars, keycols = c("vs", "gear"))
  expect_equal(num_rep_cols(bmf), 0L) # repeated rowlabels are excluded from num_rep_cols
  expect_equal(num_rep_cols(blmf), 2L)

  out <- export_as_txt(bmf, rep_cols = 2, cpp = 90) %>%
    strsplit("\n") %>%
    unlist()
  expect_true(grepl(out[35], pattern = "mpg")) # mpg is repeated
  expect_true(grepl(out[35], pattern = "cyl")) # cyl is repeated

  out <- export_as_txt(blmf, cpp = 70) %>%
    strsplit("\n") %>%
    unlist()
  expect_true(grepl(out[51], pattern = "vs")) # vs is repeated
  expect_true(grepl(out[51], pattern = "gear")) # gear is repeated

  out <- export_as_txt(blmf, rep_cols = 1, cpp = 70) %>%
    strsplit("\n") %>%
    unlist()
  expect_true(grepl(out[51], pattern = "vs")) # vs is repeated
  expect_false(grepl(out[51], pattern = "gear")) # gear is NOT repeated
})
