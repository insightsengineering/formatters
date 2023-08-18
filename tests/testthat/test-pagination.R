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
    pagindices <- pag_indices_inner(mf_rinfo(dfmf),
                                    25,
                                    min_siblings = 10,
                                    verbose = TRUE)
    expect_equal(length(pagindices[[1]]),
                 21)

    ## min siblings preceding test
    expect_error(pag_indices_inner(mf_rinfo(dfmf),
                                   25,
                                   min_siblings = 30,
                                   verbose = TRUE))



    ## silly coverage for bizarre (/impossible) error cases
    dfmf_b <- basic_matrix_form(mtcars,parent_path = c("root_split", "level1"))
    ## expect_error(pag_indices_inner(mf_rinfo(dfmf_b),
    ##                                8,
    ##                                min_siblings = 0,
    ##                                nosplitin = "root_split",
    ##                                verbose = TRUE))
    expect_error(paginate_to_mpfs(dfmf_b,
                                  lpp = 8 + 2,
                                  min_siblings = 0,
                                  nosplitin = "root_split",
                                  verbose = TRUE),
                 "Unable to find any valid pagination")

    expect_error({mf_rinfo(dfmf) <- mtcars[1:3,]})
    dfmf_sillytopleft <- dfmf
    ## XXXX no setter for this so we're doing something terrible. sadface
    dfmf_sillytopleft$has_topleft <- TRUE
    strs <- mf_strings(dfmf_sillytopleft)
    strs[1,1] <- "ha\nha\nha\nha\nha\nha\n"
    mf_strings(dfmf_sillytopleft) <- strs
    expect_error(formatters:::mform_handle_newlines(dfmf_sillytopleft))

    dfmf_cont <- dfmf
    mf_rinfo(dfmf_cont)$node_class <- "ContentRow"
    ## expect_error(pag_indices_inner(mf_rinfo(dfmf_cont),
    ##                                8,
    ##                                min_siblings = 0,
    ##                                verbose = TRUE))

    expect_error(paginate_to_mpfs(dfmf_cont,
                                  lpp = 8 + 2,
                                  min_siblings = 0,
                                  verbose = TRUE),
                 "Unable to find any valid pagination")

    ## https://github.com/insightsengineering/rtables/issues/318
    dfmf2 <- dfmf
    dfmf2$strings[1, 2] <- "m\npg"
    dfmf2$strings[1, 1] <- "tleft mats"
    dfmf2$has_topleft <- TRUE
    dfmf2 <- formatters:::mform_handle_newlines(dfmf2)
    expect_identical(
        dfmf2$strings[1:2, 1:2],
        matrix(c("tleft mats", "", "m", "pg"), nrow = 2, ncol = 2)
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

    hpagmpfs <- paginate_to_mpfs(df2mf,
                                 lpp = 8 + 2,
                                 min_siblings = 0,
                                 verbose = TRUE)
    expect_true(all(sapply(hpagmpfs,
                           function(x) max(mf_lgrouping(x)) - mf_nrheader(x)) == 2L))


## vpaginds <- vert_pag_indices(df2mf, cpp = 40, verbose = TRUE)

## expect_identical(
##   lengths(vpaginds),
##   c(3L, 3L, 3L, 2L, 1L)
## )

    vpagmpfs <- paginate_to_mpfs(df2mf,
                                 cpp = 40,
                                 verbose = TRUE,
                                 pg_height = 100000)

    expect_identical(sapply(vpagmpfs, ncol),
                     c(3L, 3L, 3L, 2L, 1L))


    vpagmpfs2 <- paginate_to_mpfs(df2mf,
                                  cpp = 39,
                                  verbose = TRUE,
                                  pg_height = 100000)

    expect_identical(
        sapply(vpagmpfs2, ncol),
        c(2L, 2L, 2L, 3L, 2L, 1L)
    )

    vpaginds2 <- vert_pag_indices(df2mf, cpp = 39, verbose = TRUE)

## expect_identical(
##   lengths(vpaginds2),
##   c(2L, 2L, 2L, 3L, 2L, 1L)
## )

    vpaginds3 <- vert_pag_indices(df2mf, cpp = 44, verbose = TRUE, rep_cols = 1L)

    ## make sure it was repeated as requested
    expect_identical(sapply(vpaginds3, function(x) x[1]),
                     rep(1L, 5))

    ## make sure it doesn't appear more than once
    expect_identical(sapply(vpaginds3, function(x) sum(x == 1L)),
                     rep(1L, 5))


    ## they all appear
    expect_equal(sort(unique(unlist(vpaginds3))),
                 1:12)

    expect_identical(lengths(vpaginds3),
                     c(3L, 3L, 3L, 4L, 3L))

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

    expect_error({mf_spans(df3mf) <- matrix(1, nrow = 2, ncol = 3)})

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
    expect_error(paginate_indices(dfmf, lpp = 15, cpp = 10000, verbose = TRUE),
                 "Unable to find any valid pagination")


    expect_error(paginate_indices(dfmf, lpp = 2, cpp = 10000, verbose = TRUE),
                 "Lines of repeated context .* larger than specified lines per page")

    res <- paginate_to_mpfs(dfmf, pg_width = 4, pg_height = 4, margins = rep(0, 4),
                            min_siblings = 0, verbose = TRUE)


    ## coverage for forced pagination support
    dfmf2 <- structure(dfmf, class = c("fakeclass", class(dfmf)))

    mf_subset_rows <- formatters:::mpf_subset_rows
    setOldClass(class(dfmf2))
    setMethod("do_forced_paginate",
              "fakeclass",
              function(obj) {
        pt1 <- mf_subset_rows(obj, 1)
        class(pt1) <- setdiff(class(obj), "fakeclass")
        pt2 <- mf_subset_rows(obj, 2:32)
        class(pt2) <- setdiff(class(obj), "fakeclass")
        list(pt1, pt2)
    })

    res <- paginate_to_mpfs(dfmf2, pg_width = 4, pg_height = 4,
                            margins = rep(0, 4),
                            min_siblings = 0, verbose = TRUE)

    expect_identical(page_lcpp( pg_width = 4, pg_height = 4,
                               margins = rep(0, 4)),
                     list(cpp = 60, lpp = 36))


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


    expect_error(paginate_indices(dfmf2),
                 "forced pagination is required for this object")
    ## diagnose_pagination smoke test coverage
    ## actual functionality cannot be tested because it relies on capturing
    ## the message stream which testthat is already doing and
    ## cannot be piggybacked on.

    ## the ;TRUE is a hack becasue expect_success didn't do what it seems like it should
    ## this will fail if the first part befoer the ; throws an error.
    expect_true({diagnose_pagination(dfmf, lpp = NULL, cpp = 60); TRUE})
    expect_true({dgnostic <- diagnose_pagination(dfmf, lpp = 60, cpp = NULL); TRUE})
    expect_true({dgnostic <- diagnose_pagination(dfmf2, pg_width = 4, pg_height = 4,
                                                 margins = rep(0, 4),
                                                 min_siblings = 0); TRUE})

    ## diagnose_pagination when no valid pagination is found
    expect_true({dgnostic <- diagnose_pagination(dfmf_b,
                                                 lpp = 8 + 2,
                                                 min_siblings = 0,
                                                 nosplitin = "root_split"); TRUE})

})
test_that("page to lcpp stuff works", {
    expect_identical(
        page_lcpp(margins = c(
                      top = .5, bottom = .5,
                      left = .65, right = .65
                  ), font_size = 12),
        list(cpp = 72, lpp = 60)
    )

    expect_identical(formatters:::calc_lcpp(),
                     formatters:::calc_lcpp(page_type = "letter"))

})


test_that("non-monospaced fonts are caught", {

    ## non-monospaced fonts
    expect_error(page_lcpp(font_family = "Times"), "does not appear to be monospaced")

    expect_identical(
        page_lcpp("a4"),
        page_lcpp(
            pg_width = formatters:::pg_dim_names$a4[[1]],
            pg_height = formatters:::pg_dim_names$a4[[2]]
        )
    )
})

test_that("spans and string matrix match after pagination
          when table has single column", {

  df <- as.data.frame(mtcars[, 1])
  test <- basic_matrix_form(df)
  pag_test <- paginate_to_mpfs(test)

  expect_identical(
    dim(pag_test[[1]]$spans), dim(pag_test[[1]]$strings))
})
