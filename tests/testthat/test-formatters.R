values <- c(5.123456, 7.891112)
test_that("formats work", {
    ## listing supported formats and enuring they all read as valid
    forms <- list_valid_format_labels()

    res <- sapply(forms, function(vc) all(sapply(vc, is_valid_format)))
    expect_true(all(res))

    ## core formatter tests for format strings

    expect_identical(
        format_value(values[1], format = "xx"),
        paste(values[1])
    )

    expect_identical(
  format_value(values[1], format = "xx."),
  "5"
  )

    expect_identical(
        format_value(values[1], format = "xx.x"),
        "5.1"
    )

    expect_identical(
        format_value(values[1], format = "xx.xx"),
        "5.12"
    )

    expect_identical(
        format_value(values[1], format = "xx.xxx"),
        "5.123"
    )

    expect_identical(
        format_value(values[1], format = "xx.xxxx"),
        "5.1235"
    )

    expect_identical(
        format_value(values[1], format = "xx%"),
        paste0(values[1] * 100, "%")
    )

    expect_identical(
        format_value(values[1], format = "xx.%"),
        "512%"
    )

    expect_identical(
        format_value(values[1], format = "xx.x%"),
        "512.3%"
    )

    expect_identical(
        format_value(values[1], format = "xx.xx%"),
        "512.35%"
    )

    expect_identical(
        format_value(values[1], format = "xx.xxx%"),
        "512.346%"
    )

    expect_identical(
        format_value(values[1], format = ">999.9"),
        "5.1"
    )

    expect_identical(
        format_value(10000, format = ">999.9"),
        ">999.9"
    )

    expect_identical(
        format_value(values[1], format = ">999.99"),
        "5.12"
    )

    expect_identical(
        format_value(10000, format = ">999.99"),
        ">999.99"
    )

    expect_identical(
        format_value(.0004, format = "x.xxxx | (<0.0001)"),
        "0.0004"
    )

    expect_identical(
        format_value(.00004, format = "x.xxxx | (<0.0001)"),
        "<0.0001"
    )

    expect_identical(
        format_value(values, format = "xx / xx"),
        paste(values, collapse = " / ")
    )

    expect_identical(
        format_value(values, format = "xx. / xx."),
        "5 / 8"
    )

    expect_identical(
        format_value(values, format = "xx.x / xx.x"),
        "5.1 / 7.9"
    )

    expect_identical(
        format_value(values, format = "xx.xx / xx.xx"),
        "5.12 / 7.89"
    )

    expect_identical(
        format_value(values, format = "xx.xxx / xx.xxx"),
        "5.123 / 7.891"
    )

    expect_identical(
        format_value(values, format = "xx (xx%)"),
        paste0(values[1], " (", values[2] * 100, "%)")
    )

    expect_identical(
        format_value(values, format = "xx (xx.%)"),
        paste0(values[1], " (789%)")
    )

    expect_identical(
        format_value(values, format = "xx. (xx.%)"),
        paste0(5, " (789%)")
    )

    expect_identical(
        format_value(values, format = "xx (xx.x%)"),
        paste0(values[1], " (789.1%)")
    )

    expect_identical(
        format_value(values, format = "xx (xx.xx%)"),
        paste0(values[1], " (789.11%)")
    )

    expect_identical(
        format_value(values, format = "xx.x (xx.x%)"),
        "5.1 (789.1%)"
    )

    expect_identical(
        format_value(values, format = "xx.xx (xx.xx%)"),
        "5.12 (789.11%)"
    )

    expect_identical(
        format_value(values, format = "xx.x (xx.x%)"),
        "5.1 (789.1%)"
    )

    expect_identical(
        format_value(values, format = "(xx, xx)"),
        paste0("(", values[1], ", ", values[2], ")")
    )

    expect_identical(
        format_value(values, format = "(xx., xx.)"),
        "(5, 8)"
    )

    expect_identical(
        format_value(values, format = "(xx.x, xx.x)"),
        "(5.1, 7.9)"
    )

    expect_identical(
        format_value(values, format = "(xx.xx, xx.xx)"),
        "(5.12, 7.89)"
    )

    expect_identical(
        format_value(values, format = "(xx.xxx, xx.xxx)"),
        "(5.123, 7.891)"
    )

    expect_identical(
        format_value(values, format = "(xx.xxxx, xx.xxxx)"),
        "(5.1235, 7.8911)"
    )

    expect_identical(
        format_value(values, format = "xx - xx"),
        paste(values, collapse = " - ")
    )

    expect_identical(
        format_value(values, format = "xx.x - xx.x"),
        "5.1 - 7.9"
    )

    expect_identical(
        format_value(values, format = "xx.xx - xx.xx"),
        "5.12 - 7.89"
    )

    expect_identical(
        format_value(values, format = "xx (xx%)"),
        paste0(values[1], " (", values[2] * 100, "%)")
    )

    expect_identical(
        format_value(values, format = "xx (xx)"),
        paste0(values[1], " (", values[2], ")")
    )


    expect_identical(
        format_value(values, format = "xx (xx.)"),
        paste0(values[1], " (8)")
    )


    expect_identical(
        format_value(values, format = "xx (xx.x)"),
        paste0(values[1], " (7.9)")
    )

    expect_identical(
        format_value(values, format = "xx (xx.xx)"),
        paste0(values[1], " (7.89)")
    )


    expect_identical(
        format_value(values, format = "xx. (xx.)"),
        paste0(5, " (8)")
    )


    expect_identical(
        format_value(values, format = "xx.x (xx.x)"),
        "5.1 (7.9)"
    )

    expect_identical(
        format_value(values, format = "xx.xx (xx.xx)"),
        "5.12 (7.89)"
    )

    expect_identical(
        format_value(values, format = "xx.x, xx.x"),
        "5.1, 7.9"
    )

    expect_identical(
        format_value(values, format = "xx.x to xx.x"),
        "5.1 to 7.9"
    )

    expect_identical(
        format_value(c(values, 10.1235), format = "xx. (xx. - xx.)"),
        "5 (8 - 10)"
    )

    expect_identical(
        format_value(c(values, 10.1235), format = "xx.x (xx.x - xx.x)"),
        "5.1 (7.9 - 10.1)"
    )

    expect_identical(
        format_value(c(values, 10.1235), format = "xx.xx (xx.xx - xx.xx)"),
        "5.12 (7.89 - 10.12)"
    )

    expect_identical(
        format_value(c(values, 10.1235), format = "xx.xxx (xx.xxx - xx.xxx)"),
        "5.123 (7.891 - 10.124)"
    )

    expect_identical(format_value(NULL, "xx"), "")

    expect_identical(
        format_value(5.123, "xx.x", output = "html"),
        list("5.1" = htmltools::tagList(format_value(5.123, "xx.x"), NULL))
    )

    expect_identical(format_value(c(500,1), "N=xx (xx%)"),
                     "N=500 (100%)")

    ## errors

    expect_error(format_value(5.1, "abcd"), "unknown format label")
    expect_error(format_value(5.1, "xx - xx"), "are of different length")

    expect_error(format_value(c(5.1, 2, 3), "xx - xx"), "are of different length")
    ## handling NAs

    results <- vapply(forms[["1d"]], function(fmt) format_value(NA, format = fmt), "")
    justnastr <- results == "NA"

    expect_true(all(justnastr))

    expect_identical(
        format_value(NA, "xx.", na_str = "-"),
        "-"
    )
    expect_identical(
        format_value(NA, "xx", na_str = "-"),
        "-"
    )

    ## trailing 0s are correct
    expect_identical(format_value(0, "xx."), "0")
    expect_identical(format_value(0, "xx.x"), "0.0")
    expect_identical(format_value(0, "xx.xx"), "0.00")
    expect_identical(format_value(0, "xx.xxx"), "0.000")
    expect_identical(format_value(0, "xx.xxxx"), "0.0000")


    expect_identical(
        format_value(c(NA, NA), format = "xx.x - xx.x", na_str = c("hi", "lo")),
        "hi - lo"
    )

    expect_identical(
        format_value(c(NA, NA), format = "xx.x - xx.x", na_str = "what"),
        "what"
    )

    expect_identical(
        format_value(c(NA, 5.2), format = "xx.x - xx.x", na_str = "what"),
        "what - 5.2"
    )

    expect_identical(
        format_value(c(NA, 5.2), format = "xx.x - xx.x", na_str = c("hi", "lo")),
        "hi - 5.2"
    )

    expect_identical(
        format_value(NA, format = "xx.x", na_str = character()),
        "NA"
    )

    expect_identical(
        format_value(NA, format = "xx.x", na_str = NA_character_),
        "NA"
    )

})

test_that("sprintf formats work", {
    ## sprintf_format functionality
    myfun <- sprintf_format("hi there %1.4f")
    expect_true(is_valid_format(myfun))
    expect_identical(
        format_value(pi, format = myfun),
        "hi there 3.1416"
    )
})


test_that("labels and miscellany", {
    thing <- 5.1234
    expect_true(is.null(obj_label(thing)))
    obj_label(thing) <- "hi thing"
    expect_identical(obj_label(thing), "hi thing")
    expect_true(is.null(obj_format(thing)))
    obj_format(thing) <- "xx.x"
    expect_identical(
        format_value(thing, obj_format(thing)),
        "5.1"
    )


    ## labels

    x <- 15
    expect_identical(
        obj_label(with_label(x, "hi")),
        "hi"
    )

    mydf <- mtcars
    lbls <- paste("LBL: ", names(mydf))
    var_labels(mydf) <- lbls
    expect_identical(
        var_labels(mydf),
        setNames(
            lbls,
            names(mydf)
        )
    )

    mydf <- var_relabel(mydf, mpg = "New MPG")
    expect_identical(
        var_labels(mydf),
        c(mpg = "New MPG", setNames(
                               lbls[-1],
                               names(mydf)[-1]
                           ))
    )


    expect_true(all(is.na(var_labels(var_labels_remove(mydf)))))
})

test_that("all valid format labels can be applied without error", {
    ## additional full smoke test of labels without output checking

    values2 <- c(values, 987)
    labs <- list_valid_format_labels()

    r1 <- vapply(
        labs[["1d"]],
        function(lb) {
        tmp <- format_value(values2[1], lb)
        TRUE
    }, NA
    )

    expect_true(all(r1))

    r2 <- vapply(
        labs[["2d"]],
        function(lb) {
        tmp <- format_value(values2[1:2], lb)
        TRUE
    }, NA
    )

    expect_true(all(r2))


    r3 <- vapply(
        labs[["3d"]],
        function(lb) {
        tmp <- format_value(values2, lb)
        TRUE
    }, NA
    )

    expect_true(all(r3))

    expect_identical(
        var_labels(data.frame()),
        character()
    )
})

## silly coverage things
expect_identical(padstr("hi", 4, "center"),
                 " hi ")
expect_identical(padstr("hi", 4, "left"),
                 "hi  ")
expect_identical(padstr("hi", 4, "right"),
                 "  hi")
expect_identical(padstr(NA, 4, "center"),
                 "<NA>")

expect_error(padstr(c("hi", "lo"), 5))
expect_error(padstr(5, "hi"))


expect_identical(spans_to_viscell(c(2, 2, 2, 2, 1, 3, 3, 3)),
                 c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))

expect_equal(nlines(character()), 0)

expect_error(page_dim("wakawakawaka"))

## XXX this is a very stupid test that has NO VALUE
stupidobj <- NA_real_
obj_na_str(stupidobj) <- "wat"
obj_format(stupidobj) <- "xx.x"
expect_silent(obj_align(stupidobj) <- "left") # setter for ANY align
expect_identical(format_value(stupidobj,
                              format = obj_format(stupidobj),
                              na_str = obj_na_str(stupidobj)),
                 "wat")
expect_identical(obj_align(stupidobj), "left") # getter for ANY align

## XXX I'm not sure if we use this functionality anywhere
## and as I note in the code implementing it its dangerous and I'm
## not convinced we want it. Remove this test once we learn our lesson
## and remove the list method
mylst <- list("hi",
              c("there\nyou", "person", "ahoy"))
expect_equal(nlines(mylst), 5)
expect_equal(nlines(list()),
             0)



## testing mf_* roundtrip


dfmf <- basic_matrix_form(mtcars)

dfmf_wrong <- dfmf

mf_nrheader(dfmf_wrong) <- 2
expect_equal(mf_nrheader(dfmf_wrong), 2)
mf_nrheader(dfmf_wrong) <- 1
mf_rfnotes(dfmf_wrong) <- c("silly", "stuff")
expect_identical(mf_rfnotes(dfmf_wrong), c("silly", "stuff"))

## test indent to big breakage
## note this is a *very* artificial example and I'm not sure how much
## value it has beyodn increasing coverage
cwths <- propose_column_widths(dfmf_wrong)
cwths[1] <- 10
dfmf_wrong$indent_size <- 15
mf_rinfo(dfmf_wrong)$indent <- 1L
expect_error(toString(dfmf_wrong, widths = cwths))
## annoying direct constructor calls to ensure full coverage

strs <- matrix(byrow = TRUE, ncol = 2,
               c("lab1", "lab2",
                 "spn_val", "spn_val")
               )

spans <- matrix(byrow = TRUE, ncol = 2,
                c(1, 1,
                  2, 2)
                )

aligns <- matrix(byrow = TRUE, ncol = 2,
                 "center")
fmts <- matrix(byrow = TRUE, ncol = 2,
                 "xx")

rinfo <- formatters:::pagdfrow(nm = "row", lab = "row", rnum = 1,
                                   pth = "row", extent = 1, rclass = "silly")

mpf <- MatrixPrintForm(strings = strs, spans = spans, aligns = aligns,
                       formats = fmts, row_info = rinfo,
                       nlines_header = 1, nrow_header = 1, has_topleft = FALSE)

expect_equal(length(grep("spn_val", toString(mpf))),
             1L)

### Decimal Alignment Testing ================
test_that("error when widths are < than decimal aligned values", {
  df_error <- basic_matrix_form(mtcars)
  df_error$aligns[, -c(1)] <- "dec_left"

  expect_error(
    toString(df_error, widths = c(25, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)),
    paste0(
      "Inserted width\\(s\\) for column\\(s\\) disp \\(-1\\), wt \\(-1\\), ",
      "qsec \\(-1\\) is\\(are\\) not wide enough for the desired alignment."
    )
  )
})
test_that("padstr works with dec_left", {
  bmf <- basic_matrix_form(mtcars[1:4, c(1, 6)])
  bmf$aligns[, -c(1)] <- "dec_left"
  cw <- propose_column_widths(bmf) + 5

  result <- strsplit(toString(bmf, widths = cw, hsep = "-"), "\\n")[[1]]
  result_no_cw <- strsplit(toString(bmf, hsep = "-"), "\\n")[[1]]

  nc_res <- sapply(result, nchar, USE.NAMES = FALSE)
  nc_res_ncw <- sapply(result_no_cw, nchar, USE.NAMES = FALSE)

  expect_equal(nc_res - 15, nc_res_ncw)

  expected <- c(
    "                 mpg    wt   ",
    "-----------------------------",
    "Mazda RX4        21     2.62 ",
    "Mazda RX4 Wag    21     2.875",
    "Datsun 710       22.8   2.32 ",
    "Hornet 4 Drive   21.4   3.215"
  )
  expect_identical(result_no_cw, expected)
})

test_that("padstr works with dec_right", {
  bmf <- basic_matrix_form(mtcars[1:4, c(1, 6)])
  bmf$aligns[, -c(1)] <- "dec_right"
  result <- strsplit(toString(bmf, hsep = "-"), "\\n")[[1]]
  expected <- c(
    "                  mpg      wt",
    "-----------------------------",
    "Mazda RX4        21     2.62 ",
    "Mazda RX4 Wag    21     2.875",
    "Datsun 710       22.8   2.32 ",
    "Hornet 4 Drive   21.4   3.215"
  )
  expect_identical(result, expected)
})

test_that("padstr works with decimal", {
  bmf <- basic_matrix_form(mtcars[1:4, c(1, 6)])
  bmf$aligns[, -c(1)] <- "decimal"
  result <- strsplit(toString(bmf, hsep = "-"), "\\n")[[1]]
  expected <- c(
    "                 mpg     wt  ",
    "-----------------------------",
    "Mazda RX4        21     2.62 ",
    "Mazda RX4 Wag    21     2.875",
    "Datsun 710       22.8   2.32 ",
    "Hornet 4 Drive   21.4   3.215"
  )
  expect_identical(result, expected)
})

test_that("Decimal alignment: a specific case with larger widths", {
  hard_c <- c(12345.6, 0.235678, 6.7, 9.26, 1, 11)
  lhc <- length(hard_c)
  bmf <- basic_matrix_form(mtcars[1:lhc, c(1, 6)])
  cw0 <- propose_column_widths(bmf)
  bmf$strings[2:c(lhc + 1), 2] <- as.character(hard_c)
  bmf$strings[2:c(lhc + 1), 3] <- paste0(hard_c, "%")
  bmf$formats[2:c(lhc + 1), 3] <- rep("xx%", lhc)

  # decimal
  bmf$aligns[, -1] <- "decimal"
  cw <- cw_err <- propose_column_widths(bmf)
  expect_equal(sum(cw - cw0), 16) # small check of increased colwidths
  cw_err[c(2, 3)] <- cw[c(2, 3)] - 6
  cw[c(2, 3)] <- cw[c(2, 3)] + 6

  er_msg <- paste0(
    "Inserted width\\(s\\) for column\\(s\\) mpg \\(-6\\), wt \\(-6\\) ",
    "is\\(are\\) not wide enough for the desired alignment."
  )
  expect_error(toString(bmf, widths = cw_err), er_msg)

  res_dec <- strsplit(toString(bmf, widths = cw, hsep = "-"), "\\n")[[1]]

  expected <- c(
    "                           mpg                   wt         ",
    "------------------------------------------------------------",
    "Mazda RX4              12345.6              12345.6%        ",
    "Mazda RX4 Wag              0.235678             0.235678%   ",
    "Datsun 710                 6.7                  6.7%        ",
    "Hornet 4 Drive             9.26                 9.26%       ",
    "Hornet Sportabout          1                    1%          ",
    "Valiant                   11                   11%          "
  )
  expect_identical(res_dec, expected)

  # dec_right
  bmf$aligns[, -1] <- "dec_right"
  res_decr <- strsplit(toString(bmf, widths = cw, hsep = "-"), "\\n")[[1]]

  expected <- c(
    "                                   mpg                    wt",
    "------------------------------------------------------------",
    "Mazda RX4                 12345.6              12345.6%     ",
    "Mazda RX4 Wag                 0.235678             0.235678%",
    "Datsun 710                    6.7                  6.7%     ",
    "Hornet 4 Drive                9.26                 9.26%    ",
    "Hornet Sportabout             1                    1%       ",
    "Valiant                      11                   11%       "
  )
  expect_identical(res_decr, expected)

  # dec_left
  bmf$aligns[, -1] <- "dec_left"
  res_decl <- strsplit(toString(bmf, widths = cw, hsep = "-"), "\\n")[[1]]

  expected <- c(
    "                    mpg                  wt                 ",
    "------------------------------------------------------------",
    "Mazda RX4           12345.6              12345.6%           ",
    "Mazda RX4 Wag           0.235678             0.235678%      ",
    "Datsun 710              6.7                  6.7%           ",
    "Hornet 4 Drive          9.26                 9.26%          ",
    "Hornet Sportabout       1                    1%             ",
    "Valiant                11                   11%             "
  )
  expect_identical(res_decl, expected)

  # decimal mix
  bmf$aligns[-1, -1] <- rep(c("dec_left", "dec_right", "decimal"), each = 2)
  bmf$strings[-1, 1] <- bmf$aligns[-1, 2]

  res_decl <- strsplit(toString(bmf, widths = cw, hsep = "-"), "\\n")[[1]]

  expected <- c(
    "                    mpg                  wt                 ",
    "------------------------------------------------------------",
    "dec_left            12345.6              12345.6%           ",
    "dec_left                0.235678             0.235678%      ",
    "dec_right                     6.7                  6.7%     ",
    "dec_right                     9.26                 9.26%    ",
    "decimal                    1                    1%          ",
    "decimal                   11                   11%          "
  )
  expect_identical(res_decl, expected)
})

test_that("All supported 1d format cases of decimal alignment", {
  hard_c_formats <- list_valid_format_labels()$`1d`
  hard_c <- sapply(hard_c_formats, gsub, pattern = "x", replacement = "1")
  lhc <- length(hard_c)
  reduced_df <- mtcars[seq_len(lhc), c(1, 6, 7)]
  rownames(reduced_df) <- letters[seq_len(lhc)]
  bmf <- basic_matrix_form(reduced_df)
  cw <- propose_column_widths(bmf)
  bmf$strings[2:c(lhc + 1), seq(2, 3)] <- as.character(hard_c)
  bmf$formats[2:c(lhc + 1), seq(2, 3)] <- hard_c

  # decimal
  set.seed(1)
  bmf$aligns[, 2] <- "decimal"
  sample_list_aligns <- sample(list_valid_aligns(),
                               size = nrow(bmf$aligns),
                               replace = TRUE
  )
  bmf$strings[, 4] <- bmf$aligns[, 3] <- sample_list_aligns

  expect_error(cw <- propose_column_widths(bmf), regexp = "*1.1111 | (<0.0001)*")
  bmf$aligns[nrow(bmf$aligns), c(2, 3)] <- "center"
  cw <- propose_column_widths(bmf)
  cw[3] <- cw[3] + 4
  res_dec <- strsplit(toString(bmf, widths = cw, hsep = "-"), "\\n")[[1]]

  expected <- c(
    "           mpg           wt                         left   ",
    "-----------------------------------------------------------",
    "a          11                     11              decimal  ",
    "b          11.           11.                      left     ",
    "c          11.1                            11.1   right    ",
    "d          11.11                        11.11     dec_right",
    "e          11.111                11.111           center   ",
    "f          11.1111          11.1111               dec_left ",
    "g          11%                              11%   right    ",
    "h          11.%                   11.%            center   ",
    "i          11.1%                 11.1%            center   ",
    "j          11.11%        11.11%                   left     ",
    "k          11.111%                      11.111%   dec_right",
    "l       (N=11)                       (N=11)       dec_right",
    "m        >999.9                          >999.9   right    ",
    "n        >999.99          >999.99                 dec_left ",
    "o   1.1111 | (<0.0001)     1.1111 | (<0.0001)     dec_left "
  )
  expect_identical(res_dec, expected)
})

test_that("All 2d cases for decimal alignment", {
  formats2d <- list_valid_format_labels()$`2d`
  hard_c <- sapply(formats2d, gsub, pattern = "x", replacement = "1")
  reduced_df <- mtcars[seq_along(hard_c), c(1, 6)]
  rownames(reduced_df) <- formats2d
  colnames(reduced_df) <- c("dec_left", "decimal")
  bmf <- basic_matrix_form(reduced_df)
  bmf$strings[seq(2, length(hard_c) + 1), seq(2, 3)] <- as.character(hard_c)
  bmf$formats[seq(2, length(hard_c) + 1), seq(2, 3)] <- hard_c

  bmf$aligns[, 2] <- "decimal"
  bmf$aligns[, 3] <- "dec_right"
  bmf$col_widths <- NULL
  expect_error(res_dec <- strsplit(toString(bmf, hsep = "-"), "\\n")[[1]],
               regexp = "*first 3 selected from column dec_left*")

  # expected <- c(
  #   "                          dec_left                     decimal",
  #   "--------------------------------------------------------------",
  #   "xx / xx                11 / 11               11 / 11          ",
  #   "xx. / xx.              11. / 11.             11. / 11.        ",
  #   "xx.x / xx.x            11.1 / 11.1           11.1 / 11.1      ",
  #   "xx.xx / xx.xx          11.11 / 11.11         11.11 / 11.11    ",
  #   "xx.xxx / xx.xxx        11.111 / 11.111       11.111 / 11.111  ",
  #   "N=xx (xx%)           N=11 (11%)            N=11 (11%)         ",
  #   "xx (xx%)               11 (11%)              11 (11%)         ",
  #   "xx (xx.%)              11 (11.%)             11 (11.%)        ",
  #   "xx (xx.x%)             11 (11.1%)            11 (11.1%)       ",
  #   "xx (xx.xx%)            11 (11.11%)           11 (11.11%)      ",
  #   "xx. (xx.%)             11. (11.%)            11. (11.%)       ",
  #   "xx.x (xx.x%)           11.1 (11.1%)          11.1 (11.1%)     ",
  #   "xx.xx (xx.xx%)         11.11 (11.11%)        11.11 (11.11%)   ",
  #   "(xx, xx)              (11, 11)              (11, 11)          ",
  #   "(xx., xx.)            (11., 11.)            (11., 11.)        ",
  #   "(xx.x, xx.x)          (11.1, 11.1)          (11.1, 11.1)      ",
  #   "(xx.xx, xx.xx)        (11.11, 11.11)        (11.11, 11.11)    ",
  #   "(xx.xxx, xx.xxx)      (11.111, 11.111)      (11.111, 11.111)  ",
  #   "(xx.xxxx, xx.xxxx)    (11.1111, 11.1111)    (11.1111, 11.1111)",
  #   "xx - xx                11 - 11               11 - 11          ",
  #   "xx.x - xx.x            11.1 - 11.1           11.1 - 11.1      ",
  #   "xx.xx - xx.xx          11.11 - 11.11         11.11 - 11.11    ",
  #   "xx (xx)                11 (11)               11 (11)          ",
  #   "xx. (xx.)              11. (11.)             11. (11.)        ",
  #   "xx.x (xx.x)            11.1 (11.1)           11.1 (11.1)      ",
  #   "xx.xx (xx.xx)          11.11 (11.11)         11.11 (11.11)    ",
  #   "xx (xx.)               11 (11.)              11 (11.)         ",
  #   "xx (xx.x)              11 (11.1)             11 (11.1)        ",
  #   "xx (xx.xx)             11 (11.11)            11 (11.11)       ",
  #   "xx.x, xx.x             11.1, 11.1            11.1, 11.1       ",
  #   "xx.x to xx.x           11.1 to 11.1          11.1 to 11.1     "
  # )
  #
  # expect_identical(res_dec, expected)
})

test_that("fmt_config works as expected", {
  x <- fmt_config()
  expect_identical(obj_format(x), NULL)
  expect_identical(obj_na_str(x), "NA")
  expect_identical(obj_align(x), "center")

  x <- fmt_config(format = "xx.xx", na_str = "<Missing>", align = "right")
  expect_identical(obj_format(x), "xx.xx")
  expect_identical(obj_na_str(x), "<Missing>")
  expect_identical(obj_align(x), "right")

  # Test setters
  expect_silent(obj_format(x) <- function() {})
  expect_silent(obj_na_str(x) <- "something wrong")
  expect_silent(obj_align(x) <- "something wrong")
})
