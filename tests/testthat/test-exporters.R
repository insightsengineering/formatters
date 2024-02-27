test_that("exporters work", {
  ## XXX rework to use make_basemf_fnotes() (indices aren't identical, may change test outcomes)
  dfmf <- basic_matrix_form(mtcars)
  main_title(dfmf) <- "main title"
  main_footer(dfmf) <- "main footer"
  prov_footer(dfmf) <- "prov footer"
  subtitles(dfmf) <- c("sub", "titles")

  dfmf$strings[2, 2] <- paste(dfmf$strings[2, 2, drop = TRUE], "{*}")
  dfmf$strings[3, 4] <- paste(dfmf$strings[3, 4, drop = TRUE], "{1}")
  dfmf$strings[6, 3] <- paste(dfmf$strings[6, 3, drop = TRUE], "{2}")
  dfmf$strings[10, 1:3] <- paste(dfmf$strings[10, 1:3], "{*}")
  dfmf$strings[11, 4:6] <- paste(dfmf$strings[11, 4:6], "{*}")

  mf_rfnotes(dfmf) <- c(
    "{1} - fnote 1 is the coolest",
    "{2} - no way, fnote 2 forever",
    "{*} - symbooollllssss"
  )

  dfmf <- mform_build_refdf(dfmf)
  mf_rfnotes(dfmf) <- reconstruct_basic_fnote_list(dfmf)
  mf_col_widths(dfmf) <- propose_column_widths(dfmf)
  ## covered below
  ## fil <- tempfile(fileext = ".rtf")


  ## myrtf <- mpf_to_rtf(dfmf)
  ## r2rtf::write_rtf(myrtf, fil)
  ## expect_true(file.exists(fil))


  fil2 <- tempfile(fileext = ".txt")

  export_as_txt(dfmf, file = fil2, page_break = "~~~~\n", verbose = TRUE)
  exptlines <- readLines(fil2)

  ## 2 pages, 1 page break
  pbreak_pos <- grep("~", exptlines, fixed = TRUE)
  expect_identical(length(pbreak_pos), 1L)

  ## one per page
  sym_msg_pos <- grep("{*} - symbooollllssss", exptlines, fixed = TRUE)

  expect_identical(length(grep("{*} - symbooollllssss", exptlines, fixed = TRUE)), 2L)
  expect_true(sym_msg_pos[1] < pbreak_pos && sym_msg_pos[2] > pbreak_pos)

  ## 4 on first page, incl the message, 2 on second page incl message
  expect_identical(length(grep("{*}", exptlines, fixed = TRUE)), 6L)



  exp_h_pags <- 3L
  exp_v_pags <- 2L
  fil3 <- tempfile(fileext = ".txt")
  export_as_txt(
    dfmf,
    pg_height = 4, pg_width = 4, file = fil3, page_break = "~~~~\n",
    margins = rep(0, 4)
  )
  exptlines <- readLines(fil3)

  msg_1_pos <- grep("{1} - fnote 1 is the coolest", exptlines, fixed = TRUE)
  expect_identical(length(msg_1_pos), 1L)

  msg_asterisk_pos <- grep("{*} - symbooollllssss", exptlines, fixed = TRUE)
  expect_identical(
    length(msg_asterisk_pos),
    exp_h_pags
  )

  expect_true(msg_1_pos < min(msg_asterisk_pos))


  expect_identical(
    length(grep("~", exptlines, fixed = TRUE)),
    exp_h_pags * exp_v_pags - 1L
  )


  ## export_as_rtf rudimentary coverage
  if (requireNamespace("r2rtf")) {
    fil4 <- tempfile(fileext = ".rtf")
    export_as_rtf(dfmf, file = fil4)
    expect_true(file.exists(fil4))
    if (file.exists("Rplots.pdf")) {
      file.remove("Rplots.pdf") # coming probably from rtf::
    }
  }


})

test_that("mpf_subset_rows works when there are newlines/wrapping in column labels", {
  ## https://github.com/insightsengineering/rtables/issues/634
  strs <- matrix(c(
    "hi", "lo",
    "", "there",
    "(N=50)", "(N=whoknows)",
    "value", "value",
    "value2", "value2"
  ), nrow = 5, byrow = TRUE)

  rinfo <- rbind(
    pagdfrow(
      nm = "what", lab = "what", rnum = 1, pth = list(), extent = 1L, nsibs = 1, sibpos = 1, rclass = "what"
    ),
    pagdfrow(
      nm = "what2", lab = "what2", rnum = 2, pth = list(), extent = 1L, nsibs = 1, sibpos = 1, rclass = "what"
    )
  )
  mymf <- MatrixPrintForm(
    strings = strs, aligns = matrix("center", ncol = 2, nrow = 5),
    formats = matrix("xx", ncol = 2, nrow = 5),
    spans = matrix(1L, ncol = 2, nrow = 5),
    has_topleft = FALSE,
    line_grouping = c(1, 1, 2, 3, 4),
    nrow_header = 2,
    row_info = rinfo
  )

  mymf_out <- toString(mymf, hsep = "-")
  expct_lns <- c(
    "              lo     ",
    "  hi        there    ",
    "(N=50)   (N=whoknows)",
    "---------------------",
    "value       value    ",
    "value2      value2   \n"
  )
  expect_identical(
    mymf_out,
    paste(expct_lns, collapse = "\n")
  )
  newmf <- mpf_subset_rows(mymf, 1)
  expect_identical(
    toString(newmf, hsep = "-"),
    paste(c(expct_lns[1:5], ""), collapse = "\n")
  )
})


test_that("export_as_txt maintains repeated columns when paginate is TRUE", {
  dfmf <- basic_matrix_form(mtcars)

  # repeat first 3 columns in each page
  pag_out <- export_as_txt(dfmf, cpp = 90, rep_cols = 3)

  # There should be two "disp", "cyl" and "mpg" columns
  expect_identical(length(gregexpr(c("mpg"), pag_out)[[1]]), 2L)
  expect_identical(length(gregexpr(c("cyl"), pag_out)[[1]]), 2L)
  expect_identical(length(gregexpr(c("disp"), pag_out)[[1]]), 2L)
})

test_that("export_as_txt maintains horizontal separator from table", {
  dfmf <- basic_matrix_form(mtcars)
  horizontal_sep(dfmf) <- "="
  # repeat first 3 columns in each page
  out <- strsplit(export_as_txt(dfmf), "\n")[[1]][2]
  expect_equal(out, paste0(rep("=", nchar(out)), collapse = ""))
})


test_that("export_as_pdf works", {
  # Enhancing coverage -> modified copy from rtables
  bmf <- basic_matrix_form(mtcars)
  tmpf <- tempfile(fileext = ".pdf")

  expect_warning(
    export_as_pdf(bmf, file = tmpf, landscape = TRUE, width = 3, paginate = FALSE),
    "width of page 1 exceeds the available space"
  )
  expect_true(file.exists(tmpf))
  file.remove(tmpf)
  expect_warning(
    export_as_pdf(bmf, file = tmpf, height = 3, paginate = FALSE),
    "height of page 1 exceeds the available space"
  )

  res <- export_as_pdf(bmf, file = tmpf, paginate = TRUE, cpp = 90)

  expect_equal(res$npages, 2)
})
