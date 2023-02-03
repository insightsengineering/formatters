context("Exporters")

export_fact <- function() {
  tbl2 <- NULL
  function() {
    if(is.null(tbl2)) {
      lyt <- rtables::basic_table() %>%
        rtables::split_cols_by("ARM") %>%
        rtables::split_cols_by("SEX", split_fun = rtables::keep_split_levels(c("M", "F"))) %>%
        rtables::split_rows_by("STRATA1") %>%
        rtables::summarize_row_groups() %>%
        rtables::split_rows_by("RACE", split_fun = rtables::keep_split_levels(c("WHITE", "ASIAN"))) %>%
        rtables::analyze(c("AGE", "BMRKR2", "COUNTRY"))

      tbl2 <<- rtables::build_table(lyt, ex_adsl)
    }
    tbl2
  }
}

tt_to_export <- export_fact()

# Creating data-set with wide content to test wrapping
tt_to_test_wrapping <- function() {
    trimmed_data <- ex_adsl %>%
        dplyr::filter(SEX %in% c("M", "F")) %>%
        dplyr::filter(RACE %in% levels(RACE)[1:2])

    levels(trimmed_data$ARM)[1] <- "Incredibly long column name to be wrapped"
    levels(trimmed_data$ARM)[2] <- "This_should_be_somewhere_split"

    rtables::basic_table(title = "Enough long title to be probably wider than expected",
              main_footer = "Also this seems quite wider than expected initially.") %>%
      rtables::split_cols_by("ARM") %>%
      rtables::split_rows_by("RACE", split_fun = rtables::drop_split_levels) %>%
      rtables::analyze(c("AGE", "EOSDY"),
              na_str = "A very long content to_be_wrapped_and_splitted",
              inclNAs = TRUE) %>%
      rtables::build_table(trimmed_data)
}

tt_for_wrap <- tt_to_test_wrapping()

test_that("export_as_txt works with and without pagination", {
  lyt <- rtables::basic_table() %>%
    rtables::split_cols_by("ARM") %>%
    rtables::analyze(c("AGE", "BMRKR2", "COUNTRY"))

  tbl <- rtables::build_table(lyt, ex_adsl)

  tmptxtf <- tempfile()
  export_as_txt(tbl, file = tmptxtf, paginate = TRUE, lpp = 8, verbose = TRUE)
  txtlns <- readLines(tmptxtf)
  expect_identical(grep("\\\\s\\\\n", txtlns),
                   c(9L, 17L))

  expect_identical(toString(tbl),
                   export_as_txt(tbl, file = NULL, paginate = FALSE))
})

test_that("export_as_txt works with wrapping", {
  clw <- c(5, 7, 6, 6) + 12
  lpp_tmp <- 18

  ## no vert pagination because lpp is so big
  tmptxtf1 <- tempfile()
  export_as_txt(tt_for_wrap,
                file = tmptxtf1,
                paginate = TRUE,
                lpp = 150,
                colwidths = clw,
                tf_wrap = TRUE,
                max_width = 20, cpp = 80, verbose = TRUE)
  txtlns1 <- readLines(tmptxtf1)
  pagepos1 <- grep("\\\\s\\\\n", txtlns1)
  expect_identical(pagepos1, 30L) ##c(30L, 58L))

  ## explicitly no vertical pagination (lpp = NULL)
  tmptxtf1b <- tempfile()
  export_as_txt(tt_for_wrap,
                file = tmptxtf1b,
                paginate = TRUE,
                lpp = NULL,
                colwidths = clw,
                tf_wrap = TRUE,
                max_width = 20, cpp = 80)

  txtlns1b <- readLines(tmptxtf1b)
  expect_identical(txtlns1, txtlns1b)

  ## no horiz pagination, tf_wrap FALSE
  tmptxtf2 <- tempfile()
  expect_warning(export_as_txt(tt_for_wrap,
                               file = tmptxtf2,
                               paginate = TRUE,
                               lpp = lpp_tmp,
                               colwidths = clw,
                               tf_wrap = FALSE,
                               max_width = 20, verbose = TRUE))
  txtlns2 <- readLines(tmptxtf2)
  pagepos2 <- grep("\\\\s\\\\n", txtlns2)
  expect_identical(pagepos2, 18L) ##c(26L, 50L))

  tmptxtf2b <- tempfile()
  expect_error(export_as_txt(tt_for_wrap,
                             file = tmptxtf2b,
                             paginate = TRUE,
                             lpp = lpp_tmp,
                             colwidths = clw,
                             tf_wrap = TRUE,
                             max_width = 20, verbose = TRUE))
  export_as_txt(tt_for_wrap,
                file = tmptxtf2b,
                paginate = TRUE,
                lpp = lpp_tmp,
                colwidths = clw,
                tf_wrap = TRUE,
                max_width = 40, verbose = TRUE)
  txtlns2b <- readLines(tmptxtf2b)
  pagepos2b <- grep("\\\\s\\\\n", txtlns2b)
  expect_identical(pagepos2b, c(16L, 33L, 49L)) ## 16 because we dont' get our first pick of pagination spots anymore

  ## both vertical and horizontal pagination #458
  tmptxtf3 <- tempfile()
  ## this fails, no valid pagination after both heade rand footer
  ## are wrapped to 20
  expect_error(export_as_txt(tt_for_wrap,
                             file = tmptxtf3,
                             paginate = TRUE,
                             lpp = lpp_tmp,
                             colwidths = clw,
                             tf_wrap = TRUE,
                             max_width = 20,
                             cpp = 80))
  export_as_txt(tt_for_wrap,
                file = tmptxtf3,
                paginate = TRUE,
                lpp = lpp_tmp,
                colwidths = clw,
                tf_wrap = TRUE,
                max_width = 40,
                cpp = 80, verbose = TRUE)

  txtlns3 <- readLines(tmptxtf3)
  pagepos3 <- grep("\\\\s\\\\n", txtlns3)
  expect_identical(pagepos3[1], pagepos2b[1])
})

test_that("export_as_pdf works", {
  tbl <- tt_to_export()
  tmpf <- tempfile(fileext = ".pdf")

  expect_warning(export_as_pdf(tbl, file = tmpf, landscape = TRUE, width = 3, paginate = FALSE),
                 "width of page 1 exceeds the available space")
  expect_true(file.exists(tmpf))
  expect_warning(export_as_pdf(tbl, file = tmpf, height = 3, paginate = FALSE),
                 "height of page 1 exceeds the available space")

  res <- export_as_pdf(tbl, file = tmpf)

  expect_equal(res$npages, 3)
})

# test_that("exporting pdfs gives the correct values", {
#     if (check_pdf) {
#         lyt <- rtables::basic_table(title = " ") %>%
#             rtables::split_rows_by("SEX", page_by = TRUE) %>%
#             rtables::analyze("AGE")
#
#         # Building the table
#         tbl <- rtables::build_table(lyt, DM)
#
#         tmpf <- tempfile(fileext = ".pdf")
#         res <- export_as_pdf(tbl, file = tmpf, hsep = "=", lpp = 20)
#         res_pdf <- pdf_text(tmpf)
#
#         # Removing spaces and replacing separators
#         res_pdf <- gsub(res_pdf, pattern = "==*", replacement = "+++")
#         res_pdf <- gsub(res_pdf, pattern = "  +", replacement = " ")
#         res_pdf <- gsub(res_pdf, pattern = " \n", replacement = "")
#
#         # Pagination is present as vector in pdf_text. Doing the same with tbl
#         expected <- sapply(rtables::paginate_table(tbl), function(x) toString(x, hsep = "="), USE.NAMES = FALSE)
#         names(expected) <- NULL
#
#         # Removing spaces and replacing separators
#         expected <- gsub(expected, pattern = "==*", replacement = "+++")
#         expected <- gsub(expected, pattern = "  +", replacement = " ")
#         expected <- gsub(expected, pattern = " \n", replacement = "\n")
#         expected <- gsub(expected, pattern = "^\n", replacement = "")
#         expect_identical(res_pdf, expected)
#         ## TODO understand better how to compare exactly these outputs
#     }
# })

test_that("exporting pdf does the inset", {
  tbl <- tt_to_export()
  table_inset(tbl) <- 100
  tmpf <- tempfile(fileext = ".pdf")

  expect_error(export_as_pdf(tbl, file = tmpf))
})

test_that("flextable export works", {
  analysisfun <- function(x, ...) {
    rtables::in_rows(row1 = 5,
            row2 = c(1, 2),
            .row_footnotes = list(row1 = "row 1 - row footnote"),
            .cell_footnotes = list(row2 = "row 2 - cell footnote"))
  }

  lyt <- rtables::basic_table() %>%
    rtables::split_cols_by("ARM") %>%
    rtables::split_cols_by("SEX", split_fun = rtables::keep_split_levels(c("M", "F"))) %>%
    rtables::split_rows_by("STRATA1") %>%
    rtables::summarize_row_groups() %>%
    rtables::split_rows_by("RACE", split_fun = rtables::keep_split_levels(c("WHITE", "ASIAN"))) %>%
    rtables::analyze("AGE", afun = analysisfun)

  tbl <-  rtables::build_table(lyt, ex_adsl)
  ft <- export_as_flextable(tbl, total_width = 20)
  expect_equal(sum(unlist(nrow(ft))), 20)
  ft

  ft2 <- export_as_flextable(tbl, paginate = TRUE, lpp = 20)
  expect_equal(length(ft2), 6)
})
