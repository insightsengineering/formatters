


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
expect_error(pag_indices_inner(mf_rinfo(dfmf_b),
                               8,
                               min_siblings = 0,
                               nosplitin = "root_split",
                               verbose = TRUE))
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
expect_error(pag_indices_inner(mf_rinfo(dfmf_cont),
                               8,
                               min_siblings = 0,
                               verbose = TRUE))

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
  mf_rinfo(df2mf)[, -which(names(mf_rinfo(df2mf)) == "ref_info_df")]
)

hpaginds <- pag_indices_inner(mf_rinfo(df2mf),
  8,
  min_siblings = 0,
  verbose = TRUE
)

expect_true(all(lengths(hpaginds) == 2L))




vpaginds <- vert_pag_indices(df2mf, cpp = 40, verbose = TRUE)

expect_identical(
  lengths(vpaginds),
  c(3L, 3L, 3L, 2L, 1L)
)

vpaginds2 <- vert_pag_indices(df2mf, cpp = 39, verbose = TRUE)

expect_identical(
  lengths(vpaginds2),
  c(2L, 2L, 2L, 3L, 2L, 1L)
)

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

## XXX this test is not longer relevant and needs to be rewritten
## the hack of manually altering nreflines is no longer relevant
## This does beg the question of why self_extent was including nreflines
## we probably need to change that too!!!

## df4 <- mtcars
## df4mf <- basic_matrix_form(df4)
## rinfo4 <- mf_rinfo(df4mf)
## ## self_extent is expected to include nreflines!!!!
## rinfo4$nreflines <- 1
## rinfo4$self_extent <- rinfo4$self_extent + 1
## mf_rinfo(df4mf) <- rinfo4
## pags4inds <- pag_indices_inner(mf_rinfo(df4mf),
##   8,
##   min_siblings = 0,
##   verbose = TRUE
## )

## ## should be 3 because each row now takes 1 (the row) + 1(the footnote) lines
## ## plus 2 lines for the divider + empty line for having referential footnotes at all

## ## NB that +2 will change if we change the behavior re dividers and referential footnotes!
## expect_equal(length(pags4inds[[1]]),
##              3)
## expect_equal(length(tail(pags4inds, 1)[[1]]),
##             2)
