## listing supported formats and enuring they all read as valid
forms <- list_valid_format_labels()

res <- sapply(forms, function(vc) all(sapply(vc, is_valid_format)))
expect_true(all(res))

## core formatter tests for format strings
values <- c(5.123456, 7.891112)
expect_identical(format_value(values[1], format = "xx"),
                 paste(values[1]))

expect_identical(format_value(values[1], format = "xx."),
                 "5")

expect_identical(format_value(values[1], format = "xx.x"),
                 "5.1")

expect_identical(format_value(values[1], format = "xx.xx"),
                 "5.12")

expect_identical(format_value(values[1], format = "xx.xxx"),
                 "5.123")

expect_identical(format_value(values[1], format = "xx.xxxx"),
                 "5.1235")

expect_identical(format_value(values[1], format = "xx%"),
                 paste0(values[1]*100, "%"))

expect_identical(format_value(values[1], format = "xx.%"),
                 "512%")

expect_identical(format_value(values[1], format = "xx.x%"),
                 "512.3%")

expect_identical(format_value(values[1], format = "xx.xx%"),
                 "512.35%")

expect_identical(format_value(values[1], format = "xx.xxx%"),
                 "512.346%")

expect_identical(format_value(values[1], format = ">999.9"),
                 "5.1")

expect_identical(format_value(10000, format = ">999.9"),
                 ">999.9")

expect_identical(format_value(values[1], format = ">999.99"),
                 "5.12")

expect_identical(format_value(10000, format = ">999.99"),
                 ">999.99")

expect_identical(format_value(.0004, format = "x.xxxx | (<0.0001)"),
                 "0.0004")

expect_identical(format_value(.00004, format = "x.xxxx | (<0.0001)"),
                 "<0.0001")

expect_identical(format_value(values, format = "xx / xx"),
                 paste(values, collapse  = " / "))

expect_identical(format_value(values, format = "xx. / xx."),
                 "5 / 8")

expect_identical(format_value(values, format = "xx.x / xx.x"),
                 "5.1 / 7.9")

expect_identical(format_value(values, format = "xx.xx / xx.xx"),
                 "5.12 / 7.89")

expect_identical(format_value(values, format = "xx.xxx / xx.xxx"),
                 "5.123 / 7.891")

expect_identical(format_value(values, format = "xx (xx%)"),
                 paste0(values[1], " (", values[2]*100, "%)"))

expect_identical(format_value(values, format = "xx (xx.%)"),
                 paste0(values[1], " (789%)"))

expect_identical(format_value(values, format = "xx. (xx.%)"),
                 paste0(5, " (789%)"))

expect_identical(format_value(values, format = "xx (xx.x%)"),
                 paste0(values[1], " (789.1%)"))

expect_identical(format_value(values, format = "xx (xx.xx%)"),
                 paste0(values[1], " (789.11%)"))

expect_identical(format_value(values, format = "xx.x (xx.x%)"),
                 "5.1 (789.1%)")

expect_identical(format_value(values, format = "xx.xx (xx.xx%)"),
                 "5.12 (789.11%)")

expect_identical(format_value(values, format = "xx.x (xx.x%)"),
                 "5.1 (789.1%)")

expect_identical(format_value(values, format = "(xx, xx)"),
                 paste0("(", values[1], ", ", values[2], ")"))

expect_identical(format_value(values, format = "(xx., xx.)"),
                 "(5, 8)")

expect_identical(format_value(values, format = "(xx.x, xx.x)"),
                 "(5.1, 7.9)")

expect_identical(format_value(values, format = "(xx.xx, xx.xx)"),
                 "(5.12, 7.89)")

expect_identical(format_value(values, format = "(xx.xxx, xx.xxx)"),
                 "(5.123, 7.891)")

expect_identical(format_value(values, format = "(xx.xxxx, xx.xxxx)"),
                 "(5.1235, 7.8911)")

expect_identical(format_value(values, format = "xx - xx"),
                 paste(values, collapse  = " - "))

expect_identical(format_value(values, format = "xx.x - xx.x"),
                 "5.1 - 7.9")

expect_identical(format_value(values, format = "xx.xx - xx.xx"),
                 "5.12 - 7.89")

expect_identical(format_value(values, format = "xx (xx%)"),
                 paste0(values[1], " (", values[2]*100, "%)"))

expect_identical(format_value(values, format = "xx (xx)"),
                 paste0(values[1], " (", values[2], ")"))


expect_identical(format_value(values, format = "xx (xx.)"),
                 paste0(values[1], " (8)"))


expect_identical(format_value(values, format = "xx (xx.x)"),
                 paste0(values[1], " (7.9)"))

expect_identical(format_value(values, format = "xx (xx.xx)"),
                 paste0(values[1], " (7.89)"))


expect_identical(format_value(values, format = "xx. (xx.)"),
                 paste0(5, " (8)"))


expect_identical(format_value(values, format = "xx.x (xx.x)"),
                 "5.1 (7.9)")

expect_identical(format_value(values, format = "xx.xx (xx.xx)"),
                 "5.12 (7.89)")

expect_identical(format_value(values, format = "xx.x, xx.x"),
                 "5.1, 7.9")

expect_identical(format_value(values, format = "xx.x to xx.x"),
                 "5.1 to 7.9")

expect_identical(format_value(c(values, 10.1235), format = "xx. (xx. - xx.)"),
                 "5 (8 - 10)")

expect_identical(format_value(c(values, 10.1235), format = "xx.x (xx.x - xx.x)"),
                 "5.1 (7.9 - 10.1)")

expect_identical(format_value(c(values, 10.1235), format = "xx.xx (xx.xx - xx.xx)"),
                 "5.12 (7.89 - 10.12)")

expect_identical(format_value(c(values, 10.1235), format = "xx.xxx (xx.xxx - xx.xxx)"),
                 "5.123 (7.891 - 10.124)")


expect_identical(format_value(NULL, "xx"), "")


expect_identical(format_value(5.123, "xx.x", output = "html"),
                 list("5.1" = htmltools::tagList(format_value(5.123, "xx.x"), NULL)))


## errors

expect_error(format_value(5.1, "abcd"), "unknown format label")
expect_error(format_value(5.1, "xx - xx"), "are of different length")

expect_error(format_value(c(5.1, 2, 3), "xx - xx"), "are of different length")
## handling NAs

results <- vapply(forms[["1d"]], function(fmt) format_value(NA, format = fmt), "")
justnastr <- results == "NA"

expect_identical(names(results)[!justnastr],
                 c("xx%", "xx.%", "xx.x%", "xx.xx%", "xx.xxx%", "(N=xx)"))

expect_identical(format_value(NA, "xx.", na_str = "-"),
                 "-")
expect_identical(format_value(NA, "xx", na_str = "-"),
                 "-")

## trailing 0s are correct
expect_identical(format_value(0, "xx."), "0")
expect_identical(format_value(0, "xx.x"), "0.0")
expect_identical(format_value(0, "xx.xx"), "0.00")
expect_identical(format_value(0, "xx.xxx"), "0.000")
expect_identical(format_value(0, "xx.xxxx"), "0.0000")


## sprintf_format functionality
myfun <- sprintf_format("hi there %1.4f")
expect_true(is_valid_format(myfun))
expect_identical(format_value(pi, format = myfun),
                     "hi there 3.1416")




## https://github.com/insightsengineering/formatters/issues/18
dfmf <- basic_matrix_form(mtcars)
expect_identical(main_footer(dfmf), "")
ftmsg <- "my footer is here"
main_footer(dfmf) <- ftmsg
expect_identical(main_footer(dfmf),
                 ftmsg)


## https://github.com/Roche/rtables/issues/318
dfmf2 <- dfmf
dfmf2$strings[1, 2] <- "m\npg"
dfmf2$strings[1, 1] <- "tleft mats"

dfmf2 <- formatters:::mform_handle_newlines(dfmf2, has_topleft = TRUE)
expect_identical(dfmf2$strings[1:2, 1:2],
                 matrix(c("tleft mats", "", "m", "pg"), nrow = 2, ncol = 2))





strout <- toString(dfmf)
expect_true(any(grepl(ftmsg, strout)))

df2 <- mtcars
df2$extra <- paste("row", 1:NROW(df2), "haha", sep = "\n")

df2mf <- basic_matrix_form(df2)

mf_rinfo <- formatters:::mf_rinfo

expect_identical(basic_pagdf(row.names(df2),
                             extents = 3L),
                 mf_rinfo(df2mf))

hpaginds <- pag_indices_inner(mf_rinfo(df2mf),
                              8, min_siblings = 0)

expect_true(all(lengths(hpaginds) == 2L))

vpaginds <- vert_pag_indices(df2mf, cpp = 40)

expect_identical(lengths(vpaginds),
                 c(3L, 3L, 3L, 2L, 1L))

vpaginds2 <- vert_pag_indices(df2mf, cpp = 39, verbose = TRUE)

expect_identical(lengths(vpaginds2),
                 c(2L, 2L, 2L, 3L, 2L, 1L))


df3 <- data.frame(x = 1:5, y = c(1:3, 8, 9),
                 row.names = c("spna", "spnb", "spnc", "sep1", "sep2"))

df3mf <- basic_matrix_form(df3)

spnmat <- formatters:::mf_spans(df3mf)

spnmat[2:3, 2:3] <- 2

df3mf$spans <- spnmat
df3mf$display[2:3, 3] <- FALSE
df3mf$aligns[2:6, 2:3] <- "center"
strout <- toString(df3mf)
expect_false(grepl("1[[:space:]]*1", strout))
expect_true(grepl("3[[:space:]]*3", strout))


expect_identical(spread_integer(7, 3),
                 c(3, 2, 2))

expect_error(spread_integer(3.5, 2))


## matrix_form on a matrix form a no op
expect_identical(df3mf, matrix_form(df3mf))
expect_identical(divider_height(df3mf), 1L)
expect_identical(subtitles(df3mf), character())
expect_identical(page_titles(df3mf), character())
prov_footer(df3mf) <- "file: myfile.txt"
expect_identical(prov_footer(df3mf), "file: myfile.txt")
expect_identical(nlines(NULL), 0L)
expect_identical(nlines("hi\nthere"), 2L)


thing <- 5.1234
expect_true(is.null(obj_label(thing)))
obj_label(thing) <- "hi thing"
expect_identical(obj_label(thing), "hi thing")
expect_true(is.null(obj_format(thing)))
obj_format(thing) <- "xx.x"
expect_identical(format_value(thing, obj_format(thing)),
                 "5.1")


## labels

x <- 15
expect_identical(obj_label(with_label(x, "hi")),
                 "hi")

mydf <- mtcars
lbls <- paste("LBL: ", names(mydf))
var_labels(mydf) <- lbls
expect_identical(var_labels(mydf),
                 setNames(lbls,
                          names(mydf)))

mydf <- var_relabel(mydf, mpg = "New MPG")
expect_identical(var_labels(mydf),
                 c(mpg = "New MPG", setNames(lbls[-1],
                                             names(mydf)[-1])))


expect_true(all(is.na(var_labels(var_labels_remove(mydf)))))


## additional full smoke test of labels without output checking

values2 <- c(values, 987)
labs <- list_valid_format_labels()

r1 <- vapply(labs[["1d"]],
             function(lb) {
    tmp <- format_value(values2[1], lb)
    TRUE
}, NA)

expect_true(all(r1))

r2 <- vapply(labs[["2d"]],
             function(lb) {
    tmp <- format_value(values2[1:2], lb)
    TRUE
}, NA)

expect_true(all(r2))


r3 <- vapply(labs[["3d"]],
             function(lb) {
    tmp <- format_value(values2, lb)
    TRUE
}, NA)

expect_true(all(r3))

expect_identical(var_labels(data.frame()),
                 character())
