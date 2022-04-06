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

expect_identical(format_value(values, format = "xx.x (xx.x)"),
                 "5.1 (7.9)")

expect_identical(format_value(values, format = "xx.xx (xx.xx)"),
                 "5.12 (7.89)")

expect_identical(format_value(values, format = "xx.x, xx.x"),
                 "5.1, 7.9")

expect_identical(format_value(values, format = "xx.x to xx.x"),
                 "5.1 to 7.9")

expect_identical(format_value(c(values, 10.1235), format = "xx.xx (xx.xx - xx.xx)"),
                 "5.12 (7.89 - 10.12)")


## handling NAs

results <- vapply(forms[["1d"]], function(fmt) format_value(NA, format = fmt), "")
justnastr <- results == "NA"

expect_identical(names(results)[!justnastr],
                 c("xx%", "xx.x%", "xx.xx%", "xx.xxx%", "(N=xx)"))

expect_identical(format_value(NA, "xx.", na_str = "-"),
                 "-")

## trailing 0s are correct
expect_identical(format_value(0, "xx."), "0") ## XXX TODO is this right? what is xx. supposed to do????
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

strout <- toString(dfmf)
expect_true(any(grepl(ftmsg, strout)))


