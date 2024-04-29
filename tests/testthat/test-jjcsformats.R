values <- c(5.123456, 7.891112)

cdf <- c(5, 2000,5/2000)
cf <- c(5, 5/2000)

test_that("jjcsformats work", {

  ## core jjcsformat tests for format strings

  expect_error(
    format_value(values, format = "xx.x (xx.xx)"),
    "unknown format label"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx.x (xx.xx)")),
    "5.1 (7.89)"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_R("xx.x (xx.xx)")),
    "5.1 (7.89)"
  )



  expect_identical(
    format_value(c(5.05,values[2]), format = jjcsformat_xx_SAS("xx.x (xx.xx)")),
    "5.1 (7.89)"
  )

  expect_identical(
    format_value(c(5.05,values[2]), format = jjcsformat_xx_R("xx.x (xx.xx)")),
    "5.0 (7.89)"
  )

  expect_identical(
    format_value(c(5.15,values[2]), format = jjcsformat_xx_R("xx.x (xx.xx)")),
    "5.2 (7.89)"
  )

  expect_identical(
    format_value(c(5.15,values[2]), format = "xx.x (xx.x)"),
    "5.2 (7.9)"
  )

  expect_identical(
    format_value(c(4.15,values[2]), format = jjcsformat_xx_SAS("xx.x (xx.xx)")),
    "4.2 (7.89)"
  )

  expect_identical(
    format_value(c(4.15,values[2]), format = jjcsformat_xx_R("xx.x (xx.xx)")),
    "4.2 (7.89)"
  )

  expect_identical(
    format_value(c(4.15,values[2]), format = "xx.x (xx.x)"),
    "4.2 (7.9)"
  )

  expect_identical(
    format_value(c(4.15,values[2]), format = jjcsformat_xx_SAS("xx.x (xx.x)")),
    "4.2 (7.9)"
  )

  expect_identical(
    format_value(c(3.15,values[2]), format = "xx.x (xx.x)"),
    "3.1 (7.9)"
  )

  expect_identical(
    format_value(c(3.15,values[2]), format = jjcsformat_xx_SAS("xx.x (xx.x)")),
    "3.2 (7.9)"
  )

  expect_identical(
    format_value(c(3.15,values[2]), format = jjcsformat_xx_R("xx.x (xx.x)")),
    "3.1 (7.9)"
  )






  # expect_identical(
  #   format_value(values[1], format = "xx%"),
  #   paste0(values[1] * 100, "%")
  # )
  #
  # expect_identical(
  #   format_value(values[1], format = "xx.%"),
  #   "512%"
  # )
  #
  # expect_identical(
  #   format_value(values[1], format = "xx.x%"),
  #   "512.3%"
  # )
  #
  # expect_identical(
  #   format_value(values[1], format = "xx.xx%"),
  #   "512.35%"
  # )
  #
  # expect_identical(
  #   format_value(values[1], format = "xx.xxx%"),
  #   "512.346%"
  # )



  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx / xx")),
    paste(values, collapse = " / ")
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx. / xx.")),
    "5 / 8"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx.x / xx.x")),
    "5.1 / 7.9"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx.xx / xx.xx")),
    "5.12 / 7.89"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx.xxx / xx.xxx")),
    "5.123 / 7.891"
  )




  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("(xx, xx)")),
    paste0("(", values[1], ", ", values[2], ")")
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("(xx., xx.)")),
    "(5, 8)"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("(xx.x, xx.x)")),
    "(5.1, 7.9)"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("(xx.xx, xx.xx)")),
    "(5.12, 7.89)"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("(xx.xxx, xx.xxx)")),
    "(5.123, 7.891)"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("(xx.xxxx, xx.xxxx)")),
    "(5.1235, 7.8911)"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx - xx")),
    paste(values, collapse = " - ")
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx.x - xx.x")),
    "5.1 - 7.9"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx.xx - xx.xx")),
    "5.12 - 7.89"
  )



  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx (xx)")),
    paste0(values[1], " (", values[2], ")")
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx (xx.)")),
    paste0(values[1], " (8)")
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx (xx.x)")),
    paste0(values[1], " (7.9)")
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx (xx.xx)")),
    paste0(values[1], " (7.89)")
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx. (xx.)")),
    paste0(5, " (8)")
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx.x (xx.x)")),
    "5.1 (7.9)"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx.xx (xx.xx)")),
    "5.12 (7.89)"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx.x, xx.x")),
    "5.1, 7.9"
  )

  expect_identical(
    format_value(values, format = jjcsformat_xx_SAS("xx.x to xx.x")),
    "5.1 to 7.9"
  )

  expect_identical(
    format_value(c(values, 10.1235), format = jjcsformat_xx_SAS("xx. (xx. - xx.)")),
    "5 (8 - 10)"
  )

  expect_identical(
    format_value(c(values, 10.1235), format = jjcsformat_xx_SAS("xx.x (xx.x - xx.x)")),
    "5.1 (7.9 - 10.1)"
  )

  expect_identical(
    format_value(c(values, 10.1235), format = jjcsformat_xx_SAS("xx.xx (xx.xx - xx.xx)")),
    "5.12 (7.89 - 10.12)"
  )

  expect_identical(
    format_value(c(values, 10.1235), format = jjcsformat_xx_SAS("xx.xxx (xx.xxx - xx.xxx)")),
    "5.123 (7.891 - 10.124)"
  )

  expect_identical(format_value(NULL, jjcsformat_xx_SAS("xx")), "")




  expect_identical(format_value(c(500), jjcsformat_xx_SAS("N=xx")), "N=500")
  expect_identical(format_value(c(500), jjcsformat_xx_SAS("(N=xx)")), "(N=500)")

  ## errors

  expect_error(format_value(5.1, jjcsformat_xx_SAS("abcd")), "input str should contain xx")
  expect_error(format_value(5.1, jjcsformat_xx_SAS("xx - xx")), "jjcs_format_xx should contain same number of xx as the number of stats")

  expect_error(format_value(c(5.1, 2, 3), jjcsformat_xx_SAS("xx - xx")), "jjcs_format_xx should contain same number of xx as the number of stats")


  ## trailing 0s are correct
  expect_identical(format_value(0, jjcsformat_xx_SAS("xx.")), "0")
  expect_identical(format_value(0, jjcsformat_xx_SAS("xx.x")), "0.0")
  expect_identical(format_value(0, jjcsformat_xx_SAS("xx.xx")), "0.00")
  expect_identical(format_value(0, jjcsformat_xx_SAS("xx.xxx")), "0.000")
  expect_identical(format_value(0, jjcsformat_xx_SAS("xx.xxxx")), "0.0000")

})

test_that("jjcsformats NA works", {
  ## handling NAs

  expect_identical(
    format_value(NA, jjcsformat_xx_SAS("xx."), na_str = "-"),
    "-"
  )
  expect_identical(
    format_value(NA, jjcsformat_xx_SAS("xx"), na_str = "-"),
    "-"
  )

  expect_error(
    format_value(c(1, NA), jjcsformat_xx_SAS("xx")),
    "jjcs_format_xx should contain same number of xx as the number of stats"
  )

  expect_identical(
    format_value(c(1.2, NA, NA), jjcsformat_xx_SAS("xx.x (xx.x - xx.x)"), na_str = "NA"),
    c("1.2 (NA - NA)")
  )

  expect_identical(
    format_value(c(1.2, NA, NA), jjcsformat_xx_SAS("xx.x (xx.x - xx.x)"), na_str = "x"),
    c("1.2 (x - x)")
  )

  expect_identical(
    format_value(c(NA, NA, NA), jjcsformat_xx_SAS("xx.x (xx.x - xx.x)"), na_str = "x"),
    c("x")
  )

  # expect_identical(
  #   format_value(c(1.2, NA, NA), "xx.x (xx.x - xx.x)", na_str = c("x","y")),
  #   c("1.2 (x - y)")
  # )

  expect_error(
    format_value(c(1.2, NA, NA), jjcsformat_xx_SAS("xx.x (xx.x - xx.x)"), na_str = c("x","y")),
    "are of different length"
  )





  expect_identical(
    format_value(c(NA, NA), format = jjcsformat_xx_SAS("xx.x - xx.x"), na_str = c("hi", "lo")),
    "hi - lo"
  )

  expect_identical(
    format_value(c(NA, NA), format = jjcsformat_xx_SAS("xx.x - xx.x"), na_str = "what"),
    "what"
  )

  expect_identical(
    format_value(c(NA, 5.2), format = jjcsformat_xx_SAS("xx.x - xx.x"), na_str = "what"),
    "what - 5.2"
  )

  expect_identical(
    format_value(c(NA, 5.2), format = jjcsformat_xx_SAS("xx.x - xx.x"), na_str = c("hi", "lo")),
    "hi - 5.2"
  )

  expect_identical(
    format_value(NA, format = jjcsformat_xx_SAS("xx.x"), na_str = character()),
    "NA"
  )

  expect_identical(
    format_value(NA, format = jjcsformat_xx_SAS("xx.x"), na_str = NA_character_),
    "NA"
  )


  # 3 d formats

  expect_identical(
    format_value(c(6.23,NA,NA), format = jjcsformat_xx_SAS("xx.x (xx.xx, xx.xx)"), na_str = "-"),
    "6.2 (-, -)"
  )
  expect_identical(
    format_value(c(NA,NA,NA), format = jjcsformat_xx_SAS("xx.x (xx.xx, xx.xx)"), na_str = "-"),
    "-"
  )
  expect_identical(
    format_value(c(6.23,NA,NA), format = jjcsformat_xx_SAS("xx.x (xx.xx, xx.xx)"), na_str = c("-","x","x")),
    "6.2 (x, x)"
  )
  expect_identical(
    format_value(c(6.23,NA,NA), format = jjcsformat_xx_SAS("xx.x (xx.xx, xx.xx)"), na_str = c("-","x","y")),
    "6.2 (x, y)"
  )


})

test_that("jjcsformats count_fraction works", {
  expect_identical(
    format_value(cdf, format = jjcsformat_count_denom_fraction),
    "5/2000 (0.3%)"
  )

  expect_identical(
    format_value(cf, format = jjcsformat_count_fraction),
    "5 (0.3%)"
  )

  expect_identical(
    format_value(cf, format = "xx (xx.x%)"),
    "5 (0.2%)"
  )

  expect_identical(
    format_value(c(2000,2001,2000/2001), format = jjcsformat_count_denom_fraction),
    "2000/2001 (>99.9%)"
  )

  expect_identical(
    format_value(c(2000,2000/2001), format = "xx (xx.x%)"),
    "2000 (100.0%)"
  )

  # expect_identical(
  #   format_value(c(2000,2000/2001), format = tern::format_count_fraction_fixed_dp),
  #   "2000 (100.0%)"
  # )


  expect_identical(
    format_value(c(1,2001,1/2001), format = jjcsformat_count_denom_fraction),
    "1/2001 (<0.1%)"
  )

  expect_identical(
    format_value(c(1,1/2001), format = "xx (xx.x%)"),
    "1 (0.0%)"
  )

  # expect_identical(
  #   format_value(c(1,1/2001), format = tern::format_count_fraction_fixed_dp),
  #   "1 (0.0%)"
  # )


  expect_identical(
    format_value(c(3,3,3/3), format = jjcsformat_count_denom_fraction),
    "3/3 (100%)"
  )

  expect_identical(
    format_value(c(3,3/3), format = "xx (xx.x%)"),
    "3 (100.0%)"
  )

  # expect_identical(
  #   format_value(c(3,3/3), format = tern::format_count_fraction_fixed_dp),
  #   "3 (100%)"
  # )



})










