#' SAS rounding in R
#'
#' roundSAS is an alternative rounding function, ensuring that decimals equal or
#' bigger than 5 are rounded upwards to the nearest number and returned as
#' character vector.
#'
#' At the midpoint of a decimal place (e.g. 0.5, 1.5), the round function in R
#' rounds to the nearest even number (i.e. 0.5 is rounded to 0; 1.5 is rounded
#' to 2), whereas SAS rounds to the nearest number (i.e. 0.5 is rounded to 1;
#' 1.5 is rounded to 2). The roundSAS function is an alternative rounding
#' function for R that ensures rounding to the nearest number, as done in SAS.
#' roundSAS comes from this Stack Overflow post https://stackoverflow.com/questions/12688717/round-up-from-5
#'
#' Copied from tidytlg roundSAS
#'
#' @param x Numeric vector.
#' @param digits An integer specifying the number of decimal places to be
#'   displayed after rounding. Default is 0.
#' @param as_char logical value indicating conversion of rounded numerical
#'   vector to character vector; default is FALSE
#' @param na_char A character string indicating missing value; if not specified,
#'   "NA" is created
#'
#' @return character vector of rounded values
#' @export
#'
#' @examples
#' ### input data vector with midpoint decimals
#' x <- c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)
#'
#' ### rounds to integer
#' roundSAS(x, digits = 0)
#'
#' ### input data vector with a missing value
#' y <- c(8.65, 8.75, NA, 9.85, 9.95)
#'
#' ### rounds to tenths and label the missing value with "NE"
#' roundSAS(y, digits = 1, as_char = TRUE, na_char = "NE")
#'
roundSAS <- function (x, digits = 0, as_char = FALSE, na_char = NULL)
{
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z/10^digits
  z <- z * posneg
  if (as_char & is.null(na_char)) {
    formatC(z, format = "f", digits = digits)
  }
  else if (as_char & !is.null(na_char)) {
    formatC(z, format = "f", digits = digits) %>% str_replace(" *(NA|NaN|NULL)",
                                                              na_char)
  }
  else {
    z
  }
}

#' Title: Function factory for xx style formatting
#'
#' @description A function factory to generate formatting functions for value
#' formatting that support the xx style format and control the rounding method
#'
#' @param roundmethod (`string`)\cr choice of rounding methods. Options are:
#'   * `SAS`: the underlying rounding method is `tidytlg::roundSAS`, where \cr
#'   roundSAS comes from this Stack Overflow post https://stackoverflow.com/questions/12688717/round-up-from-5
#'   * `R`: the underlying rounding method is `round`
#'
#' @return `jjcs_format_xx_fct()` format function that can be used in rtables formatting calls
#' @export
#'
#' @family JJCS formats
#' @examples
#' jjcsformat_xx_SAS <- jjcs_format_xx_fct(roundmethod="SAS")
#' jjcsformat_xx <- jjcsformat_xx_SAS
#' rtables::rcell(c(1.453),jjcsformat_xx("xx.xx"))
#' rtables::rcell(c(1.453,2.45638),jjcsformat_xx("xx.xx (xx.xxx)"))
#'
jjcs_format_xx_fct <- function (roundmethod=c("SAS","R"))
{
  roundmethod = match.arg(roundmethod)

  if (roundmethod == "SAS") {
    roundfunc <- roundSAS
  }
  if (roundmethod == "R") {
    roundfunc <- round
  }

  fnct <- function(str){

    if (stringr::str_detect(str,stringr::fixed("xxx."))) {
      stop("Error: jjcs_format_xx: do not use xxx. in input str, replace by xx. instead")
    }
    if (!stringr::str_detect(str,stringr::fixed("xx"))) {
      stop("Error: jjcs_format_xx: input str should contain xx")
    }
    positions <- gregexpr(pattern = "xx\\.?x*", text = str,
                          perl = TRUE)
    x_positions <- regmatches(x = str, m = positions)[[1]]
    ### str is splitted into pieces as xx. xx xx.xxx
    ### xx is no rounding
    ### xx. rounding to integer
    ### xx.x rounding to 1 decimal, etc


    no_round <- function(x,na_str="NA"){
      if (is.na(x)) return(na_str) else
        return(x)
    }

    roundings <- lapply(X = x_positions, function(x) {
      y <- strsplit(split = "\\.", x = x)[[1]]
      ### "xx.x" will result in c("xx","x")
      ### "xx." will result in "xx"
      ### "xx" will remain "xx"

      if (x == "xx") rounding <- no_round
      else rounding <- function(x,na_str="NA") {
        if (is.na(x)) return(na_str)
        format(roundfunc(x, digits = ifelse(length(y) >
                                              1, nchar(y[2]), 0)),nsmall = ifelse(length(y) >
                                                                                    1, nchar(y[2]), 0))
      }
      return(rounding)
    })
    rtable_format <- function(x, output,na_str="NA") {

      if (!length(positions[[1]])==length(x)) {
        stop("Error: input str in call to jjcs_format_xx should contain same number of xx as the number of stats")
      }

      if ((length(na_str) == 1) & length(x) > 1) na_str <- rep(na_str,length(x))
      if ((length(na_str) > 1) & length(x) != length(na_str)) na_str <- rep(na_str,length(x))

      #values <- Map(y = x, fun = roundings, function(y, fun) fun(y))
      values2 <- list()
      for (i in 1:length(x)){
        values2[[i]] <- roundings[[i]](x[[i]],na_str=na_str[[i]])
      }

      regmatches(x = str, m = positions)[[1]] <- values2
      return(str)
    }
    return(rtable_format)
  }
  return(fnct)
}


jjcsformat_xx_SAS <- jjcs_format_xx_fct(roundmethod="SAS")
jjcsformat_xx_R <- jjcs_format_xx_fct(roundmethod="R")


### if we ever decide to switch rounding method, we just have to update jjcsformat_xx here

jjcsformat_xx <- jjcsformat_xx_SAS

#' Title: Formatting count and fraction values
#'
#' @description
#'
#' Formats a count together with fraction (and/or denominator) with special consideration when count is 0, or fraction is 1.
#' \cr See also: {tern::format_count_fraction_fixed_dp()}
#'
#' @inheritParams jjcs_format_xx_fct
#' @param x `numeric`\cr with elements `num` and `fraction` or `num`, `denom` and `fraction`.
#' @param d numeric(1). Number of digits to round fraction to (default=1)
#' @return A string in the format `count / denom (ratio %)`. If `count` is 0, the format is `0`. If fraction is >0.99, the format is `count / denom (>99.9%)`
#' @family JJCS formats
#' @name Count_fraction
#' @export
#'
#' @examples
#' jjcsformat_count_fraction(c(7,0.7))
#' jjcsformat_count_fraction(c(70000,0.9999999))
#' jjcsformat_count_fraction(c(70000,1))
#'

jjcsformat_count_fraction <- function (x,d=1, roundmethod=c("SAS","R"),...)
{
  roundmethod <- match.arg(roundmethod)
  attr(x, "label") <- NULL
  if (any(is.na(x))) {
    return("-")
  }

  checkmate::assert_vector(x)
  checkmate::assert_integerish(x[1])
  tern:::assert_proportion_value(x[2], include_boundaries = TRUE)

  fraction <- x[2]


  if (isTRUE(all.equal(fraction,1))) fraction <- 1

  if (roundmethod == "SAS"){
    fmtpct <- format(roundSAS(fraction * 100, d),nsmall=d)
  } else {
    fmtpct <- format(round(fraction * 100, d),nsmall=d)
  }

  result <-if (x[1] == 0) {
    "0"
  }
  ## per conventions still report as 100.0%
  else if (fraction == 1) {
    paste0(x[1], " (100.0%)")
  }
  ### <0.1% (even if fmtpct == 0.1, but the actual value of pct <0.1)
  ### example pct = 0.09999
  # else if (100*x[2] < 10**(-d)) {
  else if (fmtpct == format(0,nsmall=d)) {
    paste0(x[1], " (<",10**(-d),"%)")
  }
  ### >99.9% (even if fmtpct == 99.9, but the actual value of pct >99.9)
  ### example pct = 99.90001
  #else if (100*x[2] > 100-10**(-d)) {
  else if (fmtpct == format(100,nsmall=d)) {
    paste0(x[1], " (>",100-10**(-d),"%)")
  }
  else {
    paste0(x[1], " (", fmtpct, "%)")
  }
  return(result)
}

#' Title: Formatting count, denominator and fraction values
#'
#' @inherit Count_fraction
#' @export
#' @rdname Count_fraction
#' @examples
#' jjcsformat_count_denom_fraction(c(7,10,0.7))
#' jjcsformat_count_denom_fraction(c(70000,70001,70000/70001))
#' jjcsformat_count_denom_fraction(c(235,235,235/235))
jjcsformat_count_denom_fraction <- function (x, d=1,roundmethod=c("SAS","R"),...)
{
  roundmethod <- match.arg(roundmethod)
  attr(x, "label") <- NULL
  if (any(is.na(x))) {
    return("-")
  }
  checkmate::assert_vector(x)
  checkmate::assert_integerish(x[1])
  tern:::assert_proportion_value(x[3], include_boundaries = TRUE)

  fraction <- x[3]
  if (x[2] == x[1]) fraction <- 1


  fmt_x12 <- paste0(x[1], "/",x[2])

  if (roundmethod == "SAS"){
    fmtpct <- format(roundSAS(fraction * 100, d),nsmall=d)
  } else {
    fmtpct <- format(round(fraction * 100, d),nsmall=d)
  }

  result <- if (x[1] == 0) {
    "0"
  }
  else if (100*fraction == 100) {
    paste0(fmt_x12, " (100.0%)")
  }
  ### <0.1% (even if fmtpct == 0.1, but the actual value of pct <0.1)
  ### example pct = 0.09999
  else if (100*fraction < 10**(-d)) {
    paste0(fmt_x12, " (<",10**(-d),"%)")
  }
  ### >99.9% (even if fmtpct == 99.9, but the actual value of pct >99.9)
  ### example pct = 99.90001
  else if (100*fraction > 100-10**(-d)) {
    paste0(fmt_x12, " (>",100-10**(-d),"%)")
  }
  else {
    paste0(fmt_x12," (", fmtpct, "%)")
  }
  return(result)
}
