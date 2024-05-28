#' SAS rounding in R
#'
#' @description Copied from tidytlg roundSAS to avoid importing
#' round_sas comes from this Stack Overflow post https://stackoverflow.com/questions/12688717/round-up-from-5
#' \cr Internal function only
#' @param x Numeric vector.
#' @param digits An integer specifying the number of decimal places to be
#'   displayed after rounding. Default is 0.
#' @seealso  [format_xx_sas()], [format_sas_count_fraction()], [format_sas_count_denom_fraction()]
#' @return character vector of rounded values
round_sas <- function(x,
                      digits = 0) {
  # perform SAS rounding
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^digits
  z <- z * posneg

  ## return numeric vector of rounded values
  z
}

#' Check whether `x` is a proportion: number between 0 and 1.
#'
#' This function is copied from internal tern function assert_proportion_value, to avoid import
#'
#' @param x (`any`)\cr object to test.
#' @param include_boundaries (`flag`)\cr whether to include boundaries when testing
#'   for proportions.
#'
assert_proportion_value <- function(x, include_boundaries = FALSE) {
  checkmate::assert_number(x, lower = 0, upper = 1)
  checkmate::assert_flag(include_boundaries)
  if (isFALSE(include_boundaries)) {
    checkmate::assert_true(x > 0)
    checkmate::assert_true(x < 1)
  }
}








format_xx_fct <- function(round_method = c("SAS", "R")) {
  round_method <- match.arg(round_method)

  if (round_method == "SAS") {
    roundfunc <- round_sas
  }
  if (round_method == "R") {
    roundfunc <- round
  }

  fnct <- function(str) {
    if (grepl("xxx.", str, fixed = TRUE)) {
      stop("Error: format_xx: do not use xxx. in input str, replace by xx. instead")
    }
    if (!(grepl("xx", str, fixed = TRUE))) {
      stop("Error: format_xx: input str should contain xx")
    }
    positions <- gregexpr(
      pattern = "xx\\.?x*", text = str,
      perl = TRUE
    )
    x_positions <- regmatches(x = str, m = positions)[[1]]
    ### str is splitted into pieces as xx. xx xx.xxx
    ### xx is no rounding
    ### xx. rounding to integer
    ### xx.x rounding to 1 decimal, etc


    no_round <- function(x, na_str = "NA") {
      if (is.na(x)) {
        return(na_str)
      } else {
        return(x)
      }
    }

    roundings <- lapply(X = x_positions, function(x) {
      y <- strsplit(split = "\\.", x = x)[[1]]
      ### "xx.x" will result in c("xx","x")
      ### "xx." will result in "xx"
      ### "xx" will remain "xx"

      if (x == "xx") {
        rounding <- no_round
      } else {
        rounding <- function(x, na_str = "NA") {
          if (is.na(x)) {
            return(na_str)
          }
          format(roundfunc(x, digits = ifelse(length(y) >
            1, nchar(y[2]), 0)), nsmall = ifelse(length(y) >
            1, nchar(y[2]), 0))
        }
      }
      return(rounding)
    })
    rtable_format <- function(x, output, na_str = "NA") {
      if (!length(positions[[1]]) == length(x)) {
        stop("Error: input str in call to format_xx should contain same number of xx as the number of stats")
      }

      if ((length(na_str) == 1) & length(x) > 1) na_str <- rep(na_str, length(x))
      if ((length(na_str) > 1) & length(x) != length(na_str)) {
        stop(
          "input and na_str ",
          paste0("c(", toString(sprintf("'%s'", na_str)), ")"),
          " are of different length"
        )
      }

      # values <- Map(y = x, fun = roundings, function(y, fun) fun(y))
      values2 <- list()
      for (i in 1:length(x)) {
        values2[[i]] <- roundings[[i]](x[[i]], na_str = na_str[[i]])
      }

      regmatches(x = str, m = positions)[[1]] <- values2
      return(str)
    }
    return(rtable_format)
  }
  return(fnct)
}



#' xx-style formatting functions
#' @name format_xx
#' @rdname format_xx
#' @description
#' 2 formatting functions that support the string `(str)` to be in xx style format.
#' @param str xx style formatting string
#' @return
#' `format_xx_sas(str)`  is a format function with SAS rounding method that can be used as `format` argument in `format_value` calls
#' \cr `format_xx_r(str)` is a format function that can be used as `format` argument in `format_value` calls
#'
#' @examples
#' format_value(c(1.453), format_xx_sas("xx.xx"))
#' format_value(c(1.453), format_xx_r("xx.xx"))
#' format_value(c(1.453, 2.45638), format_xx_sas("xx.xx (xx.xxx)"))
#' format_value(c(1.453, 2.45638), format_xx_r("xx.xx (xx.xxx)"))
#' @export
format_xx_sas <- format_xx_fct(round_method = "SAS")

#' @rdname format_xx
#' @export
format_xx_r <- format_xx_fct(round_method = "R")



# Core function factory for count_fraction / count_denom_fraction  format style
format_xx_count_fraction_fct <- function(round_method = c("SAS", "R"), type = c("count_fraction", "count_denom_fraction")) {
  round_method <- match.arg(round_method)
  type <- match.arg(type)

  if (round_method == "SAS") {
    roundfun <- round_sas
  }
  if (round_method == "R") {
    roundfun <- round
  }



  fun <- function(x, output, d = 1) {
    checkmate::assert_vector(x)

    count <- x[1]
    checkmate::assert_integerish(count)

    if (type == "count_fraction") {
      denom <- NULL
      fraction <- x[2]
      fdenom <- NULL

      checkmate::assert_vector(x, min.len = 2, max.len = 2)
    }
    if (type == "count_denom_fraction") {
      denom <- x[2]
      fraction <- x[3]
      fdenom <- paste0("/", denom)
      checkmate::assert_vector(x, min.len = 3, max.len = 3)
    }

    attr(x, "label") <- NULL
    if (any(is.na(x))) {
      return("-")
    }

    assert_proportion_value(fraction, include_boundaries = TRUE)

    fmtpct <- format(roundfun(fraction * 100, d), nsmall = d)

    # fraction is the result of a division, so in some cases it is not exactly equal 1, even coming from x/x
    # if it is nearly equal to 1, set it to 1
    if (isTRUE(all.equal(fraction, 1))) fraction <- 1


    result <- if (count == 0) {
      "0"
    }
    ## per conventions report 100.0 as 100
    else if (fraction == 1) {
      paste0(count, fdenom, " (100%)")
    }
    ### <0.1% (even if fmtpct == 0.1, but the actual value of pct <0.1)
    ### example pct = 0.09999
    # else if (100*x[2] < 10**(-d)) {
    else if (fmtpct == format(0, nsmall = d)) {
      paste0(count, fdenom, " (<", 10**(-d), "%)")
    }
    ### >99.9% (even if fmtpct == 99.9, but the actual value of pct >99.9)
    ### example pct = 99.90001
    # else if (100*x[2] > 100-10**(-d)) {
    else if (fmtpct == format(100, nsmall = d)) {
      paste0(count, fdenom, " (>", 100 - 10**(-d), "%)")
    } else {
      paste0(count, fdenom, " (", fmtpct, "%)")
    }
    return(result)
  }
  return(fun)
}

#' @title Count_fraction
#' @name count_fraction
#' @aliases format_count_fraction
#' @description A family of formatting functions for presenting a count together with fraction (and denominator) with special consideration when count is 0, or fraction is 1.
#' \cr `format_r_count_fraction` is a version that is similar to tern function tern::format_count_fraction_fixed_dp
#' @rdname count_fraction
#'
#' @param x `numeric`\cr with elements `num` and `fraction` or `num`, `denom` and `fraction`.
#' @param d numeric(1). Number of digits to round fraction to (default=1)
#' @param output required for rtables interface
#'
#' @return A string in the format `count / denom (ratio %)`. If `count` is 0, the format is `0`. If fraction is >0.99, the format is `count / denom (>99.9%)`
#' @export
#' @examples
#' format_sas_count_fraction(c(7, 0.7))
#' format_sas_count_fraction(c(70000, 0.9999999))
#' format_sas_count_fraction(c(70000, 1))
#'
#' format_sas_count_denom_fraction(c(3, 2000, 3 / 2000))
#' format_sas_count_denom_fraction(c(3, 2000, 3 / 2000))
format_sas_count_fraction <- format_xx_count_fraction_fct("SAS", "count_fraction")



#' @rdname count_fraction
#' @export

format_r_count_fraction <- format_xx_count_fraction_fct("R", "count_fraction")


#' @rdname count_fraction
#' @export
#'
format_sas_count_denom_fraction <- format_xx_count_fraction_fct("SAS", "count_denom_fraction")

#' @rdname count_fraction
#' @export
#'
#' @examples
#' format_r_count_denom_fraction(c(3, 2000, 3 / 2000))
#' format_r_count_denom_fraction(c(3, 2000, 3 / 2000))
format_r_count_denom_fraction <- format_xx_count_fraction_fct("R", "count_denom_fraction")
