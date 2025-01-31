fun_takes <- function(f, nm) {
  nm %in% names(formals(f))
}

call_format_fun <- function(f,
                            value,
                            na_str,
                            round_type,
                            output) {
  args <- c(
    list(value),
    if (fun_takes(f, "na_str")) list(na_str = na_str),
    if (fun_takes(f, "round_type")) list(round_type = round_type),
    if (fun_takes(f, "output")) list(output = output)
  )
  do.call(f, args)
}

formats_1d <- c(
  "xx", "xx.", "xx.x", "xx.xx", "xx.xxx", "xx.xxxx",
  "xx%", "xx.%", "xx.x%", "xx.xx%", "xx.xxx%", "(N=xx)", "N=xx", ">999.9", ">999.99",
  "x.xxxx | (<0.0001)"
)

formats_2d <- c(
  "xx / xx", "xx. / xx.", "xx.x / xx.x", "xx.xx / xx.xx", "xx.xxx / xx.xxx",
  "N=xx (xx%)", "xx (xx%)", "xx (xx.%)", "xx (xx.x%)", "xx (xx.xx%)",
  "xx. (xx.%)", "xx.x (xx.x%)", "xx.xx (xx.xx%)",
  "(xx, xx)", "(xx., xx.)", "(xx.x, xx.x)", "(xx.xx, xx.xx)",
  "(xx.xxx, xx.xxx)", "(xx.xxxx, xx.xxxx)",
  "xx - xx", "xx.x - xx.x", "xx.xx - xx.xx",
  "xx (xx)", "xx. (xx.)", "xx.x (xx.x)", "xx.xx (xx.xx)",
  "xx (xx.)", "xx (xx.x)", "xx (xx.xx)",
  "xx.x, xx.x",
  "xx.x to xx.x"
)

formats_3d <- c(
  "xx. (xx. - xx.)",
  "xx.x (xx.x - xx.x)",
  "xx.xx (xx.xx - xx.xx)",
  "xx.xxx (xx.xxx - xx.xxx)"
)

#' List of currently supported formats and vertical alignments
#'
#' @description We support `xx` style format labels grouped by 1d, 2d, and 3d.
#' Currently valid format labels cannot be added dynamically. Format functions
#' must be used for special cases.
#'
#' @return
#' * `list_valid_format_labels()` returns a nested list, with elements listing the supported 1d, 2d,
#'   and 3d format strings.
#'
#' @examples
#' list_valid_format_labels()
#'
#' @name list_formats
#' @export
list_valid_format_labels <- function() {
  structure(
    list(
      "1d" = formats_1d,
      "2d" = formats_2d,
      "3d" = formats_3d
    ),
    info = "xx does not modify the element, and xx. rounds a number to 0 digits"
  )
}

#' @return
#' * `list_valid_aligns()` returns a character vector of valid vertical alignments.
#'
#' @examples
#' list_valid_aligns()
#'
#' @name list_formats
#' @export
list_valid_aligns <- function() {
  c("left", "right", "center", "decimal", "dec_right", "dec_left")
}

#' Check if a format or alignment is supported
#'
#' @description Utility functions for checking formats and alignments.
#'
#' @param x (`string` or `function`)\cr format string or an object returned by [sprintf_format()]
#' @param stop_otherwise (`flag`)\cr whether an error should be thrown if `x` is not a valid format.
#'
#' @return
#' * `is_valid_format` returns `TRUE` if `x` is `NULL`, a supported format string, or a function, and
#'   `FALSE` otherwise.
#'
#' @note If `x` is a function, no check is performed to verify that it returns a valid format.
#'
#' @examples
#' is_valid_format("xx.x")
#' is_valid_format("fakeyfake")
#'
#' @name check_formats
#' @export
is_valid_format <- function(x, stop_otherwise = FALSE) {
  is_valid <- is.null(x) || (length(x) == 1 && (is.function(x) || x %in% unlist(list_valid_format_labels())))

  if (stop_otherwise && !is_valid) {
    stop("format needs to be a format label, sprintf_format object, a function, or NULL")
  }

  is_valid
}

#' @param algn (`character`)\cr a character vector that indicates the requested cell alignments.
#'
#' @return
#'  * `check_aligns` returns `TRUE` if the provided alignments are supported, otherwise, an error is thrown.
#'
#' @examples
#' check_aligns(c("decimal", "dec_right"))
#'
#' @name check_formats
#' @export
check_aligns <- function(algn) {
  if (anyNA(algn)) {
    stop("Got missing-value for text alignment.")
  }
  invalid <- setdiff(algn, list_valid_aligns())
  if (length(invalid) > 0) {
    stop("Unsupported text-alignment(s): ", paste(invalid, collapse = ", "))
  }
  invisible(TRUE)
}

#' Specify text format via a `sprintf` format string
#'
#' @param format (`string`)\cr a format string passed to [sprintf()].
#'
#' @return A formatting function which wraps and applies the specified `sprintf`-style format
#'   to string `format`.
#'
#' @seealso [sprintf()]
#'
#' @examples
#' fmtfun <- sprintf_format("(N=%i")
#' format_value(100, format = fmtfun)
#'
#' fmtfun2 <- sprintf_format("%.4f - %.2f")
#' format_value(list(12.23456, 2.724))
#'
#' @export
sprintf_format <- function(format) {
  function(x, ...) {
    do.call(sprintf, c(list(fmt = format), x))
  }
}

#' Round and prepare a value for display
#'
#' This function is used within [format_value()] to prepare numeric values within
#' cells for formatting and display.
#'
#' @param x (`numeric(1)`)\cr value to format.
#' @param digits (`numeric(1)`)\cr number of digits to round to, or `NA` to convert to a
#'   character value with no rounding.
#' @param na_str (`string`)\cr the value to return if `x` is `NA`.
#' @param round_type (`"iec"` or `"sas"`)\cr the type of rounding to perform. iec,
#'   the default, peforms rounding compliant with IEC 60559 (see details), while
#'   sas performs nearest-value rounding consistent with rounding within SAS.
#'
#' @details
#' This function combines rounding behavior with the strict decimal display of
#' [sprintf()]. By default, R's standards-compliant [round()]
#' function (see the Details section of that documentation) is used. The exact
#' behavior is as follows:
#'
#' \enumerate{
#'   \item{If `x` is `NA`, the value of `na_str` is returned.}
#'   \item{If `x` is non-`NA` but `digits` is `NA`, `x` is converted to a character and returned.}
#'   \item{If `x` and `digits` are both non-NA, [round()] is called first, and then [sprintf()]
#'     is used to convert the rounded value to a character with the appropriate number of trailing
#'     zeros enforced.}
#' }
#'
#' @return A character value representing the value after rounding, containing any trailing zeros
#'   required to display *exactly* `digits` elements.
#'
#' @note
#' This differs from the base R [round()] function in that `NA` digits indicate `x` should be converted
#' to character and returned unchanged whereas `round(x, digits=NA)` returns `NA` for all values of `x`.
#'
#' This behavior will differ from `as.character(round(x, digits = digits))` in the case where there are
#' not at least `digits` significant digits after the decimal that remain after rounding. It *may* differ from
#' `sprintf("\%.Nf", x)` for values ending in `5` after the decimal place on many popular operating systems
#' due to `round`'s stricter adherence to the IEC 60559 standard, particularly for R versions > 4.0.0 (see
#' warning in [round()] documentation).
#'
#' @seealso [format_value()], [round()], [sprintf()]
#'
#' @examples
#' round_fmt(0, digits = 3)
#' round_fmt(.395, digits = 2)
#' round_fmt(NA, digits = 1)
#' round_fmt(NA, digits = 1, na_str = "-")
#' round_fmt(2.765923, digits = NA)
#' round_fmt(0.845, digits = 2)
#' round_fmt(0.845, digits = 2, round_type = "sas")
#'
#' @export
#' @aliases rounding
round_fmt <- function(x, digits, na_str = "NA", round_type = c("iec", "sas")) {
  round_type <- match.arg(round_type)
  if (!is.na(digits) && digits < 0) {
    stop("round_fmt currently does not support non-missing values of digits < 0")
  }
  if (is.na(x)) {
    na_str
  } else if (is.na(digits)) {
    paste0(x)
  } else {
    rndx <- switch(round_type,
      iec = round(x, digits),
      sas = round_sas(x, digits)
    )
    sprfmt <- paste0("%.", digits, "f")
    sprintf(fmt = sprfmt, rndx)
  }
}

## https://stackoverflow.com/questions/12688717/round-up-from-5

#' @inheritParams round_fmt
#'
round_sas <- function(x,
                      digits = 0) {
  # perform SAS rounding ----------------------------------------------------
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^digits
  z <- z * posneg
  ## return numeric vector of rounded values
  z
}


val_pct_helper <- function(x, dig1, dig2, na_str, pct = TRUE, round_type = c("iec", "sas")) {
  if (pct) {
    x[2] <- x[2] * 100
  }
  if (length(na_str) == 1) {
    na_str <- rep(na_str, 2)
  }
  paste0(
    round_fmt(x[1], digits = dig1, na_str = na_str[1], round_type = round_type),
    " (",
    round_fmt(x[2], digits = dig2, na_str = na_str[2], round_type = round_type),
    if (pct) "%", ")"
  )
}

sep_2d_helper <- function(x, dig1, dig2, sep, na_str, wrap = NULL, round_type = c("iec", "sas")) {
  ret <- paste(mapply(round_fmt, x = x, digits = c(dig1, dig2), na_str = na_str, round_type = round_type),
    collapse = sep
  )
  if (!is.null(wrap)) {
    ret <- paste(c(wrap[1], ret, wrap[2]), collapse = "")
  }
  ret
}

## na_or_round <- function(x, digits, na_str) {
##     if(is.na(x))
##         na_str
##     else
##         round(x, digits = digits)
## }

#' Converts a (possibly compound) value into a string using the `format` information
#'
#' @param x (`ANY`)\cr the value to be formatted.
#' @param format (`string` or `function`)\cr the format label (string) or formatter function to
#'   apply to `x`.
#' @param na_str (`character`)\cr character vector to display when the values of `x` are missing.
#'   If only one string is provided, it is applied for all missing values. Defaults to `"NA"`.
#' @param output (`string`)\cr output type.
#'
#' @details A length-zero value for `na_str` will be interpreted as `"NA"`.
#'
#' @return Formatted text representing the cell `x`.
#'
#' @seealso [round_fmt()]
#'
#' @examples
#' x <- format_value(pi, format = "xx.xx")
#' x
#'
#' format_value(x, output = "ascii")
#'
#' # na_str works with multiple values
#' format_value(c(NA, 1, NA), format = "xx.x (xx.x - xx.x)", na_str = c("NE", "<missing>"))
#'
#' @export
format_value <- function(x, format = NULL, output = c("ascii", "html"), na_str = "NA", round_type = c("iec", "sas")) {
  ## if(is(x, "CellValue"))
  ##     x = x[[1]]

  if (length(x) == 0) {
    return("")
  }

  output <- match.arg(output)
  round_type <- match.arg(round_type)

  # Checks for NAs in the input
  if (length(na_str) == 0) {
    na_str <- "NA"
  }
  if (any(is.na(na_str))) {
    na_str[is.na(na_str)] <- "NA"
  }
  if (length(na_str) == 1) {
    if (!all(is.na(x))) {
      ## array adds an unneeded dim attribute which causes problems
      na_str <- rep(na_str, length(x))
    }
  } else { # length(na_str) > 1
    tmp_na_str <- rep("NA", length(x))
    tmp_na_str[is.na(x)] <- na_str[seq(sum(is.na(x)))]
    na_str <- tmp_na_str
  }
  # if (length(na_str) < sum(is.na(x))) { # not a fun of vec recycling
  #   na_str <- rep(na_str, length.out = sum(is.na(x)))
  # }

  txt <- if (all(is.na(x)) && length(na_str) == 1L) {
    na_str
  } else if (is.null(format)) {
    toString(x)
  } else if (is.function(format)) {
    call_format_fun(f = format, value = x, na_str = na_str, round_type = round_type, output = output)
  } else if (is.character(format)) {
    l <- if (format %in% formats_1d) {
      1
    } else if (format %in% formats_2d) {
      2
    } else if (format %in% formats_3d) {
      3
    } else {
      stop(
        "Unknown format label: '", format,
        "'. Run `list_valid_format_labels()` to get a list of all available formats."
      )
    }
    if (format != "xx" && length(x) != l) {
      stop(
        "Cell contents <", paste(x, collapse = ", "), "> and format '",
        format, "' are of different lengths (", length(x), " vs ", l, ")."
      )
    }
    switch(format,
      "xx" = as.character(x),
      "xx." = round_fmt(x, digits = 0, na_str = na_str, round_type = round_type),
      "xx.x" = round_fmt(x, digits = 1, na_str = na_str, round_type = round_type),
      "xx.xx" = round_fmt(x, digits = 2, na_str = na_str, round_type = round_type),
      "xx.xxx" = round_fmt(x, digits = 3, na_str = na_str, round_type = round_type),
      "xx.xxxx" = round_fmt(x, digits = 4, na_str = na_str, round_type = round_type),
      "xx%" = paste0(round_fmt(x * 100, digits = NA, na_str = na_str, round_type = round_type), "%"),
      "xx.%" = paste0(round_fmt(x * 100, digits = 0, na_str = na_str, round_type = round_type), "%"),
      "xx.x%" = paste0(round_fmt(x * 100, digits = 1, na_str = na_str, round_type = round_type), "%"),
      "xx.xx%" = paste0(round_fmt(x * 100, digits = 2, na_str = na_str, round_type = round_type), "%"),
      "xx.xxx%" = paste0(round_fmt(x * 100, digits = 3, na_str = na_str, round_type = round_type), "%"),
      "(N=xx)" = paste0("(N=", round_fmt(x, digits = NA, na_str = na_str, round_type = round_type), ")"),
      "N=xx" = paste0("N=", round_fmt(x, digits = NA, na_str = na_str, round_type = round_type)),
      ">999.9" = ifelse(x > 999.9, ">999.9", round_fmt(x, digits = 1, na_str = na_str, round_type = round_type)),
      ">999.99" = ifelse(x > 999.99, ">999.99", round_fmt(x, digits = 2, na_str = na_str, round_type = round_type)),
      "x.xxxx | (<0.0001)" = ifelse(x < 0.0001, "<0.0001", round_fmt(x, digits = 4, na_str = na_str, round_type = round_type)),
      "xx / xx" = sep_2d_helper(x, dig1 = NA, dig2 = NA, sep = " / ", na_str = na_str, round_type = round_type),
      "xx. / xx." = sep_2d_helper(x, dig1 = 0, dig2 = 0, sep = " / ", na_str = na_str, round_type = round_type),
      "xx.x / xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = " / ", na_str = na_str, round_type = round_type),
      "xx.xx / xx.xx" = sep_2d_helper(x, dig1 = 2, dig2 = 2, sep = " / ", na_str = na_str, round_type = round_type),
      "xx.xxx / xx.xxx" = sep_2d_helper(x, dig1 = 3, dig2 = 3, sep = " / ", na_str = na_str, round_type = round_type),
      "N=xx (xx%)" = paste0("N=", val_pct_helper(x, dig1 = NA, dig2 = NA, na_str = na_str, round_type = round_type)),
      "xx (xx%)" = val_pct_helper(x, dig1 = NA, dig2 = NA, na_str = na_str, round_type = round_type),
      "xx (xx.%)" = val_pct_helper(x, dig1 = NA, dig2 = 0, na_str = na_str, round_type = round_type),
      "xx (xx.x%)" = val_pct_helper(x, dig1 = NA, dig2 = 1, na_str = na_str, round_type = round_type),
      "xx (xx.xx%)" = val_pct_helper(x, dig1 = NA, dig2 = 2, na_str = na_str, round_type = round_type),
      "xx. (xx.%)" = val_pct_helper(x, dig1 = 0, dig2 = 0, na_str = na_str, round_type = round_type),
      "xx.x (xx.x%)" = val_pct_helper(x, dig1 = 1, dig2 = 1, na_str = na_str, round_type = round_type),
      "xx.xx (xx.xx%)" = val_pct_helper(x, dig1 = 2, dig2 = 2, na_str = na_str, round_type = round_type),
      "(xx, xx)" = sep_2d_helper(x,
        dig1 = NA, dig2 = NA, sep = ", ",
        na_str = na_str, wrap = c("(", ")"),
        round_type = round_type
      ),
      "(xx., xx.)" = sep_2d_helper(x,
        dig1 = 0, dig2 = 0, sep = ", ",
        na_str = na_str, wrap = c("(", ")"),
        round_type = round_type
      ),
      "(xx.x, xx.x)" = sep_2d_helper(x,
        dig1 = 1, dig2 = 1, sep = ", ",
        na_str = na_str, wrap = c("(", ")"),
        round_type = round_type
      ),
      "(xx.xx, xx.xx)" = sep_2d_helper(x,
        dig1 = 2, dig2 = 2, sep = ", ",
        na_str = na_str, wrap = c("(", ")"),
        round_type = round_type
      ),
      "(xx.xxx, xx.xxx)" = sep_2d_helper(x,
        dig1 = 3, dig2 = 3, sep = ", ",
        na_str = na_str, wrap = c("(", ")"),
        round_type = round_type
      ),
      "(xx.xxxx, xx.xxxx)" = sep_2d_helper(x,
        dig1 = 4, dig2 = 4, sep = ", ",
        na_str = na_str, wrap = c("(", ")"),
        round_type = round_type
      ),
      "xx - xx" = sep_2d_helper(x, dig1 = NA, dig2 = NA, sep = " - ", na_str = na_str, round_type = round_type),
      "xx.x - xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = " - ", na_str = na_str, round_type = round_type),
      "xx.xx - xx.xx" = sep_2d_helper(x, dig1 = 2, dig2 = 2, sep = " - ", na_str = na_str, round_type = round_type),
      "xx (xx)" = val_pct_helper(x, dig1 = NA, dig2 = NA, na_str = na_str, pct = FALSE, round_type = round_type),
      "xx. (xx.)" = val_pct_helper(x, dig1 = 0, dig2 = 0, na_str = na_str, pct = FALSE, round_type = round_type),
      "xx.x (xx.x)" = val_pct_helper(x, dig1 = 1, dig2 = 1, na_str = na_str, pct = FALSE, round_type = round_type),
      "xx.xx (xx.xx)" = val_pct_helper(x, dig1 = 2, dig2 = 2, na_str = na_str, pct = FALSE, round_type = round_type),
      "xx (xx.)" = val_pct_helper(x, dig1 = NA, dig2 = 0, na_str = na_str, pct = FALSE, round_type = round_type),
      "xx (xx.x)" = val_pct_helper(x, dig1 = NA, dig2 = 1, na_str = na_str, pct = FALSE, round_type = round_type),
      "xx (xx.xx)" = val_pct_helper(x, dig1 = NA, dig2 = 2, na_str = na_str, pct = FALSE, round_type = round_type),
      "xx.x, xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = ", ", na_str = na_str, round_type = round_type),
      "xx.x to xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = " to ", na_str = na_str, round_type = round_type),
      "xx.xx (xx.xx - xx.xx)" = paste0(
        round_fmt(x[1], digits = 2, na_str = na_str[1]), " ",
        sep_2d_helper(x[2:3],
          dig1 = 2, dig2 = 2,
          sep = " - ", na_str = na_str[2:3],
          wrap = c("(", ")"),
          round_type = round_type
        )
      ),
      "xx. (xx. - xx.)" = paste0(
        round_fmt(x[1], digits = 0, na_str = na_str[1]), " ",
        sep_2d_helper(x[2:3],
          dig1 = 0, dig2 = 0,
          sep = " - ", na_str = na_str[2:3],
          wrap = c("(", ")"),
          round_type = round_type
        )
      ),
      "xx.x (xx.x - xx.x)" = paste0(
        round_fmt(x[1], digits = 1, na_str = na_str[1]), " ",
        sep_2d_helper(x[2:3],
          dig1 = 1, dig2 = 1,
          sep = " - ", na_str = na_str[2:3],
          wrap = c("(", ")"),
          round_type = round_type
        )
      ),
      "xx.xxx (xx.xxx - xx.xxx)" = paste0(
        round_fmt(x[1], digits = 3, na_str = na_str[1]), " ",
        sep_2d_helper(x[2:3],
          dig1 = 3, dig2 = 3,
          sep = " - ", na_str = na_str[2:3],
          wrap = c("(", ")"),
          round_type = round_type
        )
      ),
      paste("format string", format, "not found")
    )
  }
  # Check that probably never happens as it is almost always already text
  txt[is.na(txt)] <- na_str[1]


  if (output == "ascii") {
    txt
  } else if (output == "html") {
    ## convert to tagList
    ## convert \n to <br/>

    if (identical(txt, "")) {
      txt
    } else {
      els <- unlist(strsplit(txt, "\n", fixed = TRUE))
      Map(function(el, is.last) {
        tagList(el, if (!is.last) tags$br() else NULL)
      }, els, c(rep(FALSE, length(els) - 1), TRUE))
    }
  } else {
    txt
  }
}

setClassUnion("FormatSpec", c("NULL", "character", "function", "list"))
setClassUnion("characterOrNULL", c("NULL", "character"))
setClass("fmt_config",
  slots = c(
    format = "FormatSpec",
    format_na_str = "characterOrNULL",
    align = "characterOrNULL"
  )
)

#' Format configuration
#'
#' @param format (`string` or `function`)\cr a format label (string) or formatter function.
#' @param na_str (`string`)\cr string that should be displayed in place of missing values.
#' @param align (`string`)\cr alignment values should be rendered with.
#'
#' @return An object of class `fmt_config` which contains the following elements:
#'   * `format`
#'   * `na_str`
#'   * `align`
#'
#' @examples
#' fmt_config(format = "xx.xx", na_str = "-", align = "left")
#' fmt_config(format = "xx.xx - xx.xx", align = "right")
#'
#' @export
fmt_config <- function(format = NULL, na_str = "NA", align = "center") {
  new("fmt_config", format = format, format_na_str = na_str, align = align)
}
