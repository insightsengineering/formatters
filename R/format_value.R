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
#' @param x (`character(1)` or `function`)\cr format string or an object returned by [sprintf_format()]
#' @param stop_otherwise (`logical(1)`)\cr whether an error should be thrown if `x` is not a valid format.
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
#'  * `check_aligns` returns `TRUE` if the provided alignments are supported, otherwise an error is thrown.
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
#' @param format (`character(1)`)\cr a format string passed to [sprintf()].
#'
#' @return A formatting function which wraps and will apply the specified `sprintf`-style format
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
#' @param na_str (`character(1)`)\cr the value to return if `x` is `NA`.
#'
#' @details
#' This function combines the rounding behavior of R's standards-compliant [round()]
#' function (see the Details section of that documentation) with the strict decimal display
#' of [sprintf()]. The exact behavior is as follows:
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
#' due to `round`'s stricter adherence to the `IEC 60559` standard, particularly for R versions > 4.0.0 (see
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
#'
#' @export
#' @aliases rounding
round_fmt <- function(x, digits, na_str = "NA") {
  if (!is.na(digits) && digits < 0) {
    stop("round_fmt currentlyd does not support non-missing values of digits <0")
  }
  if (is.na(x)) {
    na_str
  } else if (is.na(digits)) {
    paste0(x)
  } else {
    sprfmt <- paste0("%.", digits, "f")
    sprintf(fmt = sprfmt, round(x, digits = digits))
  }
}

val_pct_helper <- function(x, dig1, dig2, na_str, pct = TRUE) {
  if (pct) {
    x[2] <- x[2] * 100
  }
  if (length(na_str) == 1) {
    na_str <- rep(na_str, 2)
  }
  paste0(
    round_fmt(x[1], digits = dig1, na_str = na_str[1]),
    " (",
    round_fmt(x[2], digits = dig2, na_str = na_str[2]),
    if (pct) "%", ")"
  )
}

sep_2d_helper <- function(x, dig1, dig2, sep, na_str, wrap = NULL) {
  ret <- paste(mapply(round_fmt, x = x, digits = c(dig1, dig2), na_str = na_str),
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
#' @param x (`any`)\cr the value to be formatted.
#' @param format (`character(1)` or `function`)\cr the format label (string) or formatter function to
#'   apply to `x`.
#' @param na_str (`character(1)`)\cr string to display when the value of `x` is missing. Defaults to `"NA"`.
#' @param output (`character(1)`)\cr output type.
#'
#' @details A length-zero value for `na_str` will be interpreted as `"NA"`, as will any
#' missing values within a non-length-zero `na_str` vector.
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
#' @export
format_value <- function(x, format = NULL, output = c("ascii", "html"), na_str = "NA") {
  ## if(is(x, "CellValue"))
  ##     x = x[[1]]

  if (length(x) == 0) {
    return("")
  }

  output <- match.arg(output)
  if (length(na_str) == 0) {
    na_str <- "NA"
  }
  if (any(is.na(na_str))) {
    na_str[is.na(na_str)] <- "NA"
  }
  ## format <- if (!missing(format)) format else obj_format(x)

  txt <- if (all(is.na(x)) && length(na_str) == 1L) {
    na_str
  } else if (is.null(format)) {
    toString(x)
  } else if (is.function(format)) {
    format(x, output = output)
  } else if (is.character(format)) {
    l <- if (format %in% formats_1d) {
      1
    } else if (format %in% formats_2d) {
      2
    } else if (format %in% formats_3d) {
      3
    } else {
      stop(
        "unknown format label: ", format,
        ". use list_valid_format_labels() to get a list of all formats"
      )
    }
    if (format != "xx" && length(x) != l) {
      stop(
        "cell <", paste(x), "> and format ",
        format, " are of different length"
      )
    }
    if (length(na_str) < sum(is.na(x))) {
      na_str <- rep(na_str, length.out = sum(is.na(x)))
    }
    switch(format,
      "xx" = as.character(x),
      "xx." = round_fmt(x, digits = 0, na_str = na_str),
      "xx.x" = round_fmt(x, digits = 1, na_str = na_str),
      "xx.xx" = round_fmt(x, digits = 2, na_str = na_str),
      "xx.xxx" = round_fmt(x, digits = 3, na_str = na_str),
      "xx.xxxx" = round_fmt(x, digits = 4, na_str = na_str),
      "xx%" = paste0(round_fmt(x * 100, digits = NA, na_str = na_str), "%"),
      "xx.%" = paste0(round_fmt(x * 100, digits = 0, na_str = na_str), "%"),
      "xx.x%" = paste0(round_fmt(x * 100, digits = 1, na_str = na_str), "%"),
      "xx.xx%" = paste0(round_fmt(x * 100, digits = 2, na_str = na_str), "%"),
      "xx.xxx%" = paste0(round_fmt(x * 100, digits = 3, na_str = na_str), "%"),
      "(N=xx)" = paste0("(N=", round_fmt(x, digits = NA, na_str = na_str), ")"),
      "N=xx" = paste0("N=", round_fmt(x, digits = NA, na_str = na_str)),
      ">999.9" = ifelse(x > 999.9, ">999.9", round_fmt(x, digits = 1, na_str = na_str)),
      ">999.99" = ifelse(x > 999.99, ">999.99", round_fmt(x, digits = 2, na_str = na_str)),
      "x.xxxx | (<0.0001)" = ifelse(x < 0.0001, "<0.0001", round_fmt(x, digits = 4, na_str = na_str)),
      "xx / xx" = sep_2d_helper(x, dig1 = NA, dig2 = NA, sep = " / ", na_str = na_str),
      "xx. / xx." = sep_2d_helper(x, dig1 = 0, dig2 = 0, sep = " / ", na_str = na_str),
      "xx.x / xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = " / ", na_str = na_str),
      "xx.xx / xx.xx" = sep_2d_helper(x, dig1 = 2, dig2 = 2, sep = " / ", na_str = na_str),
      "xx.xxx / xx.xxx" = sep_2d_helper(x, dig1 = 3, dig2 = 3, sep = " / ", na_str = na_str),
      "N=xx (xx%)" = paste0("N=", val_pct_helper(x, dig1 = NA, dig2 = NA, na_str = na_str)),
      "xx (xx%)" = val_pct_helper(x, dig1 = NA, dig2 = NA, na_str = na_str),
      "xx (xx.%)" = val_pct_helper(x, dig1 = NA, dig2 = 0, na_str = na_str),
      "xx (xx.x%)" = val_pct_helper(x, dig1 = NA, dig2 = 1, na_str = na_str),
      "xx (xx.xx%)" = val_pct_helper(x, dig1 = NA, dig2 = 2, na_str = na_str),
      "xx. (xx.%)" = val_pct_helper(x, dig1 = 0, dig2 = 0, na_str = na_str),
      "xx.x (xx.x%)" = val_pct_helper(x, dig1 = 1, dig2 = 1, na_str = na_str),
      "xx.xx (xx.xx%)" = val_pct_helper(x, dig1 = 2, dig2 = 2, na_str = na_str),
      "(xx, xx)" = sep_2d_helper(x,
        dig1 = NA, dig2 = NA, sep = ", ",
        na_str = na_str, wrap = c("(", ")")
      ),
      "(xx., xx.)" = sep_2d_helper(x,
        dig1 = 0, dig2 = 0, sep = ", ",
        na_str = na_str, wrap = c("(", ")")
      ),
      "(xx.x, xx.x)" = sep_2d_helper(x,
        dig1 = 1, dig2 = 1, sep = ", ",
        na_str = na_str, wrap = c("(", ")")
      ),
      "(xx.xx, xx.xx)" = sep_2d_helper(x,
        dig1 = 2, dig2 = 2, sep = ", ",
        na_str = na_str, wrap = c("(", ")")
      ),
      "(xx.xxx, xx.xxx)" = sep_2d_helper(x,
        dig1 = 3, dig2 = 3, sep = ", ",
        na_str = na_str, wrap = c("(", ")")
      ),
      "(xx.xxxx, xx.xxxx)" = sep_2d_helper(x,
        dig1 = 4, dig2 = 4, sep = ", ",
        na_str = na_str, wrap = c("(", ")")
      ),
      "xx - xx" = sep_2d_helper(x, dig1 = NA, dig2 = NA, sep = " - ", na_str = na_str),
      "xx.x - xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = " - ", na_str = na_str),
      "xx.xx - xx.xx" = sep_2d_helper(x, dig1 = 2, dig2 = 2, sep = " - ", na_str = na_str),
      "xx (xx)" = val_pct_helper(x, dig1 = NA, dig2 = NA, na_str = na_str, pct = FALSE),
      "xx. (xx.)" = val_pct_helper(x, dig1 = 0, dig2 = 0, na_str = na_str, pct = FALSE),
      "xx.x (xx.x)" = val_pct_helper(x, dig1 = 1, dig2 = 1, na_str = na_str, pct = FALSE),
      "xx.xx (xx.xx)" = val_pct_helper(x, dig1 = 2, dig2 = 2, na_str = na_str, pct = FALSE),
      "xx (xx.)" = val_pct_helper(x, dig1 = NA, dig2 = 0, na_str = na_str, pct = FALSE),
      "xx (xx.x)" = val_pct_helper(x, dig1 = NA, dig2 = 1, na_str = na_str, pct = FALSE),
      "xx (xx.xx)" = val_pct_helper(x, dig1 = NA, dig2 = 2, na_str = na_str, pct = FALSE),
      "xx.x, xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = ", ", na_str = na_str),
      "xx.x to xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = " to ", na_str = na_str),
      "xx.xx (xx.xx - xx.xx)" = paste0(
        round_fmt(x[1], digits = 2, na_str = na_str[1]), " ",
        sep_2d_helper(x[2:3],
          dig1 = 2, dig2 = 2,
          sep = " - ", na_str = na_str[2:3],
          wrap = c("(", ")")
        )
      ),
      "xx. (xx. - xx.)" = paste0(
        round_fmt(x[1], digits = 0, na_str = na_str[1]), " ",
        sep_2d_helper(x[2:3],
          dig1 = 0, dig2 = 0,
          sep = " - ", na_str = na_str[2:3],
          wrap = c("(", ")")
        )
      ),
      "xx.x (xx.x - xx.x)" = paste0(
        round_fmt(x[1], digits = 1, na_str = na_str[1]), " ",
        sep_2d_helper(x[2:3],
          dig1 = 1, dig2 = 1,
          sep = " - ", na_str = na_str[2:3],
          wrap = c("(", ")")
        )
      ),
      "xx.xxx (xx.xxx - xx.xxx)" = paste0(
        round_fmt(x[1], digits = 3, na_str = na_str[1]), " ",
        sep_2d_helper(x[2:3],
          dig1 = 3, dig2 = 3,
          sep = " - ", na_str = na_str[2:3],
          wrap = c("(", ")")
        )
      ),
      paste("format string", format, "not found")
    )
  }
  txt[is.na(txt)] <- na_str
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
#' @param format (`character(1)` or `function`)\cr a format label (string) or formatter function.
#' @param na_str (`character(1)`)\cr string that should be displayed in place of missing values.
#' @param align (`character(1)`)\cr alignment values should be rendered with.
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
