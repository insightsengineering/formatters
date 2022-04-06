
#' @importFrom htmltools tags tagList

formats_1d <- c(
  "xx", "xx.", "xx.x", "xx.xx", "xx.xxx", "xx.xxxx",
  "xx%", "xx.%", "xx.x%", "xx.xx%", "xx.xxx%", "(N=xx)", ">999.9", ">999.99",
  "x.xxxx | (<0.0001)"
)

formats_2d <- c(
  "xx / xx", "xx. / xx.", "xx.x / xx.x", "xx.xx / xx.xx", "xx.xxx / xx.xxx",
  "xx (xx%)", "xx (xx.%)", "xx (xx.x%)", "xx (xx.xx%)",
  "xx. (xx.%)", "xx.x (xx.x%)", "xx.xx (xx.xx%)",
  "(xx, xx)", "(xx., xx.)", "(xx.x, xx.x)", "(xx.xx, xx.xx)",
  "(xx.xxx, xx.xxx)", "(xx.xxxx, xx.xxxx)",
  "xx - xx", "xx.x - xx.x", "xx.xx - xx.xx",
  "xx.x (xx.x)", "xx.xx (xx.xx)",
  "xx.x, xx.x",
  "xx.x to xx.x"
)

formats_3d <- c(
  "xx.xx (xx.xx - xx.xx)"
)

#' List with currently support 'xx' style format labels grouped by 1d, 2d and 3d
#'
#' Currently valid format labels can not be added dynamically. Format functions
#' must be used for special cases
#'
#' @export
#' @return A nested list, with elements listing the supported 1d, 2d, and 3d format strings.
#' @examples
#'
#' list_valid_format_labels()
#'
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

#' Check if a format is supported
#'
#' @param x either format string or an object returned by \code{sprintf_format}
#' @param stop_otherwise logical, if \code{x} is not a format should an error be
#'   thrown
#' @note No check if the function is actually a formatter is performed.
#' @return \code{TRUE} if \code{x} is \code{NULL}, a supported format string, or a function; \code{FALSE} otherwise.
#'
#' @export
#'
#' @examples
#' is_valid_format("xx.x")
#' is_valid_format("fakeyfake")
is_valid_format <- function(x, stop_otherwise=FALSE) {
  is_valid <- is.null(x) ||
    (length(x) == 1 &&
       (is.function(x) ||
        x %in% unlist(list_valid_format_labels())))

  if (stop_otherwise && !is_valid)
    stop("format needs to be a format label, sprintf_format object, a function, or NULL")

  is_valid
}


#' Specify text format via a sprintf format string
#'
#'
#' @param format character(1). A format string passed to sprintf.
#'
#' @export
#' @return A formating function which wraps and will apply the specified \code{printf} style format string \code{format}.
#' @seealso \code{\link[base]{sprintf}}
#'
#' @examples
#'
#' fmtfun <- sprintf_format("(N=%i")
#' format_value(100, format = fmtfun)
#'
#' fmtfun2 <- sprintf_format("%.4f - %.2f")
#' format_value(list(12.23456, 2.724))
sprintf_format <- function(format) {
    function(x,...) {
        do.call(sprintf, c(list(fmt = format), x))
    }
}


#' Round and prepare a value for display
#'
#' This function is used within \code{\link{format_value}} to prepare numeric values within
#' cells for formatting and display.
#'
#' @aliases rounding
#' @param x numeric(1). Value to format
#' @param digits numeric(1). Number of digits to round to, or \code{NA} to convert to a
#' character value with no rounding.
#' @param na_str character(1). The value to return if \code{x} is \code{NA}.
#'
#' @details
#' This function combines the rounding behavior of R's standards-complaint
#' \code{\link{round}} function (see the Details section of that documentation)
#' with the strict decimal display of \code{\link{sprintf}}. The exact behavior
#' is as follows:
#'
#' \enumerate{
#' \item{If \code{x} is NA, the value of \code{na_str} is returned}
#' \item{If \code{x} is non-NA but \code{digits} is NA, \code{x} is converted to a character
#' and returned}
#' \item{If \code{x} and \code{digits} are both non-NA, \code{round} is called first,
#' and then \code{sprintf} is used to convert the rounded value to a character with the
#' appropriate number of trailing zeros enforced.}
#' }
#'
#' @return A character value representing the value after rounding, containing
#' containing any trailling zeros required to display \emph{exactly} \code{digits}
#' elements.
#' @note
#' This differs from the base R \code{\link{round}} function in that \code{NA}
#' digits indicate x should be passed converted to character and returned unchanged
#' whereas \code{round(x, digits =NA)} returns \code{NA} for all values of \code{x}.
#'
#' This behavior will differ from \code{as.character(round(x, digits = digits))}
#' in the case where there are not at least \code{digits} significant digits
#' after the decimal that remain after rounding. It \emph{may} differ from
#' \code{sprintf("\%.Nf", x)} for values ending in \code{5} after the decimal place
#' on many popular operating systems due to \code{round}'s stricter adherence to the
#' IEC 60559 standard, particularly for R versions > 4.0.0 (see Warning in \code{\link[base:round]{round}}
#' documentation).
#'
#' @export
#' @seealso \code{link{format_value}} \code{\link[base:round]{round}} \code{\link[base:sprintf]{sprintf}}
#' @examples
#'
#' round_fmt(0, digits = 3)
#' round_fmt(.395, digits = 2)
#' round_fmt(NA, digits = 1)
#' round_fmt(NA, digits = 1, na_str = "-")
#' round_fmt(2.765923, digits = NA)
round_fmt <- function(x, digits, na_str = "NA") {
    if(!is.na(digits) && digits < 0)
        stop("round_fmt currentlyd does not support non-missing values of digits <0")
    if(is.na(x)) {
        na_str
    } else if(is.na(digits)) {
        paste0(x)
    } else {
        sprfmt <- paste0("%.", digits, "f")
        sprintf(fmt = sprfmt, round(x, digits = digits))
    }
}



val_pct_helper <- function(x, dig1, dig2, na_str, pct = TRUE) {
    if(pct)
        x[2] <- x[2] * 100
    paste0(round_fmt(x[1], digits = dig1, na_str = na_str),
           " (",
           round_fmt(x[2], digits = dig2, na_str = na_str),
           if(pct) "%", ")")
}

sep_2d_helper <- function(x, dig1, dig2, sep, na_str, wrap = NULL) {

    ret <- paste(mapply(round_fmt, x = x, digits = c(dig1, dig2), na_str = na_str),
                 collapse = sep)
    if(!is.null(wrap))
        ret <- paste(c(wrap[1], ret, wrap[2]), collapse = "")
    ret
}

## na_or_round <- function(x, digits, na_str) {
##     if(is.na(x))
##         na_str
##     else
##         round(x, digits = digits)

## }

#' Converts a (possibly compound) value into a string using the \code{format} information
#'
#' @param x ANY. The value to be formatted
#' @param format character(1) or function. The format label (string) or formatter function to apply to \code{x}.
#' @param na_str character(1). String that should be displayed when the value of \code{x} is missing. Defaults to \code{"NA"}.
#' @param output character(1). output type
#'
#' @return formatted text representing the cell \code{x}.
#' @export
#'
#' @seealso [round_fmt()]
#' @examples
#'
#' x <- format_value(pi, format = "xx.xx")
#' x
#'
#' format_value(x, output = "ascii")
#'
format_value <- function(x, format = NULL, output = c("ascii", "html"), na_str = "NA") {
    ## if(is(x, "CellValue"))
    ##     x = x[[1]]

    if (length(x) == 0) return("")

    output <- match.arg(output)
   ## format <- if (!missing(format)) format else obj_format(x)


    txt <- if (is.null(format)) {
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
                      stop("unknown format label: ", format, ". use list_valid_format_labels() to get a list of all formats")
                  }
               if (format != "xx" && length(x) != l) stop("cell <", paste(x), "> and format ", format, " are of different length")

               switch(
                   format,
                   "xx" = if (all(is.na(x))) "NA" else as.character(x),
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
                   ">999.9" = ifelse(x > 999.9, ">999.9", round_fmt(x, digits = 1, na_str = na_str)),
                   ">999.99" = ifelse(x > 999.99, ">999.99", round_fmt(x, digits = 2, na_str = na_str)),
                   "x.xxxx | (<0.0001)" = ifelse(x < 0.0001, "<0.0001", round_fmt(x, digits = 4, na_str = na_str)),
                   "xx / xx" = sep_2d_helper(x, dig1 = NA, dig2 = NA, sep = " / ", na_str = na_str),
                   "xx. / xx." = sep_2d_helper(x, dig1 = 0, dig2 = 0, sep = " / ", na_str = na_str),
                   "xx.x / xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = " / ", na_str = na_str),
                   "xx.xx / xx.xx" = sep_2d_helper(x, dig1 = 2, dig2 = 2, sep = " / ", na_str = na_str),
                   "xx.xxx / xx.xxx" = sep_2d_helper(x, dig1 = 3, dig2 = 3, sep = " / ", na_str = na_str),
                   "xx (xx%)" = val_pct_helper(x, dig1 = NA, dig2 = NA, na_str = na_str),
                   "xx (xx.%)" = val_pct_helper(x, dig1 = NA, dig2 = 0, na_str = na_str),
                   "xx (xx.x%)" = val_pct_helper(x, dig1 = NA, dig2 = 1, na_str = na_str),
                   "xx (xx.xx%)" = val_pct_helper(x, dig1 = NA, dig2 = 2, na_str = na_str),
                   "xx. (xx.%)" = val_pct_helper(x, dig1 = 0, dig2 = 0, na_str = na_str),
                   "xx.x (xx.x%)" = val_pct_helper(x, dig1 = 1, dig2 = 1, na_str = na_str),
                   "xx.xx (xx.xx%)" = val_pct_helper(x, dig1 = 2, dig2 = 2, na_str = na_str),
                   "(xx, xx)" = sep_2d_helper(x, dig1 = NA, dig2 = NA, sep = ", ", na_str = na_str, wrap = c("(", ")")),
                   "(xx., xx.)" = sep_2d_helper(x, dig1 = 0, dig2 = 0, sep = ", ", na_str = na_str, wrap = c("(", ")")),
                   "(xx.x, xx.x)" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = ", ", na_str = na_str, wrap = c("(", ")")),
                   "(xx.xx, xx.xx)" = sep_2d_helper(x, dig1 = 2, dig2 = 2, sep = ", ", na_str = na_str, wrap = c("(", ")")),
                   "(xx.xxx, xx.xxx)" = sep_2d_helper(x, dig1 = 3, dig2 = 3, sep = ", ", na_str = na_str, wrap = c("(", ")")),
                   "(xx.xxxx, xx.xxxx)" = sep_2d_helper(x, dig1 = 4, dig2 = 4, sep = ", ", na_str = na_str, wrap = c("(", ")")),
                   "xx - xx" = sep_2d_helper(x, dig1 = NA, dig2 = NA, sep = " - ", na_str = na_str),
                   "xx.x - xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = " - ", na_str = na_str),
                   "xx.xx - xx.xx" = sep_2d_helper(x, dig1 = 2, dig2 = 2, sep = " - ", na_str = na_str),
                   "xx.x (xx.x)" = val_pct_helper(x, dig1 = 1, dig2 = 1, na_str = na_str, pct = FALSE),
                   "xx.xx (xx.xx)" = val_pct_helper(x, dig1 = 2, dig2 = 2, na_str = na_str, pct = FALSE),
                   "xx.x, xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = ", ", na_str = na_str),
                   "xx.x to xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = " to ", na_str = na_str),
                   "xx.xx (xx.xx - xx.xx)" = paste0(round_fmt(x[1], digits = 2, na_str = na_str), " ",
                                                    sep_2d_helper(x[2:3], dig1 = 2, dig2 = 2,
                                                                  sep = " - ", na_str = na_str,
                                                                  wrap = c("(", ")"))),
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
            }, els, c(rep(FALSE, length(els) -1), TRUE))
        }

    } else {
        txt
    }

}
