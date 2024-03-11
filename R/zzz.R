#' Default horizontal separator
#'
#' The default horizontal separator character which can be displayed in the current
#' charset for use in rendering table-like objects.
#'
#' @param hsep_char (`character(1)`)\cr character that will be set in the R environment
#'   options as the default horizontal separator. Must be a single character. Use
#'   `getOption("formatters_default_hsep")` to get its current value (`NULL` if not set).
#'
#' @return `unicode` 2014 (long dash for generating solid horizontal line) if in a
#'   locale that uses a UTF character set, otherwise an ASCII hyphen with a
#'   once-per-session warning.
#'
#' @examples
#' default_hsep()
#' set_default_hsep("o")
#' default_hsep()
#'
#' @name default_horizontal_sep
#' @export
default_hsep <- function() {
  system_default_hsep <- getOption("formatters_default_hsep")

  if (is.null(system_default_hsep)) {
    if (any(grepl("^UTF", utils::localeToCharset()))) {
      hsep <- "\u2014"
    } else {
      if (interactive()) {
        warning(
          "Detected non-UTF charset. Falling back to '-' ",
          "as default header/body separator. This warning ",
          "will only be shown once per R session."
        ) # nocov
      } # nocov
      hsep <- "-" # nocov
    }
  } else {
    hsep <- system_default_hsep
  }
  hsep
}

#' @name default_horizontal_sep
#' @export
set_default_hsep <- function(hsep_char) {
  checkmate::assert_string(hsep_char, n.chars = 1, null.ok = TRUE)
  options("formatters_default_hsep" = hsep_char)
}

#' Default page number format
#'
#' If set, the default page number string will appear on the bottom right of
#' every page of a paginated table. The current `cpp` is used to position the string.
#'
#' @param page_number (`character(1)`)\cr single string value to set the page number format.
#'   It should be formatted similarly to the following format: `"page {i}/{n}"`.
#'   `{i}` will be replaced with the current page number, and `{n}` will be replaced with the
#'   total page number. Current `cpp` is used to position the string in the bottom right corner.
#'
#' @return The page number format string (`NULL` if not set).
#'
#' @examples
#' default_page_number()
#' set_default_page_number("page {i} of {n}")
#' default_page_number()
#'
#' @name default_page_number
#' @export
default_page_number <- function() {
  getOption("formatter_default_page_number", default = NULL)
}

#' @name default_page_number
#' @export
set_default_page_number <- function(page_number) {
  checkmate::assert_string(page_number, null.ok = TRUE)
  options("formatter_default_page_number" = page_number)
}
