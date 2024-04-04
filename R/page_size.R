
#' @import grid
#' @import grDevices
NULL
## https://www.ietf.org/rfc/rfc0678.txt


times_font_name <- function() {
    plat <- .Platform$OS.type
    switch(plat,
           "unix"  = "Times",
           "windows" = "Times New Roman")
}


#' Font size specification
#'
#' @param font_family (`character(1)`)\cr font family to use during
#'   string width and lines-per-page calculations. You can specify
#'   "Times New Roman" as "Times" or "serif", regardless of OS.
#'   Beyond that, see `family` entry in [graphics::par()]
#'   for details.
#' @param font_size (`numeric(1)`)\cr font size to use during string width
#'   calculations and lines-per-page calculations.
#' @param lineheight (`numeric(1)`)\cr line height to use during
#'   lines-per-page calculations.
#'
#' @details Passing the output of this constructor
#' to the rendering or pagination machinery defines
#' a font for use when calculating word wrapping and pagination.
#'
#' @note Specifying font in this way to, e.g., [export_as_txt()] or
#' [toString()] will not affect the font size of the output, as these
#' are both raw text formats. [export_as_pdf()] will use the specified font.
#'
#' @seealso [nchar_ttype()], [toString()], [`pagination_algo`], [export_as_pdf()]
#'
#' @examples
#' fspec <- font_spec("Courier", 8, 1)
#'
#' lets <- paste(letters, collapse = "")
#'
#' nchar_ttype(lets, fspec)
#'
#' fspec2 <- font_spec("Times", 8, 1)
#'
#' nchar_ttype(lets, fspec2)
#'
#' @export
font_spec <- function(font_family = "Courier",
                      font_size = 8,
                      lineheight = 1) {
    if(font_family %in% c("Times New Roman", "Times", "serif"))
        font_family <- times_font_name()
    structure(list(family = font_family,
         size = font_size,
         lineheight = lineheight),
         class = c("font_spec", "list"))

}
std_cpi <- 10L
std_lpi <- 6L

std_full_pg_wd_in <- 8.5

std_full_pg_ht_in <- 11

std_log_pg_wd_chars <- 72

std_log_pg_ht_lines <- 60

std_marg_ht <- round((std_full_pg_ht_in - std_log_pg_ht_lines / std_lpi) / 2, 2)
std_marg_wd <- round((std_full_pg_wd_in - std_log_pg_wd_chars / std_cpi) / 2, 2)

std_margins <- list(
  top = std_marg_ht,
  bottom = std_marg_ht,
  left = std_marg_wd,
  right = std_marg_wd
)

## does not appear to be used anywhere
## to_inches_num <- function(x) {
##   if (is(x, "unit")) {
##     x <- unclass(convertUnit(x, "inches"))
##   }
##   x
## }

## Physical size, does not take margins into account
pg_dim_names <- list(
  letter = c(8.5, 11),
  a4 = c(8.27, 11.69),
  legal = c(8.5, 14)
)

#' Supported named page types
#'
#' List supported named page types.
#'
#' @return
#' * `page_types` returns a character vector of supported page types
#' * `page_dim` returns the dimensions (width, then height) of the selected page type.
#'
#' @export
#' @examples
#' page_types()
#' page_dim("a4")
page_types <- function() {
  names(pg_dim_names)
}

#' @param page_type (`string`)\cr the name of a page size specification. Call
#'   [page_types()] for supported values.
#'
#' @export
#' @rdname page_types
page_dim <- function(page_type) {
  if (is.null(page_type)) {
    return(NULL)
  }
  if (!page_type %in% page_types()) {
    stop("Unrecognized page-size specification: ", page_type)
  }
  pg_dim_names[[page_type]]
}

ascii_chars <- vapply(33:126, function(n) rawToChar(as.raw(n)), "")

#' Calculate lines per inch and characters per inch for font
#'
#' @inheritParams page_lcpp
#'
#' @details
#' This function opens a PDF graphics device, writes to a temporary file, then
#' utilizes [grid::convertWidth()] and [grid::convertHeight()] to calculate lines
#' per inch and characters per inch for the specified font family, size, and
#' line height.
#'
#' An error is thrown if the font is not monospaced (determined by comparing
#' the effective widths of the `M` and `.` glyphs).
#'
#' @return A named list with `cpi` and `lpi`, the characters and lines per
#' inch, respectively.
#'
#' @examples
#' font_lcpi <- getFromNamespace("font_lcpi", "formatters")
#'
#' font_lcpi()
#' font_lcpi(font_size = 8)
#' font_lcpi(font_size = 8, lineheight = 1.1)
#'
#' @keywords internal
font_lcpi <- function(font_family = "Courier", font_size = 8, lineheight = 1, fontspec = font_spec(font_family, font_size, lineheight)) {
  new_dev <- open_font_dev(fontspec)
  if(new_dev)
      on.exit(close_font_dev())
  list(
    cpi = 1 / convertWidth(unit(1, "strwidth", " "), "inches", valueOnly = TRUE),
    lpi = convertHeight(unit(1, "inches"), "lines", valueOnly = TRUE)
  )
}

marg_order <- c("bottom", "left", "top", "right")

#' Determine lines per page (LPP) and characters per page (CPP) based on font and page type
#'
#' @inheritParams open_font_dev
#' @param page_type (`string`)\cr name of a page type. See [`page_types`]. Ignored
#'   when `pg_width` and `pg_height` are set directly.
#' @param landscape (`flag`)\cr whether the dimensions of `page_type` should be
#'   inverted for landscape orientation. Defaults to `FALSE`, ignored when `pg_width` and
#'   `pg_height` are set directly.
#' @param font_family (`string`)\cr name of a font family. An error will be thrown
#'   if the family named is not monospaced. Defaults to `"Courier"`.
#' @param font_size (`numeric(1)`)\cr font size. Defaults to `12`.
#' @param lineheight (`numeric(1)`)\cr line height. Defaults to `1`.
#' @param margins (`numeric(4)`)\cr named numeric vector containing `"bottom"`, `"left"`,
#'   `"top"`, and `"right"` margins in inches. Defaults to `.5` inches for both vertical
#'   margins and `.75` for both horizontal margins.
#' @param pg_width (`numeric(1)`)\cr page width in inches.
#' @param pg_height (`numeric(1)`)\cr page height in inches.
#'
#' @return A named list containing LPP (lines per page) and CPP (characters per page)
#'   elements suitable for use by the pagination machinery.
#'
#' @examples
#' page_lcpp()
#' page_lcpp(font_size = 10)
#' page_lcpp("a4", font_size = 10)
#'
#' page_lcpp(margins = c(top = 1, bottom = 1, left = 1, right = 1))
#' page_lcpp(pg_width = 10, pg_height = 15)
#'
#' @export
page_lcpp <- function(page_type = page_types(),
                      landscape = FALSE,
                      font_family = "Courier",
                      font_size = 8,
                      lineheight = 1,
                      margins = c(top = .5, bottom = .5, left = .75, right = .75),
                      pg_width = NULL,
                      pg_height = NULL,
                      fontspec = font_spec(font_family, font_size, lineheight)) {
  if (is.null(page_type)) {
    page_type <- page_types()[1]
  } else {
    page_type <- match.arg(page_type)
  }

  if (is.null(names(margins))) {
    names(margins) <- marg_order
  } else {
    margins <- margins[marg_order]
  }
  if (any(is.na(margins))) {
    stop("margins argument must have names 'bottom', 'left', 'top' and 'right'.")
  }
  lcpi <- font_lcpi(fontspec = fontspec)

  wdpos <- ifelse(landscape, 2, 1)
  pg_width <- pg_width %||% pg_dim_names[[page_type]][wdpos]
  pg_height <- pg_height %||% pg_dim_names[[page_type]][-wdpos]

  pg_width <- pg_width - sum(margins[c("left", "right")])
  pg_height <- pg_height - sum(margins[c("top", "bottom")])

  list(
    cpp = floor(lcpi[["cpi"]] * pg_width),
    lpp = floor(lcpi[["lpi"]] * pg_height)
  )
}



.open_fdev_is_monospace <- function() {
  if(!font_dev_state$open)
    stop(".open_fdev_is_monospace called when font dev state is not open. This shouldn't happen, please contact the maintainers.")
  font_dev_state$ismonospace
}




## safe wrapper around .open_fdev_is_monospace
is_monospace <- function(font_family = "Courier",
                         font_size = 8,
                         lineheight = 1,
                         fontspec = font_spec(font_family,
                                              font_size,
                                              lineheight)) {
    if(is.null(fontspec))
      return(TRUE)
    new_dev <- open_font_dev(fontspec)
    if(new_dev)
      on.exit(close_font_dev())
    .open_fdev_is_monospace()
}

## pg_types <- list(
##     "fsrp" = c(cpp = 110, lpp = 66),
##     "fsrp8" = c(cpp = 110, lpp = 66),
##     "fsrp7" = c(cpp = 110, lpp = 75),
##     "fsrl" = c(cpp = 149, lpp = 51),
##     "fsrl8" = c(cpp = 149, lpp = 51),
##     "fsrl7" = c(cpp = 150, lpp = 59),
##     "erp" = c(cpp = 96, lpp = 66),
##     "erp8" = c(cpp = 96, lpp = 66),
##     "erl" = c(cpp = 149, lpp = 45),
##     "erl8" = c(cpp = 149, lpp = 45),
##     "sasp" = c(cpp = 93, lpp = 73),
##     "sasp8" = c(cpp = 93, lpp = 73),
##     "sasl" = c(cpp = 134, lpp = 52),
##     "sasl8" = c(cpp = 134, lpp = 52),
##     "sasp7" = c(cpp = 107, lpp = 92),
##     "sasl7" = c(cpp = 154, lpp = 64),
##     "sasp6" = c(cpp = 125, lpp = 108),
##     "sasl6" = c(cpp = 180, lpp = 75),
##     "sasp10" = c(cpp = 78, lpp = 64),
##     "sasl10" = c(cpp = 108, lpp = 45),
##     "sasp9" = c(cpp = 87, lpp = 71),
##     "sasl9" = c(cpp = 120, lpp = 51),
##     "rapidp10" = c(cpp = 78, lpp = 64),
##     "rapidl10" = c(cpp = 108, lpp = 45),
##     "rapidp9" = c(cpp = 87, lpp = 71),
##     "rapidl9" = c(cpp = 120, lpp = 51),
##     "rapidp" = c(cpp = 93, lpp = 73),
##     "rapidp8" = c(cpp = 93, lpp = 73),
##     "rapidl" = c(cpp = 134, lpp = 52),
##     "rapidl8" = c(cpp = 134, lpp = 52),
##     "rapidp7" = c(cpp = 107, lpp = 92),
##     "rapidl7" = c(cpp = 154, lpp = 64),
##     "rapidp6" = c(cpp = 125, lpp = 108),
##     "rapidl6" = c(cpp = 180, lpp = 75),
##     "shibal" = c(cpp = 170, lpp = 48),
##     "shibal10" = c(cpp = 137, lpp = 39),
##     "shibal8" = c(cpp = 170, lpp = 48),
##     "shibal7" = c(cpp = 194, lpp = 56),
##     "shibal6" = c(cpp = 225, lpp = 65),
##     "shibap" = c(cpp = 112, lpp = 78),
##     "shibap10" = c(cpp = 89, lpp = 64),
##     "shibap8" = c(cpp = 112, lpp = 78),
##     "shibap7" = c(cpp = 127, lpp = 92),
##     "shibap6" = c(cpp = 148, lpp = 108))

##     ~courier_size,   ~cpi,                                      ~lpi,
##      6,              floor(129 / pg_dim_names[["letter"]][1]),  floor(85 / pg_dim_names[["letter"]][2]),
##      7,              floor(110 / pg_dim_names[["letter"]][1]),  floor(76 / pg_dim_names[["letter"]][2]),
##      8,              floor(95 / pg_dim_names[["letter"]][1]),   floor(68 / pg_dim_names[["letter"]][2]),
##      9,              floor(84 / pg_dim_names[["letter"]][1]),   floor(61 / pg_dim_names[["letter"]][2]),
##     10,              floor(75 / pg_dim_names[["letter"]][1]),   floor(56 / pg_dim_names[["letter"]][2])
## )

## courier_lcpi <- function(size) {
##     grid.newpage()
##     gp <- gpar(fontfamily="Courier New", fontsize = size, lineheight = 1)
##     pushViewport(plotViewport( gp = gp))
##     list(cpi = round(1/convertWidth(unit(1, "strwidth", "h"), "inches", valueOnly = TRUE), 0),
##          lpi = round(convertHeight(unit(1, "inches"), "lines", valueOnly = TRUE), 0))
## }
