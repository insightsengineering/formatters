### This file defines the generics which make up the interface `formatters` offers.
### Defining methods for these generics for a new table-like class should be fully
### sufficient for hooking that class up to the `formatters` pagination and rendering
### machinery.

#' Make row layout summary data frames for use during pagination
#'
#' All relevant information about table rows (e.g. indentations) is summarized in a `data.frame`.
#' This function works **only** on `rtables` and `rlistings` objects, and not on their `print` counterparts
#' (like [`MatrixPrintForm`]).
#'
#' @inheritParams open_font_dev
#' @param tt (`ANY`)\cr object representing the table-like object to be summarized.
#' @param visible_only (`flag`)\cr should only visible aspects of the table structure be reflected
#'   in this summary. Defaults to `TRUE`. May not be supported by all methods.
#' @param incontent (`flag`)\cr internal detail, do not set manually.
#' @param repr_ext (`integer(1)`)\cr internal detail, do not set manually.
#' @param repr_inds (`integer`)\cr internal detail, do not set manually.
#' @param sibpos (`integer(1)`)\cr internal detail, do not set manually.
#' @param nsibs (`integer(1)`)\cr internal detail, do not set manually.
#' @param rownum (`numeric(1)`)\cr internal detail, do not set manually.
#' @param indent (`integer(1)`)\cr internal detail, do not set manually.
#' @param colwidths (`numeric`)\cr internal detail, do not set manually.
#' @param path (`character`)\cr path to the (sub)table represented by `tt`. Defaults to `character()`.
#' @param max_width (`numeric(1)` or `NULL`)\cr maximum width for title/footer materials.
#' @param col_gap (`numeric(1)`)\cr the gap to be assumed between columns, in number of spaces with
#'   font specified by `fontspec`.
#'
#' @import methods
#' @include matrix_form.R
#'
#' @details
#' When `visible_only` is `TRUE` (the default), methods should return a `data.frame` with exactly one
#' row per visible row in the table-like object. This is useful when reasoning about how a table will
#' print, but does not reflect the full pathing space of the structure (though the paths which are given
#' will all work as is).
#'
#' If supported, when `visible_only` is `FALSE`, every structural element of the table (in row-space)
#' will be reflected in the returned `data.frame`, meaning the full pathing-space will be represented
#' but some rows in the layout summary will not represent printed rows in the table as it is displayed.
#'
#' Most arguments beyond `tt` and `visible_only` are present so that `make_row_df` methods can call
#' `make_row_df` recursively and retain information, and should not be set during a top-level call.
#'
#' @return A `data.frame` of row/column-structure information used by the pagination machinery.
#'
#' @note The technically present root tree node is excluded from the summary returned by
#'   both `make_row_df` and `make_col_df` (see [rtables::make_col_df()]), as it is simply the
#'   row/column structure of `tt` and thus not useful for pathing or pagination.
#'
#' @examples
#' # Expected error with matrix_form. For real case examples consult {rtables} documentation
#' mf <- basic_matrix_form(iris)
#' # make_row_df(mf) # Use table obj instead
#'
#' @export
#' @name make_row_df
setGeneric("make_row_df", function(tt, colwidths = NULL, visible_only = TRUE,
                                   rownum = 0,
                                   indent = 0L,
                                   path = character(),
                                   incontent = FALSE,
                                   repr_ext = 0L,
                                   repr_inds = integer(),
                                   sibpos = NA_integer_,
                                   nsibs = NA_integer_,
                                   max_width = NULL,
                                   fontspec = font_spec(),
                                   col_gap = 3L) {
  standardGeneric("make_row_df")
})

#' @rdname make_row_df
setMethod("make_row_df", "MatrixPrintForm", function(tt, colwidths = NULL, visible_only = TRUE,
                                                     rownum = 0,
                                                     indent = 0L,
                                                     path = character(),
                                                     incontent = FALSE,
                                                     repr_ext = 0L,
                                                     repr_inds = integer(),
                                                     sibpos = NA_integer_,
                                                     nsibs = NA_integer_,
                                                     max_width = NULL,
                                                     fontspec = font_spec(),
                                                     col_gap = mf_colgap(tt) %||% 3L) {
  msg <- paste0(
    "make_row_df can be used only on {rtables} table objects, and not on `matrix_form`-",
    "generated objects (MatrixPrintForm)."
  )
  stop(msg)
})

#' Transform `rtable` to a list of matrices which can be used for outputting
#'
#' Although `rtable`s are represented as a tree data structure when outputting the table to ASCII or HTML,
#' it is useful to map the `rtable` to an in-between state with the formatted cells in a matrix form.
#'
#' @inheritParams make_row_df
#' @param obj (`ANY`)\cr object to be transformed into a ready-to-render form (a [`MatrixPrintForm`] object).
#' @param indent_rownames (`flag`)\cr if `TRUE`, the row names column in the `strings` matrix of `obj`
#'   will have indented row names (strings pre-fixed).
#' @param expand_newlines (`flag`)\cr whether the generated matrix form should expand rows whose values
#'   contain newlines into multiple 'physical' rows (as they will appear when rendered into ASCII). Defaults
#'   to `TRUE`.
#' @param indent_size (`numeric(1)`)\cr number of spaces to be used per level of indent (if supported by
#'   the relevant method). Defaults to 2.
#'
#' @return A [`MatrixPrintForm`] classed list with an additional `nrow_header` attribute indicating the
#'   number of pseudo "rows" the column structure defines, with the following elements:
#'   \describe{
#'     \item{`strings`}{The content, as it should be printed, of the top-left material, column headers, row
#'       labels, and cell values of `tt`.}
#'     \item{`spans`}{The column-span information for each print-string in the strings matrix.}
#'     \item{`aligns`}{The text alignment for each print-string in the strings matrix.}
#'     \item{`display`}{Whether each print-string in the strings matrix should be printed or not.}
#'     \item{`row_info`}{The `data.frame` generated by [basic_pagdf()].}
#'   }
#'
#' @export
setGeneric("matrix_form", function(obj,
                                   indent_rownames = FALSE,
                                   expand_newlines = TRUE,
                                   indent_size = 2,
                                   fontspec = NULL,
                                   col_gap = NULL) {
  standardGeneric("matrix_form")
})


#' @rdname matrix_form
#' @export
setMethod("matrix_form", "MatrixPrintForm", function(obj,
                                                     indent_rownames = FALSE,
                                                     expand_newlines = TRUE,
                                                     indent_size = 2,
                                                     fontspec = NULL,
                                                     col_gap = NULL) {
  if (!is.null(fontspec)) {
    mf_fontspec(obj) <- fontspec
  }
  if (!is.null(col_gap) && !isTRUE(all.equal(col_gap, mf_colgap(obj)))) {
    mf_colgap(obj) <- col_gap
  }
  obj
})

# Generics for `toString` and helper functions -----------------------------------------------------------

## this is where we will take word wrapping
## into account when it is added
##
## ALL calculations of vertical space for pagination
## purposes must go through nlines and divider_height!!!!!!!!

## this will be customizable someday. I have foreseen it (spooky noises)
#' Divider height
#'
#' @param obj (`ANY`)\cr object.
#'
#' @return The height, in lines of text, of the divider between header and body. Currently
#'   returns `1L` for the default method.
#'
#' @examples
#' divider_height(mtcars)
#'
#' @export
setGeneric("divider_height", function(obj) standardGeneric("divider_height"))

#' @rdname divider_height
#' @export
setMethod(
  "divider_height", "ANY",
  function(obj) 1L
)

# nlines ---------------------------------------------------------------

#' Number of lines required to print a value
#'
#' @inheritParams open_font_dev
#' @param x (`ANY`)\cr the object to be printed.
#' @param colwidths (`numeric`)\cr column widths (if necessary).
#' @param max_width (`numeric(1)`)\cr width that strings should be wrapped to when
#'   determining how many lines they require.
#' @param col_gap (`numeric(1)`)\cr width of gap between columns in number of spaces.
#'   Only used by methods which must calculate span widths after wrapping.
#'
#' @return The number of lines needed to render the object `x`.
#'
#' @export
setGeneric(
  "nlines",
  ## XXX TODO come back and add fontspec default value once not having
  ## it has found all the disconnection breakages
  function(x, colwidths = NULL, max_width = NULL, fontspec, col_gap = NULL) standardGeneric("nlines")
)

## XXX beware. I think it is dangerous
#' @export
#' @rdname nlines
setMethod(
  "nlines", "list",
  function(x, colwidths, max_width, fontspec, col_gap = NULL) {
    if (length(x) == 0) {
      0L
    } else {
      sum(unlist(vapply(x, nlines, NA_integer_, colwidths = colwidths, max_width = max_width, fontspec = fontspec)))
    }
  }
)

#' @export
#' @rdname nlines
setMethod("nlines", "NULL", function(x, colwidths, max_width, fontspec, col_gap = NULL) 0L)

#' @export
#' @rdname nlines
setMethod("nlines", "character", function(x, colwidths, max_width, fontspec, col_gap = NULL) {
  splstr <- strsplit(x, "\n", fixed = TRUE)
  if (length(x) == 0) {
    return(0L)
  } ## else if(is.null(colwidths) && is.null(max_width)) { ## don't need wrapping
  ##   return(length(splstr))
  ## }

  sum(vapply(splstr,
    function(xi, max_width) {
      if (length(xi) == 0) {
        1L
      } else if (length(max_width) == 0) { ## this happens with strsplit("", "\n")
        length(xi)
      } else {
        length(wrap_txt(xi, max_width, fontspec = fontspec))
      }
    }, 1L,
    max_width = max_width
  ))
})

#' Transform objects into string representations
#'
#' Transform a complex object into a string representation ready to be printed or written
#' to a plain-text file.
#'
#' @param x (`ANY`)\cr object to be prepared for rendering.
#' @param ... additional parameters passed to individual methods.
#'
#' @export
#' @rdname tostring
setGeneric("toString", function(x, ...) standardGeneric("toString"))

## preserve S3 behavior
setMethod("toString", "ANY", base::toString)

#' Print
#'
#' Print an R object. See [print()].
#'
#' @inheritParams base::print
#'
#' @rdname basemethods
setMethod("print", "ANY", base::print)

# General/"universal" property getter and setter generics and stubs --------------------------------------

#' Label, name, and format accessor generics
#'
#' Getters and setters for basic, relatively universal attributes of "table-like" objects.
#'
#' @param obj (`ANY`)\cr the object.
#' @param value (`string` or `FormatSpec`)\cr the new value of the attribute.
#'
#' @return The name, format, or label of `obj` for getters, or `obj` after modification for setters.
#'
#' @export
#' @name lab_name
#' @aliases obj_name

# obj_name ---------------------------------------------------------------

setGeneric("obj_name", function(obj) standardGeneric("obj_name"))

#' @rdname lab_name
#' @export
setGeneric("obj_name<-", function(obj, value) standardGeneric("obj_name<-"))

# obj_label ---------------------------------------------------------------

#' @seealso with_label
#' @rdname lab_name
#' @export
setGeneric("obj_label", function(obj) standardGeneric("obj_label"))

#' @rdname lab_name
#' @param value character(1). The new label
#' @export
setGeneric("obj_label<-", function(obj, value) standardGeneric("obj_label<-"))

#' @rdname lab_name
#' @exportMethod obj_label
setMethod("obj_label", "ANY", function(obj) attr(obj, "label"))

#' @rdname lab_name
#' @exportMethod obj_label<-
setMethod(
  "obj_label<-", "ANY",
  function(obj, value) {
    attr(obj, "label") <- value
    obj
  }
)

# obj_format ---------------------------------------------------------------

#' @rdname lab_name
#' @export
setGeneric("obj_format", function(obj) standardGeneric("obj_format"))

## this covers rcell, etc
#' @rdname lab_name
#' @exportMethod obj_format
setMethod("obj_format", "ANY", function(obj) attr(obj, "format", exact = TRUE))

#' @rdname lab_name
#' @export
setMethod("obj_format", "fmt_config", function(obj) obj@format)

#' @export
#' @rdname lab_name
setGeneric("obj_format<-", function(obj, value) standardGeneric("obj_format<-"))

## this covers rcell, etc
#' @exportMethod obj_format<-
#' @rdname lab_name
setMethod("obj_format<-", "ANY", function(obj, value) {
  attr(obj, "format") <- value
  obj
})

#' @rdname lab_name
#' @export
setMethod("obj_format<-", "fmt_config", function(obj, value) {
  obj@format <- value
  obj
})

# obj_na_str ---------------------------------------------------------------

#' @rdname lab_name
#' @export
setGeneric("obj_na_str", function(obj) standardGeneric("obj_na_str"))

#' @rdname lab_name
#' @exportMethod obj_na_str
setMethod("obj_na_str", "ANY", function(obj) attr(obj, "format_na_str", exact = TRUE))

#' @rdname lab_name
#' @export
setMethod("obj_na_str", "fmt_config", function(obj) obj@format_na_str)

#' @rdname lab_name
#' @export
setGeneric("obj_na_str<-", function(obj, value) standardGeneric("obj_na_str<-"))

#' @exportMethod obj_na_str<-
#' @rdname lab_name
setMethod("obj_na_str<-", "ANY", function(obj, value) {
  attr(obj, "format_na_str") <- value
  obj
})

#' @rdname lab_name
#' @export
setMethod("obj_na_str<-", "fmt_config", function(obj, value) {
  obj@format_na_str <- value
  obj
})

# obj_align ---------------------------------------------------------------

#' @rdname lab_name
#' @export
setGeneric("obj_align", function(obj) standardGeneric("obj_align"))

#' @rdname lab_name
#' @exportMethod obj_align
setMethod("obj_align", "ANY", function(obj) attr(obj, "align", exact = TRUE))

#' @rdname lab_name
#' @export
setMethod("obj_align", "fmt_config", function(obj) obj@align)

#' @rdname lab_name
#' @export
setGeneric("obj_align<-", function(obj, value) standardGeneric("obj_align<-"))

#' @exportMethod obj_align<-
#' @rdname lab_name
setMethod("obj_align<-", "ANY", function(obj, value) {
  attr(obj, "align") <- value
  obj
})

#' @rdname lab_name
#' @export
setMethod("obj_align<-", "fmt_config", function(obj, value) {
  obj@align <- value
  obj
})

# main_title ---------------------------------------------------------------

#' General title and footer accessors
#'
#' @param obj (`ANY`)\cr object to extract information from.
#'
#' @return A character scalar (`main_title`), character vector (`main_footer`), or
#'   vector of length zero or more (`subtitles`, `page_titles`, `prov_footer`) containing
#'   the relevant title/footer contents.
#'
#' @export
#' @rdname title_footer
setGeneric("main_title", function(obj) standardGeneric("main_title"))

#' @export
#' @rdname title_footer
setMethod(
  "main_title", "MatrixPrintForm",
  function(obj) obj$main_title
)

##' @rdname title_footer
##' @export
setGeneric("main_title<-", function(obj, value) standardGeneric("main_title<-"))

##' @rdname title_footer
##' @export
setMethod(
  "main_title<-", "MatrixPrintForm",
  function(obj, value) {
    obj$main_title <- value
    obj
  }
)

# subtitles ---------------------------------------------------------------

#' @export
#' @rdname title_footer
setGeneric("subtitles", function(obj) standardGeneric("subtitles"))

#' @export
#' @rdname title_footer
setMethod(
  "subtitles", "MatrixPrintForm",
  function(obj) obj$subtitles
)

##' @rdname title_footer
##' @export
setGeneric("subtitles<-", function(obj, value) standardGeneric("subtitles<-"))

##' @rdname title_footer
##' @export
setMethod(
  "subtitles<-", "MatrixPrintForm",
  function(obj, value) {
    obj$subtitles <- value
    obj
  }
)

# page_titles ---------------------------------------------------------------

#' @export
#' @rdname title_footer
setGeneric("page_titles", function(obj) standardGeneric("page_titles"))

#' @export
#' @rdname title_footer
setMethod(
  "page_titles", "MatrixPrintForm",
  function(obj) obj$page_titles
)
#' @rdname title_footer
#' @export
setMethod("page_titles", "ANY", function(obj) NULL)

##' @rdname title_footer
##' @export
setGeneric("page_titles<-", function(obj, value) standardGeneric("page_titles<-"))

#' @export
#' @rdname title_footer
setMethod(
  "page_titles<-", "MatrixPrintForm",
  function(obj, value) {
    if (!is.character(value)) {
      stop("page titles must be in the form of a character vector, got object of class ", class(value))
    }
    obj$page_titles <- value
    obj
  }
)

# main_footer ---------------------------------------------------------------

#' @export
#' @rdname title_footer
setGeneric("main_footer", function(obj) standardGeneric("main_footer"))

#' @export
#' @rdname title_footer
setMethod(
  "main_footer", "MatrixPrintForm",
  function(obj) obj$main_footer
)

#' @rdname title_footer
#' @param value character. New value.
#' @export
setGeneric("main_footer<-", function(obj, value) standardGeneric("main_footer<-"))

#' @export
#' @rdname title_footer
setMethod(
  "main_footer<-", "MatrixPrintForm",
  function(obj, value) {
    if (!is.character(value)) {
      stop("main footer must be a character vector. Got object of class ", class(value))
    }
    obj$main_footer <- value
    obj
  }
)

# prov_footer ---------------------------------------------------------------

#' @export
#' @rdname title_footer
setGeneric("prov_footer", function(obj) standardGeneric("prov_footer"))

#' @export
#' @rdname title_footer
setMethod(
  "prov_footer", "MatrixPrintForm",
  function(obj) obj$prov_footer
)

#' @rdname title_footer
#' @export
setGeneric("prov_footer<-", function(obj, value) standardGeneric("prov_footer<-"))

#' @export
#' @rdname title_footer
setMethod(
  "prov_footer<-", "MatrixPrintForm",
  function(obj, value) {
    if (!is.character(value)) {
      stop("provenance footer must be a character vector. Got object of class ", class(value))
    }
    obj$prov_footer <- value
    obj
  }
)

#' @rdname title_footer
#' @export
all_footers <- function(obj) c(main_footer(obj), prov_footer(obj))

#' @rdname title_footer
#' @export
all_titles <- function(obj) c(main_title(obj), subtitles(obj), page_titles(obj))

# table_inset ---------------------------------------------------------------

#' Access or (recursively) set table inset
#'
#' Table inset is the amount of characters that the body of a table, referential footnotes, and
#' main footer material are inset from the left-alignment of the titles and provenance
#' footer materials.
#'
#' @param obj (`ANY`)\cr object to get or (recursively if necessary) set table inset for.
#' @param value (`string`)\cr string to use as new header/body separator.
#'
#' @return
#' * `table_inset` returns the integer value that the table body (including column heading
#'   information and section dividers), referential footnotes, and main footer should be inset
#'   from the left alignment of the titles and provenance footers during rendering.
#' * `table_inset<-` returns `obj` with the new table_inset value applied recursively to it and
#'   all its subtables.
#'
#' @export
setGeneric("table_inset", function(obj) standardGeneric("table_inset"))

#' @rdname table_inset
#' @export
setMethod(
  "table_inset", "MatrixPrintForm",
  function(obj) obj$table_inset
)

#' @rdname table_inset
#' @export
setGeneric("table_inset<-", function(obj, value) standardGeneric("table_inset<-"))

#' @rdname table_inset
#' @export
setMethod(
  "table_inset<-", "MatrixPrintForm",
  function(obj, value) {
    newval <- as.integer(value)
    if (is.na(newval) || newval < 0) {
      stop("Got invalid value for table_inset: ", newval)
    }
    obj$table_inset <- newval
    obj
  }
)

# do_forced_paginate ---------------------------------------------------------------

#' Generic for performing "forced" pagination
#'
#' Forced pagination is pagination which happens regardless of position on page. The object
#' is expected to have all information necessary to locate such page breaks, and the
#' `do_forced_pag` method is expected to fully perform those paginations.
#'
#' @param obj (`ANY`)\cr object to be paginated. The `ANY` method simply returns a list of
#'   length one, containing `obj`.
#'
#' @return A list of sub-objects, which will be further paginated by the standard pagination
#'   algorithm.
#'
#' @export
setGeneric("do_forced_paginate", function(obj) standardGeneric("do_forced_paginate"))

#' @export
#' @rdname do_forced_paginate
setMethod("do_forced_paginate", "ANY", function(obj) list(obj))

# num_rep_cols ---------------------------------------------------------------

#' Number of repeated columns
#'
#' When called on a table-like object using the formatters framework, this method returns the
#' number of columns which are mandatorily repeated after each horizontal pagination.
#'
#' Absent a class-specific method, this function returns 0, indicating no always-repeated columns.
#'
#' @param obj (`ANY`)\cr a table-like object.
#'
#' @return An integer.
#'
#' @note This number *does not* include row labels, the repetition of which is handled separately.
#'
#' @examples
#' mpf <- basic_matrix_form(mtcars)
#' num_rep_cols(mpf)
#' lmpf <- basic_listing_mf(mtcars)
#' num_rep_cols(lmpf)
#'
#' @export
setGeneric("num_rep_cols", function(obj) standardGeneric("num_rep_cols"))

#' @export
#' @rdname num_rep_cols
setMethod("num_rep_cols", "ANY", function(obj) 0L)

#' @export
#' @rdname num_rep_cols
setMethod("num_rep_cols", "MatrixPrintForm", function(obj) {
  if (.is_listing_mf(obj)) {
    return(length(.get_keycols_from_listing(obj)))
  } else {
    return(0L) # same as ANY for non-listing objects
  }
})

#' @export
#' @rdname num_rep_cols
setMethod("num_rep_cols", "MatrixPrintForm", function(obj) obj$num_rep_cols)

#' @export
#' @param value numeric(1). The new number of columns to repeat.
#' @rdname num_rep_cols
setGeneric("num_rep_cols<-", function(obj, value) standardGeneric("num_rep_cols<-"))

#' @export
#' @rdname num_rep_cols
setMethod("num_rep_cols<-", "ANY", function(obj, value) stop("No num_rep_cols<- method for class ", class(obj)))

#' @export
#' @rdname num_rep_cols
setMethod("num_rep_cols<-", "MatrixPrintForm", function(obj, value) {
  obj <- mf_update_cinfo(obj, colwidths = NULL, rep_cols = value)
  obj
})

# header_section_div -----------------------------------------------------------

#' @keywords internal
setGeneric("header_section_div", function(obj) standardGeneric("header_section_div"))

#' @keywords internal
setMethod(
  "header_section_div", "MatrixPrintForm",
  function(obj) obj$header_section_div
)

#' @keywords internal
setGeneric("header_section_div<-", function(obj, value) standardGeneric("header_section_div<-"))

#' @keywords internal
setMethod(
  "header_section_div<-", "MatrixPrintForm",
  function(obj, value) {
    obj$header_section_div <- value
    obj
  }
)

# horizontal_sep ---------------------------------------------------------------

#' @keywords internal
setGeneric("horizontal_sep", function(obj) standardGeneric("horizontal_sep"))

#' @keywords internal
setMethod(
  "horizontal_sep", "MatrixPrintForm",
  function(obj) obj$horizontal_sep
)

#' @keywords internal
setGeneric("horizontal_sep<-", function(obj, value) standardGeneric("horizontal_sep<-"))

#' @keywords internal
setMethod(
  "horizontal_sep<-", "MatrixPrintForm",
  function(obj, value) {
    obj$horizontal_sep <- value
    obj
  }
)
