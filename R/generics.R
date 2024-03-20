### This file defines the generics which make up the interface `formatters` offers.
### Defining methods for these generics for a new table-like class should be fully
### sufficient for hooking that class up to the `formatters` pagination and rendering
### machinery.


#' @import methods
#' @include matrix_form.R
#'
#' @title Make row layout summary data.frames for use during pagination
#'
#' @description
#' All relevant information about table rows (e.g. indentations) is summarized in a data.frames.
#' This function works ONLY on `rtables` and `rlistings` objects, and not on their print counterparts
#' (like `MatrixPrintForm`).
#'
#' @name make_row_df
#'
#' @inheritParams open_font_dev
#' @param tt ANY. Object representing the table-like object to be summarized.
#' @param visible_only logical(1). Should only visible aspects of the table structure be reflected in this summary.
#'   Defaults to \code{TRUE}. May not be supported by all methods.
#' @param incontent logical(1). Internal detail do not set manually.
#' @param repr_ext integer(1). Internal detail do not set manually.
#' @param repr_inds integer. Internal detail do not set manually.
#' @param sibpos integer(1). Internal detail do not set manually.
#' @param nsibs integer(1). Internal detail do not set manually.
#' @param rownum numeric(1). Internal detail do not set manually.
#' @param indent integer(1). Internal detail do not set manually.

#' @param colwidths numeric. Internal detail do not set manually.
#' @param  path character.  Path  to  the (sub)table  represented  by
#'     \code{tt}. Defaults to \code{character()}
#' @param max_width numeric(1) or NULL. Maximum width for title/footer
#' materials.
#' @param col_gap numeric(1). The gap to be assumed between columns,
#' in number of spaces with font specified by `fontspec`
#'
#' @details When  \code{visible_only} is  \code{TRUE} (the  default),
#'     methods should  return a  data.frame with  exactly one  row per
#'     visible  row in  the table-like  object.  This  is useful  when
#'     reasoning about  how a table  will print, but does  not reflect
#'     the full pathing space of the structure (though the paths which
#'     are given will all work as is).
#'
#' If  supported,  when  \code{visible_only}  is  \code{FALSE},  every
#' structural element of the table (in row-space) will be reflected in
#' the  returned data.frame,  meaning the  full pathing-space  will be
#' represented but some rows in  the layout summary will not represent
#' printed rows in the table as it is displayed.
#'
#' Most arguments beyond \code{tt} and \code{visible_only} are present so that
#' `make_row_df` methods can call `make_row_df` recursively and retain information,
#' and should not be set during a top-level call
#'
#' @note the technically present root tree node is excluded from the summary returned by
#' both \code{make_row_df} and \code{make_col_df} (see `rtables::make_col_df`), as it is simply the
#' row/column structure of \code{tt} and thus not useful for pathing or pagination.
#' @return a data.frame of row/column-structure information used by the pagination machinery.
#'
#' @rdname make_row_df
#' @export
## nocov start
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
  stop(
    "make_row_df can be used only on {rtables} table objects, and not on `matrix_form`-",
    "generated objects (MatrixPrintForm)."
  )
})
## nocov end


#' Transform `rtable` to a list of matrices which can be used for outputting
#'
#' Although `rtables` are represented as a tree data structure when outputting the table to ASCII or HTML it is
#' useful to map the `rtable` to an in between state with the formatted cells in a matrix form.
#'
#' @inheritParams make_row_df
#' @param obj ANY. Object to be transformed into a ready-to-render form (a `MatrixPrintForm` object)
#' @param indent_rownames logical(1), if TRUE the column with the row names in the `strings` matrix of has indented row
#' names (strings pre-fixed)
#' @param expand_newlines logical(1). Should the matrix form generated
#'     expand  rows  whose  values   contain  newlines  into  multiple
#'     'physical'  rows  (as  they  will  appear  when  rendered  into
#'     ASCII). Defaults to \code{TRUE}
#' @param indent_size numeric(1). Number of spaces to be used per level of indent (if supported by
#' the relevant method). Defaults to 2.
#' @export
#'
#' @details
#'
#' The strings in the return object are defined as follows: row labels are those determined by \code{summarize_rows} and
#' cell values are determined using \code{get_formatted_cells}.
#' (Column labels are calculated using a non-exported internal function.
#'
#' @return A `MatrixPrintForm` classed list with the following elements:
#' \describe{
#' \item{strings}{The content, as it should be printed, of the top-left material, column headers, row labels, and
#'   cell values of \code{tt}}
#' \item{spans}{The column-span information for each print-string in the strings matrix}
#' \item{aligns}{The text alignment for each print-string in the strings matrix}
#' \item{display}{Whether each print-string in the strings matrix should be printed or not}.
#' \item{row_info}{the data.frame generated by \code{summarize_rows(tt)}}
#' }
#'
#' With an additional \code{nrow_header} attribute indicating the number of pseudo "rows"  the
#' column structure defines.
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
  if(!is.null(fontspec))
    mf_fontspec(obj) <- fontspec
  if(!is.null(col_gap) && !isTRUE(all.equal(col_gap, mf_colgap(obj))))
    mf_colgap(obj) <- col_gap
  obj
})


## Generics for `toString` and helper functions


## this is where we will take word wrapping
## into account when it is added
##
## ALL calculations of vertical space for pagination
## purposes must go through nlines and divider_height!!!!!!!!

## this will be customizable someday. I have foreseen it (spooky noises)
#' Divider Height
#'
#' @param obj ANY. Object.
#' @return The height, in lines of text, of the divider between
#' header and body. Currently returns \code{1L} for the default method.
#' @export
#' @examples
#' divider_height(mtcars)
setGeneric("divider_height", function(obj) standardGeneric("divider_height"))

#' @rdname divider_height
#' @export
setMethod(
  "divider_height", "ANY",
  function(obj) 1L
)

#' Number of lines required to print a value
#' @inheritParams open_font_dev
#' @param x ANY. The object to be printed
#' @param colwidths numeric. Column widths (if necessary).
#' @param max_width numeric(1). Width strings should be wrapped to
#' when determining how many lines they require.
#' @param col_gap numeric(1). Width of gap between columns in number of spaces.
#' Only used by methods which must calculate span widths after wrapping.
#' @return A scalar numeric indicating the number of lines needed
#' to render the object \code{x}.
#' @export
setGeneric(
    "nlines",
    ## XXX TODO come back and add fontspec default value once not having
    ## it has found all the disconnection breakages
  function(x, colwidths = NULL, max_width = NULL, fontspec, col_gap) standardGeneric("nlines")
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
  splstr <- strsplit(x, "\n", fixed =TRUE)
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



#' @title `toString`
#'
#' @description Transform a complex object into a string representation ready
#' to be printed or written to a plain-text file
#'
#' @param x ANY. Object to be prepared for rendering.
#' @param ... Passed to individual methods.
#' @rdname tostring
#' @export
setGeneric("toString", function(x, ...) standardGeneric("toString"))

## preserve S3 behavior
setMethod("toString", "ANY", base::toString) ## nocov

#' @title Print
#'
#' @description Print an R object. see \code{[base::print()]}
#' @inheritParams base::print
#' @rdname basemethods
setMethod("print", "ANY", base::print) ## nocov










## General/"universal" property `getter` and `setter` generics and stubs

#' @title Label, Name and Format accessor generics
#'
#' @description `Getters` and `setters` for basic, relatively universal attributes
#' of "table-like" objects"
#'
#' @name lab_name
#' @param obj ANY. The object.
#' @param value character(1)/FormatSpec. The new value of the attribute.
#' @return the name, format or label of \code{obj} for `getters`, or \code{obj} after modification
#' for setters.
#' @aliases obj_name
#' @export

## no exported methods so we do nocov
# nocov start
setGeneric("obj_name", function(obj) standardGeneric("obj_name"))


#' @rdname lab_name
#' @export
setGeneric("obj_name<-", function(obj, value) standardGeneric("obj_name<-"))
# nocov end

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

#' General title/footer accessors
#'
#' @param obj ANY. Object to extract information from.
#' @export
#' @rdname title_footer
#' @return A character scalar (`main_title`), a character vector (`main_footer`), or
#' vector of length zero or more (`subtitles`, `page_titles`,
#' `prov_footer`) containing the relevant title/footer contents
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



#' @export
#' @rdname title_footer
setGeneric("subtitles", function(obj) standardGeneric("subtitles")) ## nocov

#' @export
#' @rdname title_footer
setMethod(
  "subtitles", "MatrixPrintForm",
  function(obj) obj$subtitles
)

##' @rdname title_footer
##' @export
setGeneric("subtitles<-", function(obj, value) standardGeneric("subtitles<-")) ## nocov

##' @rdname title_footer
##' @export
setMethod(
  "subtitles<-", "MatrixPrintForm",
  function(obj, value) {
    obj$subtitles <- value
    obj
  }
)

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


#' Access or (recursively) set table inset.
#'
#' Table inset is the amount of characters that the body of
#' a table, referential footnotes, and main footer material
#' are inset from the left-alignment of the titles and provenance
#' footer materials.
#'
#' @param obj ANY. Object to get or (recursively if necessary) set
#' table inset for.
#' @param value character(1). String to use as new header/body separator.
#'
#' @return for `table_inset` the integer value that the table body
#' (including column heading information and section dividers),
#' referential footnotes, and main footer should be inset from the
#' left alignment of the titles and provenance footers during rendering.
#' For `table_inset<-`, the `obj`, with the new table_inset value
#' applied recursively to it and all its subtables.
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




#' Generic for Performing "Forced Pagination"
#'
#' Forced pagination is pagination which happens regardless of
#' position on page. The object is expected to have all information
#' necessary to locate such page breaks, and the `do_forced_pag`
#' method is expected to fully perform those paginations.
#'
#' @param obj The object to be paginated.
#'
#' The `ANY` method simply returns a list of length one, containing
#' `obj`.
#'
#' @return a list of subobjects, which will be further paginated
#' by the standard pagination algorithm.
#'
#'
#' @export
setGeneric("do_forced_paginate", function(obj) standardGeneric("do_forced_paginate"))

#' @export
#' @rdname do_forced_paginate
setMethod("do_forced_paginate", "ANY", function(obj) list(obj))

#' Number of repeated columns
#'
#' When called on a table-like object using the formatters framework,
#' this method should return the number of columns which are mandatorily
#' repeated after each horizontal pagination.
#'
#' Absent a class-specific method, this function returns 0, indicating
#' no always-repeated columns.
#'
#' @param obj ANY. A table-like object.
#' @note This number \emph{does not include row labels}, the repetition
#' of which is handled separately.
#'
#' @return an integer.
#' @export
#' @examples
#' mpf <- basic_matrix_form(mtcars)
#' num_rep_cols(mpf)
setGeneric("num_rep_cols", function(obj) standardGeneric("num_rep_cols"))
#' @export
#' @rdname num_rep_cols
setMethod("num_rep_cols", "ANY", function(obj) 0L)

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
