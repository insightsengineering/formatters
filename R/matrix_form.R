## until we do it for real

#' @title Matrix Print Form - Intermediate Representation for ASCII Table Printing
#'
#' @name MatrixPrintForm
#'
#' @rdname MatrixPrintForm
#' @aliases MatrixPrintForm-class
#' @exportClass MatrixPrintForm
setOldClass(c("MatrixPrintForm", "list"))


mform_handle_newlines <- function(matform) {
  has_topleft <- mf_has_topleft(matform)
  strmat <- mf_strings(matform)
  frmmat <- mf_formats(matform)
  row_nlines <- apply(strmat, 1, function(x) max(vapply(x, nlines, 1L), 1L))
  nr_header <- mf_nrheader(matform)
  nrows <- nrow(strmat)
  if (any(row_nlines > 1)) {
    hdr_inds <- 1:nr_header
    ## used below even though we don't store it on the resulting object
    new_nlines_hdr <- sum(row_nlines[hdr_inds])
    ## groundwork for sad haxx to get tl to not be messed up
    if (has_topleft) {
      tl <- strmat[hdr_inds, 1]
    } else {
      tl <- character()
    }
    newstrmat <- rbind(
      expand_mat_rows(strmat[hdr_inds, , drop = FALSE],
        row_nlines[hdr_inds],
        cpadder = pad_vert_bottom
      ),
      expand_mat_rows(strmat[-1 * hdr_inds, , drop = FALSE], row_nlines[-hdr_inds])
    )
    newfrmmat <- rbind(
      expand_mat_rows(frmmat[hdr_inds, , drop = FALSE],
        row_nlines[hdr_inds],
        cpadder = pad_vert_bottom
      ),
      expand_mat_rows(frmmat[-1 * hdr_inds, , drop = FALSE], row_nlines[-hdr_inds])
    )
    ## sad haxx :(
    if (has_topleft) {
      newtl <- unlist(strsplit(tl, "\n"))
      if (length(newtl) > new_nlines_hdr) {
        stop(
          "Expanding top-left material resulted in more lines (", length(newtl),
          "than fit in the header."
        )
      }
      newstrmat[1:new_nlines_hdr, 1] <- c(tl, rep("", new_nlines_hdr - length(tl)))
      newfrmmat[1:new_nlines_hdr, 1] <- "xx"
    }
    mf_strings(matform) <- newstrmat
    mf_formats(matform) <- newfrmmat
    mf_spans(matform) <- expand_mat_rows(mf_spans(matform), row_nlines, rep_vec_to_len)
    mf_aligns(matform) <- expand_mat_rows(mf_aligns(matform), row_nlines, rep_vec_to_len)
    mf_display(matform) <- expand_mat_rows(mf_display(matform), row_nlines, rep_vec_to_len)
    mf_lgrouping(matform) <- rep(1:nrows, times = row_nlines)
  }

  matform
}

## constructor
#' MatrixPrintForm object constructor
#'
#' This should generally only be called by `matrix_form` custom
#' methods, and almost never from other code.
#'
#' @param  strings character  matrix.  Matrix  of formatted,  ready to
#'     display  strings  organized as  they  will  be positioned  when
#'     rendered.   Elements that  span more  than one  column must  be
#'     followed  by  the  correct number  of  placeholders  (typically
#'     either empty strings or repeats of the value).
#' @param  spans  numeric  matrix.    Matrix  of  same  dimension  as
#'     \code{strings}  giving   the  spanning  information   for  each
#'     element.    Must  be   repeated   to   match  placeholders   in
#'     \code{strings}.
#' @param  aligns character  matrix.   Matrix  of same  dimension  as
#'     \code{strings} giving  the text alignment information  for each
#'     element.    Must  be   repeated   to   match  placeholders   in
#'     \code{strings}.
#' @param formats  matrix. Matrix  of same dimension
#'     as  \code{strings} giving  the text  format information  for
#'     each  element.   Must  be  repeated to  match  placeholders  in
#'     \code{strings}.
#' @param  row_info   data.frame.   Data.frame  with  row-information
#'     necessary for pagination (XXX document exactly what that is).
#' @param  line_grouping integer. Sequence of  integers indicating how
#'     print  lines  correspond  to   semantic  rows  in  the  object.
#'     Typically   this   should   not    be   set   manually   unless
#'     expact_newlines is set to \code{FALSE}.
#' @param  ref_fnotes  list.   Referential  footnote  information  if
#'     applicable.
#' @param  nlines_header numeric(1). Number  of lines taken up  by the
#'     values of the header (ie not including the divider).
#' @param nrow_header numeric(1).  Number of \emph{rows} corresponding
#'     to the header.
#' @param  has_topleft logical(1).  Does the  corresponding table have
#'     'top left information' which should be treated differently when
#'     expanding  newlines.   Ignored   if  \code{expand_newlines}  is
#'     \code{FALSE}.
#' @param has_rowlabs  logical(1). Do  the matrices  (\code{strings},
#'     \code{spans},  \code{aligns})   each  contain  a   column  that
#'     corresponds  with  row  labels  (Rather than  with  table  cell
#'     values).  Defaults to \code{TRUE}.
#' @param main_title character(1). Main title as a string.
#' @param subtitles character. Subtitles, as a character vector.
#' @param page_titles character.  Page-specific titles, as a character
#'     vector.
#' @param main_footer character(1). Main footer as a string.
#' @param  prov_footer character.  Provenance footer  information as a
#'     character vector.
#' @param expand_newlines logical(1). Should the matrix form generated
#'     expand  rows  whose  values   contain  newlines  into  multiple
#'     'physical'  rows  (as  they  will  appear  when  rendered  into
#'     ASCII). Defaults to \code{TRUE}
#' @param col_gap numeric(1). Space (in characters) between columns
#' @param table_inset numeric(1). Table inset. See
#' \code{\link{table_inset}}
#' @export
#' @return An object of class `MatrixPrintForm`. Currently this is
#' implemented as an S3 class inheriting from list with the following
#' elements:
#' \describe{
#' \item{\code{strings}}{see argument}
#' \item{\code{spans}}{see argument}
#' \item{\code{aligns}}{see argument}
#' \item{\code{display}}{logical matrix of same dimension as `strings`
#' that specifies whether an element in `strings` will be displayed
#' when the table is rendered}
#' \item{\code{formats}}{see arugment}
#' \item{\code{row_info}}{see argument}
#' \item{\code{line_grouping}}{see argument}
#' \item{\code{ref_footnotes}}{see argument}
#' \item{\code{main_title}}{see argument}
#' \item{\code{subtitles}}{see argument}
#' \item{\code{page_titles}}{see argument}
#' \item{\code{main_footer}}{see argument}
#' \item{\code{prov_footer}}{see argument}
#' \item{\code{col_gap}}{see argument}
#' \item{\code{table_inset}}{see argument}
#' }
#'
#' as well as the following attributes:
#' \describe{
#' \item{\code{nlines_header}}{see argument}
#' \item{\code{nrow_header}}{see argument}
#' \item{\code{ncols}}{number of columns \emph{of the table}, not including
#' any row names/row labels}
#' }
MatrixPrintForm <- function(strings = NULL,
                            spans,
                            aligns,
                            formats,
                            row_info,
                            line_grouping = seq_len(NROW(strings)),
                            ref_fnotes = list(),
                            nlines_header,
                            nrow_header,
                            has_topleft = TRUE,
                            has_rowlabs = has_topleft,
                            expand_newlines = TRUE,
                            main_title = "",
                            subtitles = character(),
                            page_titles = character(),
                            main_footer = "",
                            prov_footer = character(),
                            col_gap = 3,
                            table_inset = 0L) {
  display <- matrix(rep(TRUE, length(strings)), ncol = ncol(strings))

  print_cells_mat <- spans == 1L
  if (!all(print_cells_mat)) {
    display_rws <- lapply(
      seq_len(nrow(spans)),
      function(i) {
        print_cells <- print_cells_mat[i, ]
        row <- spans[i, ]
        ##         display <- t(apply(spans, 1, function(row) {
        ## print_cells <- row == 1

        if (!all(print_cells)) {
          ## need to calculate which cell need to be printed
          print_cells <- spans_to_viscell(row)
        }
        print_cells
      }
    )
    display <- do.call(rbind, display_rws)
  }


  ncs <- if (has_rowlabs) ncol(strings) - 1 else ncol(strings)
  ret <- structure(
    list(
      strings = strings,
      spans = spans,
      aligns = aligns,
      display = display,
      formats = formats,
      row_info = row_info,
      line_grouping = line_grouping,
      ref_footnotes = ref_fnotes,
      main_title = main_title,
      subtitles = subtitles,
      page_titles = page_titles,
      main_footer = main_footer,
      prov_footer = prov_footer,
      col_gap = col_gap,
      table_inset = as.integer(table_inset),
      has_topleft = has_topleft
    ),
    nrow_header = nrow_header,
    ncols = ncs,
    class = c("MatrixPrintForm", "list")
  )
  ## .do_mat_expand(ret)
  if (expand_newlines) {
    ret <- mform_handle_newlines(ret)
  }
  ret
}

## constructor with snake_case naming convention
#' @rdname MatrixPrintForm
#' @export
matrix_print_form <- MatrixPrintForm


## hide the implementation behind abstraction incase we decide we want a real class someday
#' Setters and Getters for aspects of MatrixPrintForm Objects
#'
#' Most of these functions, particularly the settters, are intended
#' almost exclusively for internal use in, e.g., `matrix_form` methods,
#' and should generally not be called by end users.
#'
#' @param mf MatrixPrintForm(1). A MatrixPrintForm object
#' @param value ANY. The new value for the component in question.
#' @return The element of the MatrixPrintForm associated with the getter, or
#' the modified MatrixPrintForm object in the case of a setter.
#' @export
#' @rdname mpf_accessors
mf_strings <- function(mf) mf$strings
#' @export
#' @rdname mpf_accessors

mf_spans <- function(mf) mf$spans
#' @export
#' @rdname mpf_accessors

mf_aligns <- function(mf) mf$aligns

#' @export
#' @rdname mpf_accessors
mf_display <- function(mf) mf$display

#' @export
#' @rdname mpf_accessors
mf_formats <- function(mf) mf$formats

#' @export
#' @rdname mpf_accessors
mf_rinfo <- function(mf) mf$row_info

#' @export
#' @rdname mpf_accessors
mf_has_topleft <- function(mf) mf$has_topleft

#' @export
#' @rdname mpf_accessors
mf_lgrouping <- function(mf) mf$line_grouping

#' @export
#' @rdname mpf_accessors
mf_rfnotes <- function(mf) mf$ref_footnotes

#' @export
#' @rdname mpf_accessors
mf_nlheader <- function(mf) sum(mf_lgrouping(mf) <= mf_nrheader(mf))

#' @export
#' @rdname mpf_accessors
mf_nrheader <- function(mf) attr(mf, "nrow_header", exact = TRUE)


#' @export
#' @rdname mpf_accessors
`mf_strings<-` <- function(mf, value) {
  mf$strings <- value
  mf
}

.chkdim_and_replace <- function(mf, value, component) {
  strdim <- dim(mf_strings(mf))
  vdim <- dim(value)
  if (!is.null(strdim) && !identical(strdim, vdim)) {
    stop(
      "Dimensions of new '", component, "' value (",
      vdim[1], ", ", vdim[2],
      ") do not match dimensions of existing 'strings' component (",
      strdim[1], ", ", strdim[2], ")."
    )
  }
  mf[[component]] <- value
  mf
}




#' @export
#' @rdname mpf_accessors
`mf_spans<-` <- function(mf, value) {
  .chkdim_and_replace(mf, value, component = "spans")
}

#' @export
#' @rdname mpf_accessors
`mf_aligns<-` <- function(mf, value) {
  .chkdim_and_replace(mf, value, component = "aligns")
}


#' @export
#' @rdname mpf_accessors
`mf_display<-` <- function(mf, value) {
  .chkdim_and_replace(mf, value, component = "display")
}

#' @export
#' @rdname mpf_accessors
`mf_formats<-` <- function(mf, value) {
  .chkdim_and_replace(mf, value, component = "formats")
}


## NB NROW(v) == length(v) for atomic vectors so this is ok for lgrouping as wellas rinfo
.chknrow_and_replace <- function(mf, value, component, noheader = FALSE) {
  strdim <- NROW(mf_strings(mf)) - if(noheader) mf_nlheader(mf) else 0L
  vdim <- NROW(value)
  if (!is.null(strdim) && !identical(strdim, vdim)) {
    stop(
      "Number of rows/length of new '", component, "' value (",
      vdim[1],
      ") does not match existing 'strings' component (",
      strdim[1], ")."
    )
  }
  mf[[component]] <- value
  mf
}

#' @export
#' @rdname mpf_accessors
`mf_rinfo<-` <- function(mf, value) {
  .chknrow_and_replace(mf, value, component = "row_info", noheader = TRUE)
}

#' @export
#' @rdname mpf_accessors
`mf_lgrouping<-` <- function(mf, value) {
  .chknrow_and_replace(mf, value, component = "line_grouping")
}


#' @export
#' @rdname mpf_accessors
`mf_rfnotes<-` <- function(mf, value) {
  mf$ref_footnotes <- value
  mf
}



#' @export
#' @rdname mpf_accessors
`mf_nrheader<-` <- function(mf, value) {
  attr(mf, "nrow_header") <- value
  mf
}



setMethod(
  "ncol", "MatrixPrintForm",
  function(x) attr(x, "ncols", exact = TRUE)
)

#' @export
#' @rdname mpf_accessors
mpf_has_rlabels <- function(mf) ncol(mf$strings) > ncol(mf)

#' Create spoof matrix form from a data.frame
#'
#' This is  useful primarily  for writing  testing/examples, and as a
#' starting point for more sophisticated custom `matrix_form` methods
#'
#' @param df data.frame
#'
#' @return A valid `MatrixPrintForm` object representing `df`,
#' ready for ASCII rendering
#'
#' @examples
#' mform <- basic_matrix_form(mtcars)
#' cat(toString(mform))
#' @export
basic_matrix_form <- function(df) {
  fmts <- lapply(df, function(x) if (is.null(obj_format(x))) "xx" else obj_format(x))

  bodystrs <- mapply(function(x, fmt) {
    sapply(x, format_value, format = fmt)
  }, x = df, fmt = fmts)

  rnms <- row.names(df)
  if (is.null(rnms)) {
    rnms <- as.character(seq_len(NROW(df)))
  }

  cnms <- names(df)

  strings <- rbind(
    c("", cnms),
    cbind(rnms, bodystrs)
  )

  fnr <- nrow(strings)
  fnc <- ncol(strings)

  ## center alignment for column labels, left alignment for everything else
  aligns <- rbind(
    "center",
    matrix("left", nrow = NROW(df), ncol = fnc)
  )


  ## build up fake pagination df
  charcols <- which(sapply(df, is.character))
  if (length(charcols) > 0) {
    exts <- apply(df[, charcols, drop = FALSE], 1, function(x) max(vapply(x, nlines, 1L)))
  } else {
    exts <- rep(1L, NROW(df))
  }
  rowdf <- basic_pagdf(row.names(df),
    extents = exts
  )
  formats <- cbind(
    "",
    rbind(
      "",
      matrix("xx", nrow = nrow(df), ncol = ncol(df))
    )
  )

  matrix_print_form(
    strings = strings,
    aligns = aligns,
    spans = matrix(1, nrow = fnr, ncol = fnc),
    formats = formats, ## matrix("xx", nrow = fnr, ncol = fnc),
    row_info = rowdf,
    has_topleft = FALSE,
    nlines_header = 1,
    nrow_header = 1,
    has_rowlabs = TRUE
  )
}
