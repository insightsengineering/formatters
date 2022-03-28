## until we do it for real

#' @title Matrix Print Form - Intermediate Representation for ASCII Table Printing
#'
#' @name MatrixPrintForm
#'
#' @rdname matrix_print_form
#' @aliases MatrixPrintForm-class
#' @exportClass MatrixPrintForm
setOldClass(c("MatrixPrintForm", "list"))


mform_handle_newlines <- function(matform, has_topleft = TRUE) {

  ## row_nlines <- apply(matform$strings, 1, nlines)
  strmat <- mf_strings(matform)
  frmmat <- mf_formats(matform)
  row_nlines <- apply(strmat, 1, nlines)
  nlines_header <- mf_nlheader(matform) ## attr(matform, "nlines_header")
  nr_header <- mf_nrheader(matform) ## attr(matform, "nrow_header")
  nrows <- nrow(strmat) # matform$strings)
  if (any(row_nlines > 1)) {
    hdr_inds <- 1:nr_header
    ## groundwork for sad haxx to get tl to not be messed up
    if (has_topleft) {
      tl <- strmat[hdr_inds, 1]
    } else {
      tl <- character()
    }
    ## matform$strings <- rbind(expand_mat_rows(matform$strings[hdr_inds, , drop = FALSE], row_nlines[hdr_inds], cpadder = pad_vert_bottom),
    newstrmat <- rbind(
      expand_mat_rows(strmat[hdr_inds, , drop = FALSE], row_nlines[hdr_inds], cpadder = pad_vert_bottom),
      expand_mat_rows(strmat[-1 * hdr_inds, , drop = FALSE], row_nlines[-hdr_inds])
    )
    newfrmmat <- rbind(
      expand_mat_rows(frmmat[hdr_inds, , drop = FALSE], row_nlines[hdr_inds], cpadder = pad_vert_bottom),
      expand_mat_rows(frmmat[-1 * hdr_inds, , drop = FALSE], row_nlines[-hdr_inds])
    )
    ## sad haxx :(
    if (has_topleft) {
      newstrmat[1:nr_header, 1] <- c(tl, rep("", nr_header - length(tl)))
      newfrmmat[1:nr_header, 1] <- c(tl, rep("", nr_header - length(tl)))
    }
    mf_strings(matform) <- newstrmat
    mf_formats(matform) <- newfrmmat
    ## matform$spans <- expand_mat_rows(matform$spans, row_nlines, rep_vec_to_len)
    mf_spans(matform) <- expand_mat_rows(mf_spans(matform), row_nlines, rep_vec_to_len)
    ## matform$aligns <- expand_mat_rows(matform$aligns, row_nlines, rep_vec_to_len)
    mf_aligns(matform) <- expand_mat_rows(mf_aligns(matform), row_nlines, rep_vec_to_len)
    ## matform$display <- expand_mat_rows(matform$display, row_nlines, rep_vec_to_len)
    mf_display(matform) <- expand_mat_rows(mf_display(matform), row_nlines, rep_vec_to_len)
    ## matform$line_grouping <- rep(1:nrows, times = row_nlines)
    mf_lgrouping(matform) <- rep(1:nrows, times = row_nlines)

    ## attr(matform, "nlines_header") <- sum(row_nlines[1:nr_header])
    mf_nlheader(matform) <- sum(row_nlines[1:nr_header])
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
#' @export
matrix_print_form <- function(strings = NULL,
                              spans,
                              aligns,
                              formats,
                              ##                          display,
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
                              prov_footer = character()) {
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
      line_grouping = 1:nrow(strings), # this is done for real in .do_mat_expand now
      ref_footnotes = ref_fnotes,
      main_title = main_title,
      subtitles = subtitles,
      page_titles = page_titles,
      main_footer = main_footer,
      prov_footer = prov_footer
    ),
    nlines_header = nlines_header, ## this is done for real in .do_mat_expand nownlines_header,
    nrow_header = nrow_header,
    ncols = ncs,
    class = c("MatrixPrintForm", "list")
  )
  ## .do_mat_expand(ret)
  if (expand_newlines) {
    ret <- mform_handle_newlines(ret, has_topleft = has_topleft)
  }
  ret
}

## constructor with usual naming convention
#' @rdname matrix_print_form
#' @export
MatrixPrintForm <- matrix_print_form


## hide the implementation behind abstraction incase we decide we want a real class someday
mf_strings <- function(mf) mf$strings

mf_spans <- function(mf) mf$spans

mf_aligns <- function(mf) mf$aligns

mf_display <- function(mf) mf$display

mf_formats <- function(mf) mf$formats

mf_rinfo <- function(mf) mf$row_info

mf_lgrouping <- function(mf) mf$linegrouping

mf_rfnotes <- function(mf) mf$ref_footnotes

mf_nlheader <- function(mf) attr(mf, "nlines_header")

mf_nrheader <- function(mf) attr(mf, "nrow_header")


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
      strdim[1], , ", ", strdim[2], ")."
    )
  }
  mf[[component]] <- value
  mf
}




`mf_spans<-` <- function(mf, value) {
  .chkdim_and_replace(mf, value, component = "spans")
}

`mf_aligns<-` <- function(mf, value) {
  .chkdim_and_replace(mf, value, component = "aligns")
}


`mf_display<-` <- function(mf, value) {
  .chkdim_and_replace(mf, value, component = "display")
}

`mf_formats<-` <- function(mf, value) {
  .chkdim_and_replace(mf, value, component = "formats")
}


## NB NROW(v) == length(v) for atomic vectors so this is ok for lgrouping as wellas rinfo
.chknrow_and_replace <- function(mf, value, component) {
  strdim <- NROW(mf_strings(mf))
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

`mf_rinfo<-` <- function(mf, value) {
  .chknrow_and_replace(mf, value, component = "row_info")
}

`mf_lgrouping<-` <- function(mf, value) {
  .chknrow_and_replace(mf, value, component = "line_grouping")
}


`mf_rfnotes<-` <- function(mf, value) {
  mf$ref_footnotes <- value
  mf
}


`mf_nlheader<-` <- function(mf, value) {
  attr(mf, "nlines_header") <- value
  mf
}

`mf_nrheader<-` <- function(mf, value) {
  attr(mf, "nrow_header") <- value
  mf
}



setMethod(
  "ncol", "MatrixPrintForm",
  function(x) attr(x, "ncols", TRUE)
)

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


  ## build up fake pagination df,
  rowdf <- basic_pagdf(row.names(df))
  matrix_print_form(
    strings = strings,
    aligns = aligns,
    spans = matrix(1, nrow = fnr, ncol = fnc),
    formats = NULL,
    row_info = rowdf,
    has_topleft = FALSE,
    nlines_header = 1,
    nrow_header = 1
  )
}
