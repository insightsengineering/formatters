## until we do it for real

#' @title Matrix Print Form - Intermediate Representation for ASCII Table Printing
#'
#' @name MatrixPrintForm-class
#'
#' @rdname MatrixPrintForm_class
#' @aliases MatrixPrintForm-class
#' @exportClass MatrixPrintForm
setOldClass(c("MatrixPrintForm", "list"))


mform_handle_newlines <- function(matform) {
  # Retrieving relevant information
  has_topleft <- mf_has_topleft(matform)
  strmat <- mf_strings(matform)
  frmmat <- mf_formats(matform)
  spamat <- mf_spans(matform)
  alimat <- mf_aligns(matform)
  nr_header <- mf_nrheader(matform)
  nl_inds_header <- seq(1, mf_nlheader(matform))
  hdr_inds <- 1:nr_header

  # hack that is necessary only if bottom aligned
  topleft_has_nl_char <- FALSE
  if (has_topleft) {
    tl <- strmat[nl_inds_header, 1, drop = TRUE]
    strmat[nl_inds_header, 1] <- ""
    tl <- tl[nzchar(tl)] # we are not interested in initial "" but we cover initial \n
    topleft_has_nl_char <- any(grepl("\n", tl))
    tl_to_add_back <- strsplit(paste0(tl, collapse = "\n"), split = "\n", fixed = TRUE)[[1]]
    how_many_nl <- length(tl_to_add_back)
  }

  # pre-proc in case of wrapping and \n
  line_grouping <- mf_lgrouping(matform)
  strmat <- .compress_mat(strmat, line_grouping, "nl")
  frmmat <- .compress_mat(frmmat, line_grouping, "unique") # never not unique
  spamat <- .compress_mat(spamat, line_grouping, "unique")
  alimat <- .compress_mat(alimat, line_grouping, "unique")
  line_grouping <- unique(line_grouping)

  # nlines detects if there is a newline character
  row_nlines <- apply(strmat, 1, function(x) max(vapply(x, nlines, 1L), 1L))

  if (has_topleft && (sum(row_nlines[nl_inds_header]) < how_many_nl)) {
    row_nlines[1] <- row_nlines[1] + how_many_nl - sum(row_nlines[nl_inds_header])
  }

  # There is something to change
  if (any(row_nlines > 1) || topleft_has_nl_char) {
    # False: Padder should be bottom aligned if no topleft (case of rlistings)
    # It is always bottom: tl_padder <- ifelse(has_topleft, pad_vert_top, pad_vert_bottom)

    newstrmat <- rbind(
      cbind(
        expand_mat_rows(strmat[hdr_inds, 1, drop = FALSE],
          row_nlines[hdr_inds],
          cpadder = pad_vert_bottom # topleft info is NOT top aligned
        ),
        expand_mat_rows(strmat[hdr_inds, -1, drop = FALSE],
          row_nlines[hdr_inds],
          cpadder = pad_vert_bottom # colnames are bottom aligned
        )
      ),
      expand_mat_rows(strmat[-1 * hdr_inds, , drop = FALSE], row_nlines[-hdr_inds])
    )

    newfrmmat <- rbind(
      expand_mat_rows(
        frmmat[hdr_inds, , drop = FALSE],
        row_nlines[hdr_inds],
        cpadder = pad_vert_bottom
      ),
      expand_mat_rows(frmmat[-1 * hdr_inds, , drop = FALSE], row_nlines[-hdr_inds])
    )

    if (has_topleft) {
      starts_from_ind <- if (sum(row_nlines[hdr_inds]) - how_many_nl > 0) {
        sum(row_nlines[hdr_inds]) - how_many_nl
      } else {
        0
      }
      newstrmat[starts_from_ind + seq_along(tl_to_add_back), 1] <- tl_to_add_back
    }

    mf_strings(matform) <- newstrmat
    mf_formats(matform) <- newfrmmat
    mf_spans(matform) <- expand_mat_rows(spamat, row_nlines, rep_vec_to_len)
    mf_aligns(matform) <- expand_mat_rows(alimat, row_nlines, rep_vec_to_len)
    ##    mf_display(matform) <- expand_mat_rows(mf_display(matform), row_nlines, rep_vec_to_len)
    mf_lgrouping(matform) <- rep(line_grouping, times = row_nlines)
  }

  matform
}

# Helper function to recompact the lines following line groupings to then have them expanded again
.compress_mat <- function(mat, line_grouping, collapse_method = c("nl", "unique")) {
  list_compacted_mat <- lapply(unique(line_grouping), function(lg) {
    apply(mat, 2, function(mat_cols) {
      col_vec <- mat_cols[which(line_grouping == lg)]
      if (collapse_method[1] == "nl") {
        paste0(col_vec, collapse = "\n")
      } else {
        val <- unique(col_vec)
        val <- val[nzchar(val)]
        if (length(val) > 1) {
          stop("Problem in linegroupings! Some do not have the same values.") # nocov
        } else if (length(val) < 1) {
          val <- "" # Case in which it is only ""
        }
        val[[1]]
      }
    })
  })
  do.call("rbind", list_compacted_mat)
}

disp_from_spans <- function(spans) {
  display <- matrix(rep(TRUE, length(spans)), ncol = ncol(spans))

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
  display
}

## constructor
#' @title Matrix Print Form - Intermediate Representation for ASCII Table Printing
#'
#' @note The bare constructor for the `MatrixPrintForm` should generally
#' only be called by `matrix_form` custom methods, and almost never from other code.
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
#'     element. Must  be   repeated   to   match  placeholders   in
#'     \code{strings}. Must be a supported text alignment. See
#'     [decimal_align] allowed values.
#' @param formats  matrix. Matrix  of same dimension
#'     as  \code{strings} giving  the text  format information  for
#'     each  element.   Must  be  repeated to  match  placeholders  in
#'     \code{strings}.
#' @param  row_info   data.frame.   Data.frame  with  row-information
#'     necessary for pagination (XXX document exactly what that is).
#' @param  line_grouping integer. Sequence of  integers indicating how
#'     print  lines  correspond  to   semantic  rows  in  the  object.
#'     Typically   this   should   not    be   set   manually   unless
#'     `expact_newlines` is set to \code{FALSE}.
#' @param  ref_fnotes  list.   Referential  footnote  information  if
#'     applicable.
#' @param  nlines_header numeric(1). Number  of lines taken up  by the
#'     values of the header (i.e. not including the divider).
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
#' @param prov_footer character.  Provenance footer  information as a
#'     character vector.
#' @param header_section_div character(1). Divider to be used between header
#'     and body sections.
#' @param expand_newlines logical(1). Should the matrix form generated
#'     expand  rows  whose  values   contain  newlines  into  multiple
#'     'physical'  rows  (as  they  will  appear  when  rendered  into
#'     ASCII). Defaults to \code{TRUE}
#' @param col_gap numeric(1). Space (in characters) between columns
#' @param table_inset numeric(1). Table inset. See
#' \code{\link{table_inset}}
#' @param colwidths numeric. NULL, or a vector of column rendering widths.
#'     if non-NULL, must have length equal to `ncol(strings)`
#' @param indent_size numeric(1). Number of spaces to be used per level of indent (if supported by
#' the relevant method). Defaults to 2.
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
#' \item{\code{formats}}{see argument}
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
                            header_section_div = NA_character_,
                            horizontal_sep = default_hsep(),
                            col_gap = 3,
                            table_inset = 0L,
                            colwidths = NULL,
                            indent_size = 2) {
  display <- disp_from_spans(spans)

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
      header_section_div = header_section_div,
      horizontal_sep = horizontal_sep,
      col_gap = col_gap,
      table_inset = as.integer(table_inset),
      has_topleft = has_topleft,
      indent_size = indent_size,
      col_widths = colwidths
    ),
    nrow_header = nrow_header,
    ncols = ncs,
    class = c("MatrixPrintForm", "list")
  )

  ## .do_mat_expand(ret)
  if (expand_newlines) {
    ret <- mform_handle_newlines(ret)
  }


  ##  ret <- shove_refdf_into_rowinfo(ret)
  if (is.null(colwidths)) {
    colwidths <- propose_column_widths(ret)
  }
  mf_col_widths(ret) <- colwidths
  ret <- mform_build_refdf(ret)
  ret
}


#' Create a row for a referential footnote information dataframe
#'
#' @inheritParams nlines
#' @param row_path character. row path (`NA_character_` for none)
#' @param col_path character. column path (`NA_character_` for none)
#' @param row integer(1). Integer position of the row.
#' @param col integer(1). Integer position of the column.
#' @param symbol character(1). Symbol for the reference. `NA_character_` to use the `ref_index` automatically.
#' @param ref_index integer(1). The index of the footnote, used for ordering even when symbol is not NA
#' @param msg character(1). The string message, not including the symbol portion (`{symbol} - `)
#'
#' @return a single row data.frame with the appropriate columns.
#'
#' @export
#'
ref_df_row <- function(row_path = NA_character_,
                       col_path = NA_character_,
                       row = NA_integer_,
                       col = NA_integer_,
                       symbol = NA_character_,
                       ref_index = NA_integer_,
                       msg = NA_character_,
                       max_width = NULL) {
  nlines <- nlines(msg, max_width = max_width)
  data.frame(
    row_path = I(list(row_path)),
    col_path = I(list(col_path)),
    row = row,
    col = col,
    symbol = symbol,
    ref_index = ref_index,
    msg = msg,
    nlines = nlines,
    stringsAsFactors = FALSE
  )
}


## this entire thing is a hatchetjob of a hack which should not be necessary.
## mf_rinfo(mform) should have the relevant info in it and
## mf_cinfo(mform) should be non-null (!!!) and have the info in it
## in which case this becomes silly and dumb, but here we are, so here we go.
infer_ref_info <- function(mform, colspace_only) {
  if (colspace_only) {
    idx <- seq_len(mf_nlheader(mform))
  } else {
    idx <- seq_len(nrow(mf_strings(mform)))
  }


  hasrlbs <- mf_has_rlabels(mform)

  strs <- mf_strings(mform)[idx, , drop = FALSE]

  ## they're nested so \\2 is the inner one, without the brackets
  refs <- gsub("^[^{]*([{]([^}]+)[}]){0,1}$", "\\2", strs)
  ## handle spanned values
  refs[!mf_display(mform)[idx, ]] <- ""

  ## we want to count across rows first, not down columns, cause
  ## thats how footnote numbering works
  refs_inorder <- as.vector(t(refs))
  keepem <- nzchar(refs_inorder)
  if (sum(keepem) == 0) {
    return(ref_df_row()[0, ])
  }

  refs_spl <- strsplit(refs_inorder[keepem], ", ", fixed = TRUE)
  runvec <- vapply(refs_spl, length, 1L)



  row_index <- as.vector(
    t(do.call(cbind, replicate(ncol(strs), list(mf_lgrouping(mform)[idx] - mf_nlheader(mform)))))
  )[keepem]
  row_index[row_index < 1] <- NA_integer_
  c_torep <- if (hasrlbs) c(NA_integer_, seq(1, ncol(strs) - 1)) else seq_len(ncol(strs))
  col_index <- rep(c_torep, nrow(strs))[keepem]




  ret <- data.frame(
    symbol = unlist(refs_spl),
    row_path = I(mf_rinfo(mform)$path[rep(row_index, times = runvec)]),
    row = rep(row_index, times = runvec),
    col = rep(col_index, times = runvec)
  )
  ret$msg <- vapply(ret$symbol, function(sym) {
    fullmsg <- unique(grep(paste0("{", sym, "}"), fixed = TRUE, mf_rfnotes(mform), value = TRUE))
    gsub("^[{][^}]+[}] - ", "", fullmsg)
  }, "")


  col_pths <- mf_col_paths(mform)
  ret$col_path <- replicate(nrow(ret), list(NA_character_))
  non_na_col <- !is.na(ret$col)
  ret$col_path[non_na_col] <- col_pths[ret$col[non_na_col]]
  ret$ref_index <- match(ret$symbol, unique(ret$symbol))
  ##
  ret$nlines <- vapply(paste0("{", ret$symbol, "} - ", ret$msg), nlines, 1L)
  ret <- ret[, names(ref_df_row())]
  ret
}

mform_build_refdf <- function(mform) {
  rdf <- mf_rinfo(mform)
  cref_rows <- infer_ref_info(mform, colspace_only = TRUE)
  ## this will recheck sometimes but its safer and shouldn't
  ## be too prohibitively costly
  if (NROW(rdf$ref_info_df) > 0 && sum(sapply(rdf$ref_info_df, NROW)) > 0) {
    cref_rows <- infer_ref_info(mform, colspace_only = TRUE)
    rref_rows <- rdf$ref_info_df
  } else {
    cref_rows <- infer_ref_info(mform, colspace_only = FALSE)
    rref_rows <- list()
  }
  mf_fnote_df(mform) <- do.call(rbind.data.frame, c(list(cref_rows), rref_rows))
  update_mf_nlines(mform, colwidths = mf_col_widths(mform), max_width = NULL)
}









## constructor with snake_case naming convention
#' @rdname MatrixPrintForm
#' @export
matrix_print_form <- MatrixPrintForm


## hide the implementation behind abstraction in case we decide we want a real class someday
#' `Setters` and `getters` for aspects of `MatrixPrintForm` Objects
#'
#' Most of these functions, particularly the `settters`, are intended
#' almost exclusively for internal use in, e.g., `matrix_form` methods,
#' and should generally not be called by end users.
#'
#' @param mf `MatrixPrintForm(1)`. A `MatrixPrintForm` object
#' @param value ANY. The new value for the component in question.
#' @return The element of the `MatrixPrintForm` associated with the `getter`, or
#' the modified `MatrixPrintForm` object in the case of a `setter`.
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
mf_cinfo <- function(mf) mf$col_info


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
mf_colgap <- function(mf) mf$col_gap




## XXX should this be exported? not sure if there's a point
mf_col_paths <- function(mf) {
  if (!is.null(mf_cinfo(mf))) {
    mf_cinfo(mf)$path
  } else {
    as.list(paste0("col", seq_len(nrow(mf_strings(mf)) - mf_has_topleft(mf))))
  }
}


mf_col_widths <- function(mf) {
  mf$col_widths
}

`mf_col_widths<-` <- function(mf, value) {
  if (!is.null(value) && length(value) != NCOL(mf_strings(mf))) {
    stop(
      "Number of column widths (", length(value), ") does not match ",
      "number of columns in strings matrix (", NCOL(mf_strings(mf)), ")."
    )
  }
  mf$col_widths <- value
  mf
}

mf_fnote_df <- function(mf) {
  mf$ref_fnote_df
}

`mf_fnote_df<-` <- function(mf, value) {
  stopifnot(is.null(value) || (is.data.frame(value) && identical(names(value), names(ref_df_row()))))
  mf$ref_fnote_df <- value
  mf
}


splice_fnote_info_in <- function(df, refdf, row = TRUE) {
  if (NROW(df) == 0) {
    return(df)
  }

  colnm <- ifelse(row, "row", "col")
  refdf <- refdf[!is.na(refdf[[colnm]]), ]

  refdf_spl <- split(refdf, refdf[[colnm]])
  df$ref_info_df <- replicate(nrow(df), list(ref_df_row()[0, ]))
  df$ref_info_df[as.integer(names(refdf_spl))] <- refdf_spl
  df
}


shove_refdf_into_rowinfo <- function(mform) {
  refdf <- mf_fnote_df(mform)
  rowinfo <- mf_rinfo(mform)
  mf_rinfo(mform) <- splice_fnote_info_in(rowinfo, refdf)
  mform
}

update_mf_nlines <- function(mform, colwidths, max_width) {
  mform <- update_mf_ref_nlines(mform, max_width = max_width)
  mform <- update_mf_rinfo_extents(mform)

  mform
}

update_mf_rinfo_extents <- function(mform) {
  rinfo <- mf_rinfo(mform)
  refdf_all <- mf_fnote_df(mform)
  refdf_rows <- refdf_all[!is.na(refdf_all$row), ]
  if (NROW(rinfo) == 0) {
    return(mform)
  }
  lgrp <- mf_lgrouping(mform) - mf_nrheader(mform)
  lgrp <- lgrp[lgrp > 0]
  rf_nlines <- vapply(seq_len(max(lgrp)), function(ii) {
    refdfii <- refdf_rows[refdf_rows$row == ii, ]
    refdfii <- refdfii[!duplicated(refdfii$symbol), ]
    if (NROW(refdfii) == 0L) {
      return(0L)
    }
    sum(refdfii$nlines)
  }, 1L)

  raw_self_exts <- vapply(split(lgrp, lgrp), length, 0L)
  stopifnot(length(raw_self_exts) == length(rf_nlines))
  new_exts <- raw_self_exts + rf_nlines

  mapdf <- data.frame(
    row_num = as.integer(names(new_exts)),
    raw_extent = raw_self_exts
  )
  stopifnot(all(mapdf$row_num == rinfo$abs_rownumber))


  new_par_exts <- vapply(rinfo$reprint_inds, function(idx) {
    sum(0L, mapdf$raw_extent[mapdf$row_num %in% idx])
  }, 1L)

  rinfo$self_extent <- new_exts
  rinfo$par_extent <- new_par_exts
  rinfo$nreflines <- rf_nlines
  mf_rinfo(mform) <- rinfo
  mform
}

update_mf_ref_nlines <- function(mform, max_width) {
  refdf <- mf_fnote_df(mform)
  if (NROW(refdf) == 0) {
    return(mform)
  }

  refdf$nlines <- vapply(
    paste0("{", refdf$symbol, "} - ", refdf$msg),
    nlines,
    max_width = max_width,
    1L
  )
  mf_fnote_df(mform) <- refdf
  shove_refdf_into_rowinfo(mform)
}



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
      vdim[1], ", ", vdim[2], # nocov
      ") do not match dimensions of existing 'strings' component (", # nocov
      strdim[1], ", ", strdim[2], ")." # nocov
    )
  }
  mf[[component]] <- value
  mf
}


#' @export
#' @rdname mpf_accessors
`mf_spans<-` <- function(mf, value) {
  mf <- .chkdim_and_replace(mf, value, component = "spans")
  mf$display <- disp_from_spans(value)
  mf
}

#' @export
#' @rdname mpf_accessors
`mf_aligns<-` <- function(mf, value) {
  .chkdim_and_replace(mf, value, component = "aligns")
}


#' @export
#' @rdname mpf_accessors
`mf_display<-` <- function(mf, value) {
  stop("display is now a derived element of the matrix print form, modify it via `mf_spans<-`")
  .chkdim_and_replace(mf, value, component = "display")
}

#' @export
#' @rdname mpf_accessors
`mf_formats<-` <- function(mf, value) {
  .chkdim_and_replace(mf, value, component = "formats")
}


## NB NROW(v) == length(v) for atomic vectors so this is ok for lgrouping as wellas rinfo
.chknrow_and_replace <- function(mf, value, component, noheader = FALSE) {
  strdim <- NROW(mf_strings(mf)) - if (noheader) mf_nlheader(mf) else 0L
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
  ## this can someijtmes be called after expanding newlines so in general
  ## we should not expect it to match the number of rows in the strings matrix
  ## .chknrow_and_replace(mf, value, component = "row_info", noheader = TRUE)
  lgrps <- mf_lgrouping(mf)
  nrs <- length(unique(lgrps[-seq_len(mf_nlheader(mf))]))
  if (NROW(value) != nrs) {
    stop(
      "Rows in new row_info component (",
      NROW(value),
      ") does not match number of rows reflected in line_grouping component (",
      nrs, ")"
    )
  }
  mf$row_info <- value
  mf
}

#' @export
#' @rdname mpf_accessors
`mf_cinfo<-` <- function(mf, value) {
  if (NROW(value) > 0 && NROW(value) != ncol(mf)) {
    stop(
      "Number of rows in new cinfo (", NROW(value), ") does not match ",
      "number of columns (", ncol(mf), ")"
    )
  }
  mf$col_info <- value
  mf
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

#' @export
#' @rdname mpf_accessors
`mf_colgap<-` <- function(mf, value) {
  mf$col_gap <- value
  mf
}

#' @export
#' @rdname mpf_accessors
mf_ncol <- function(mf) attr(mf, "ncols", exact = TRUE)

#' @export
#' @rdname mpf_accessors
mf_nrow <- function(mf) max(mf_lgrouping(mf)) - mf_nrheader(mf)



#' @export
#' @rdname mpf_accessors
`mf_ncol<-` <- function(mf, value) {
  stopifnot(is.numeric(value))
  attr(mf, "ncols") <- value
  mf
}

#' @param x `MatrixPrintForm`. The object.
#' @export
#' @rdname mpf_accessors
setMethod(
  "ncol", "MatrixPrintForm",
  function(x) mf_ncol(x)
)

#' @export
#' @rdname mpf_accessors
mpf_has_rlabels <- function(mf) {
  .Deprecated("mf_has_rlabels")
  mf_has_rlabels(mf)
}

#' @export
#' @rdname mpf_accessors
mf_has_rlabels <- function(mf) ncol(mf$strings) > ncol(mf)

#' Create spoof matrix form from a data.frame
#'
#' This is  useful primarily  for writing  testing/examples, and as a
#' starting point for more sophisticated custom `matrix_form` methods
#'
#' @param df data.frame
#' @param parent_path character. parent path that all rows should be "children of",
#' defaults to `"root"`, and generally should not matter to end users.
#'
#' @return A valid `MatrixPrintForm` object representing `df`,
#' ready for ASCII rendering
#'
#' @examples
#' mform <- basic_matrix_form(mtcars)
#' cat(toString(mform))
#' @export
basic_matrix_form <- function(df, parent_path = "root") {
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
    extents = exts,
    parent_path = parent_path
  )
  formats <- cbind(
    "",
    rbind(
      "",
      matrix("xx", nrow = nrow(df), ncol = ncol(df))
    )
  )

  ret <- matrix_print_form(
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
  mform_build_refdf(ret)
}


map_to_new <- function(old, map) {
  inds <- match(old, map$old_idx)
  map$new_idx[inds]
}


reconstruct_basic_fnote_list <- function(mf) {
  refdf <- mf_fnote_df(mf)
  if (NROW(refdf) == 0) {
    return(NULL)
  }
  refdf <- refdf[!duplicated(refdf$symbol), ]
  paste0("{", refdf$symbol, "} - ", refdf$msg)
}

.mf_subset_core_mats <- function(mf, i, row = TRUE) {
  fillnum <- if (row) nrow(mf_strings(mf)) - mf_nlheader(mf) else ncol(mf)
  if (is.logical(i) || all(i < 0)) {
    i <- seq_len(fillnum)[i]
  }

  if (row) {
    nlh <- mf_nlheader(mf)
    ncolrows <- mf_nrheader(mf)
    i_mat <- c(seq_len(nlh), which(mf_lgrouping(mf) %in% (i + ncolrows)))
    j_mat <- seq_len(ncol(mf_strings(mf)))
  } else {
    nlabcol <- as.integer(mf_has_rlabels(mf))
    i_mat <- seq_len(nrow(mf_strings(mf)))
    j_mat <- c(seq_len(nlabcol), i + nlabcol)
  }


  mf_strings(mf) <- mf_strings(mf)[i_mat, j_mat, drop = FALSE]
  mf_lgrouping(mf) <- as.integer(as.factor(mf_lgrouping(mf)[i_mat]))
  if (!row) {
    newspans <- truncate_spans(mf_spans(mf), j_mat) # 'i' is the columns here, b/c row is FALSE
  } else {
    newspans <- mf_spans(mf)[i_mat, j_mat, drop = FALSE]
  }
  mf_spans(mf) <- newspans
  mf_formats(mf) <- mf_formats(mf)[i_mat, j_mat, drop = FALSE]

  mf_aligns(mf) <- mf_aligns(mf)[i_mat, j_mat, drop = FALSE]
  if (!row) {
    mf_ncol(mf) <- length(i)
    if (!is.null(mf_col_widths(mf))) {
      mf_col_widths(mf) <- mf_col_widths(mf)[j_mat]
    }
  }
  mf
}

## ugh. spans are **way** more of a pain than I expected x.x
truncate_one_span <- function(spanrow, j) {
  i <- 1
  len <- length(spanrow)
  while (i < len) {
    spnlen <- spanrow[i]
    inds <- seq(i, i + spnlen - 1)
    newspnlen <- sum(inds %in% j)
    spanrow[inds] <- newspnlen
    i <- i + spnlen
  }
  spanrow[j]
}

truncate_spans <- function(spans, j) {
  if (length(spans[1, ]) == 1) {
    as.matrix(apply(spans, 1, truncate_one_span, j = j))
  } else {
    t(apply(spans, 1, truncate_one_span, j = j))
  }
}


mpf_subset_rows <- function(mf, i) {
  nlh <- mf_nlheader(mf)
  lgrps <- mf_lgrouping(mf)
  row_lgrps <- tail(lgrps, -1 * nlh)
  nrs <- length(unique(row_lgrps))
  ncolrows <- length(unique(lgrps[seq_len(nlh)]))

  ncs <- ncol(mf)
  mf <- .mf_subset_core_mats(mf, i, row = TRUE)
  map <- data.frame(
    old_idx = c(seq_len(ncolrows), i + ncolrows),
    new_idx = c(seq_len(ncolrows), ncolrows + order(i))
  )

  row_map <- data.frame(old_idx = i, new_idx = order(i))

  refdf <- mf_fnote_df(mf)

  old_nas <- is.na(refdf$row)
  refdf$row <- map_to_new(refdf$row, row_map)
  refdf <- refdf[old_nas | !is.na(refdf$row), ]
  mf_fnote_df(mf) <- refdf

  rinfo <- mf_rinfo(mf)

  rinfo <- rinfo[rinfo$abs_rownumber %in% i, ]

  rinfo$abs_rownumber <- map_to_new(rinfo$abs_rownumber, row_map)
  mf_rinfo(mf) <- rinfo

  mf <- shove_refdf_into_rowinfo(mf)
  mf_rfnotes(mf) <- reconstruct_basic_fnote_list(mf)
  mf
}



## we only care about referential footnotes, cause
## they are currently the only place we're tracking
## column information that will need to be touched up
## but lets be careful and do a bit more anyway
mpf_subset_cols <- function(mf, j) {
  nc <- ncol(mf)
  if (is.logical(j) || all(j < 0)) {
    j <- seq_len(nc)[j]
  }
  if (any(j < 0)) {
    stop("cannot mix negative and positive indices")
  }

  if (length(unique(j)) != length(j)) {
    stop("duplicated columns are not allowed when subsetting a matrix print form objects")
  }


  #    j_mat <- c(if(mf_has_topleft(mf)) seq_len(nlabcol), j + nlabcol)
  map <- data.frame(old_idx = j, new_idx = order(j))

  ## this has to happen before the remap inher
  refdf <- mf_fnote_df(mf)

  mf <- .mf_subset_core_mats(mf, j, row = FALSE)


  ## future proofing (pipe dreams)
  ## uncomment if we ever manage to have col info information on MPFs
  ## if(!is.null(mf_cinfo(mf))) {
  ##     cinfo <- mf_cinfo(mf)
  ##     cinfo <- cinfo[j, , drop = FALSE]
  ##     cinfo$abs_pos <- map_to_new(cinfo$abs_pos, map)
  ##     mf_cinfo(mf) <- mf
  ## }



  keep <- is.na(refdf$col) | refdf$col %in% j
  refdf <- refdf[keep, , drop = FALSE]

  refdf$col <- map_to_new(refdf$col, map)
  mf_fnote_df(mf) <- refdf
  mf <- shove_refdf_into_rowinfo(mf)
  mf_rfnotes(mf) <- reconstruct_basic_fnote_list(mf)
  mf_ncol(mf) <- length(j)
  mf
}
