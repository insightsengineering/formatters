## until we do it for real

#' Class for Matrix Print Form
#'
#' The `MatrixPrintForm` class, an intermediate representation for ASCII table printing.
#'
#' @name MatrixPrintForm-class
#' @rdname MatrixPrintForm_class
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

  # hack that is necessary only if top-left is bottom aligned (default)
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
  # colwidths = NULL, max_width = NULL, fontspec = NULL
  # because we don't care about wrapping here we're counting lines
  # TODO probably better if we had a nlines_nowrap fun to be more explicit

  row_nlines <- apply(
    strmat,
    1,
    function(x) {
      max(
        vapply(x,
          nlines,
          colwidths = NULL,
          max_width = NULL,
          fontspec = NULL, 1L
        ),
        1L
      )
    }
  )


  # Correction for the case where there are more lines for topleft material than for cols
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

  # Solve \n in titles
  if (any(grepl("\n", all_titles(matform)))) {
    if (any(grepl("\n", main_title(matform)))) {
      tmp_title_vec <- .quick_handle_nl(main_title(matform))
      main_title(matform) <- tmp_title_vec[1]
      subtitles(matform) <- c(tmp_title_vec[-1], .quick_handle_nl(subtitles(matform)))
    } else {
      subtitles(matform) <- .quick_handle_nl(subtitles(matform))
    }
  }

  # Solve \n in footers
  main_footer(matform) <- .quick_handle_nl(main_footer(matform))
  prov_footer(matform) <- .quick_handle_nl(prov_footer(matform))

  # xxx \n in page titles are not working atm (I think)

  matform
}

.quick_handle_nl <- function(str_v) {
  if (any(grepl("\n", str_v))) {
    return(unlist(strsplit(str_v, "\n", fixed = TRUE)))
  } else {
    return(str_v)
  }
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

.fixup_cinfo <- function(col_df) {
  if (is.null(col_df$par_extent)) {
    col_df$par_extent <- 0L
  }
  if (is.null(col_df$node_class)) {
    col_df$node_class <- "Column"
  }
  col_df
}

#' Constructor for Matrix Print Form
#'
#' Constructor for `MatrixPrintForm`, an intermediate representation for ASCII table printing.
#'
#' @inheritParams open_font_dev
#' @param strings (`character matrix`)\cr matrix of formatted, ready-to-display strings
#'   organized as they will be positioned when rendered. Elements that span more than one
#'   column must be followed by the correct number of placeholders (typically either empty
#'   strings or repeats of the value).
#' @param spans (`numeric matrix`)\cr matrix of same dimension as `strings` giving the
#'   spanning information for each element. Must be repeated to match placeholders in `strings`.
#' @param aligns (`character matrix`)\cr matrix of same dimension as `strings` giving the text
#'   alignment information for each element. Must be repeated to match placeholders in `strings`.
#'   Must be a supported text alignment. See [decimal_align] for allowed values.
#' @param formats (`matrix`)\cr matrix of same dimension as `strings` giving the text format
#'   information for each element. Must be repeated to match placeholders in `strings`.
#' @param row_info (`data.frame`)\cr data frame with row-information necessary for pagination (see
#'   [basic_pagdf()] for more details).
#' @param colpaths (`list` or `NULL`)\cr `NULL`, or a list of paths to each leaf column,
#'   for use during horizontal pagination.
#' @param line_grouping (`integer`)\cr sequence of integers indicating how print lines correspond
#'   to semantic rows in the object. Typically this should not be set manually unless
#'   `expand_newlines` is set to `FALSE`.
#' @param ref_fnotes (`list`)\cr referential footnote information, if applicable.
#' @param nlines_header (`numeric(1)`)\cr number of lines taken up by the values of the header
#'   (i.e. not including the divider).
#' @param nrow_header (`numeric(1)`)\cr number of *rows* corresponding to the header.
#' @param has_topleft (`flag`)\cr does the corresponding table have "top left information"
#'   which should be treated differently when expanding newlines. Ignored if `expand_newlines`
#'   is `FALSE`.
#' @param has_rowlabs (`flag`)\cr do the matrices (`strings`, `spans`, `aligns`) each contain a
#'   column that corresponds with row labels (rather than with table cell values). Defaults to `TRUE`.
#' @param main_title (`string`)\cr main title as a string.
#' @param subtitles (`character`)\cr subtitles, as a character vector.
#' @param page_titles (`character`)\cr page-specific titles, as a character vector.
#' @param main_footer (`character`)\cr main footer, as a character vector.
#' @param prov_footer (`character`)\cr provenance footer information, as a character vector.
#' @param listing_keycols (`character`)\cr. if matrix form of a listing, this contains
#'   the key columns as a character vector.
#' @param header_section_div (`string`)\cr divider to be used between header and body sections.
#' @param horizontal_sep (`string`)\cr horizontal separator to be used for printing divisors
#'   between header and table body and between different footers.
#' @param expand_newlines (`flag`)\cr whether the matrix form generated should expand rows whose
#'   values contain newlines into multiple 'physical' rows (as they will appear when rendered into
#'   ASCII). Defaults to `TRUE`.
#' @param col_gap (`numeric(1)`)\cr space (in characters) between columns.
#' @param table_inset (`numeric(1)`)\cr table inset. See [table_inset()].
#' @param colwidths (`numeric` or `NULL`)\cr column rendering widths. If non-`NULL`, must have length
#'   equal to `ncol(strings)`.
#' @param indent_size (`numeric(1)`)\cr number of spaces to be used per level of indent (if supported by
#'   the relevant method). Defaults to 2.
#' @param rep_cols (`numeric(1)`)\cr number of columns to be repeated as context during horizontal pagination.
#'
#' @return An object of class `MatrixPrintForm`. Currently this is implemented as an S3 class inheriting
#'   from list with the following elements:
#'   \describe{
#'     \item{`strings`}{see argument.}
#'     \item{`spans`}{see argument.}
#'     \item{`aligns`}{see argument.}
#'     \item{`display`}{logical matrix of same dimension as `strings` that specifies whether an element
#'       in `strings` will be displayed when the table is rendered.}
#'     \item{`formats`}{see argument.}
#'     \item{`row_info`}{see argument.}
#'     \item{`line_grouping`}{see argument.}
#'     \item{`ref_footnotes`}{see argument.}
#'     \item{`main_title`}{see argument.}
#'     \item{`subtitles`}{see argument.}
#'     \item{`page_titles`}{see argument.}
#'     \item{`main_footer`}{see argument.}
#'     \item{`prov_footer`}{see argument.}
#'     \item{`header_section_div`}{see argument.}
#'     \item{`horizontal_sep`}{see argument.}
#'     \item{`col_gap`}{see argument.}
#'     \item{`table_inset`}{see argument.}
#'   }
#'
#'   as well as the following attributes:
#'
#'   \describe{
#'     \item{`nlines_header`}{see argument.}
#'     \item{`nrow_header`}{see argument.}
#'     \item{`ncols`}{number of columns *of the table*, not including any row names/row labels}
#'   }
#'
#' @note The bare constructor for the `MatrixPrintForm` should generally
#'   only be called by `matrix_form` custom methods, and almost never from other code.
#'
#' @examples
#' basic_matrix_form(iris) # calls matrix_form which calls this constructor
#'
#' @export
MatrixPrintForm <- function(strings = NULL,
                            spans,
                            aligns,
                            formats,
                            row_info,
                            colpaths = NULL,
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
                            listing_keycols = NULL,
                            main_footer = "",
                            prov_footer = character(),
                            header_section_div = NA_character_,
                            horizontal_sep = default_hsep(),
                            col_gap = 3,
                            table_inset = 0L,
                            colwidths = NULL,
                            indent_size = 2,
                            fontspec = font_spec(),
                            rep_cols = 0L) {
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
      listing_keycols = listing_keycols,
      table_inset = as.integer(table_inset),
      has_topleft = has_topleft,
      indent_size = indent_size,
      col_widths = colwidths,
      fontspec = fontspec,
      num_rep_cols = rep_cols
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
    colwidths <- propose_column_widths(ret, fontspec = fontspec)
  }
  mf_col_widths(ret) <- colwidths
  ret <- mform_build_refdf(ret)
  ret <- mpf_infer_cinfo(ret, colpaths = colpaths, fontspec = fontspec)

  ret
}

mf_update_cinfo <- function(mf, colwidths = NULL, rep_cols = NULL) {
  need_update <- FALSE
  if (!is.null(colwidths)) {
    mf$col_widths <- colwidths
    need_update <- TRUE
  }

  if (!is.null(rep_cols)) {
    mf$num_rep_cols <- rep_cols
    need_update <- TRUE
  }

  if (need_update && !is.null(mf_cinfo(mf))) {
    cinfo <- mf_cinfo(mf)
    r_colwidths <- mf_col_widths(mf)
    has_rlabs <- mf_has_rlabels(mf)
    if (has_rlabs) {
      r_colwidths <- r_colwidths[-1] ## row label widths
    }
    cinfo$self_extent <- r_colwidths
    nrepcols <- num_rep_cols(mf)
    rep_seq <- seq_len(nrepcols)
    cinfo$par_extent <- cumsum(c(0, cinfo$self_extent[seq_len(nrepcols)], rep(0, length(r_colwidths) - nrepcols - 1)))
    cinfo$reprint_inds <- I(lapply(seq_len(NROW(cinfo)), function(i) rep_seq[rep_seq < i]))
    mf_cinfo(mf) <- cinfo
  }
  mf
}

#' Create a row for a referential footnote information data frame
#'
#' @inheritParams nlines
#' @param row_path (`character`)\cr row path (or `NA_character_` for none).
#' @param col_path (`character`)\cr column path (or `NA_character_` for none).
#' @param row (`integer(1)`)\cr integer position of the row.
#' @param col (`integer(1)`)\cr integer position of the column.
#' @param symbol (`string`)\cr symbol for the reference. `NA_character_` to use the
#'   `ref_index` automatically.
#' @param ref_index (`integer(1)`)\cr index of the footnote, used for ordering even when
#'   symbol is not `NA`.
#' @param msg (`string`)\cr the string message, not including the symbol portion (`{symbol} - `)
#'
#' @return A single row data frame with the appropriate columns.
#'
#' @export
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
  ## include space in front of { so we don't catch \{ when
  ## rtfs want to pass markup through
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

## hide the implementation behind abstraction in case we decide we want a real class someday
#' Getters and setters for aspects of `MatrixPrintForm` objects
#'
#' Most of these functions, particularly the setters, are intended almost exclusively for
#' internal use in, e.g., [`matrix_form`] methods, and should generally not be called by end users.
#'
#' @param mf (`MatrixPrintForm`)\cr a `MatrixPrintForm` object.
#' @param value (`ANY`)\cr the new value for the component in question.
#'
#' @return
#' * Getters return the associated element of `mf`.
#' * Setters return the modified `mf` object.
#'
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

#' @export
#' @rdname mpf_accessors
mf_fontspec <- function(mf) mf$fontspec

#' @export
#' @rdname mpf_accessors
`mf_fontspec<-` <- function(mf, value) {
  mf$fontspec <- value
  mf
}

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
  mf <- mf_update_cinfo(mf, colwidths = value, rep_cols = NULL)
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
    fontspec = mf_fontspec(mform),
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
  if (NROW(value) > 0 && NROW(value) != mf_ncol(mf)) {
    stop(
      "Number of rows in new cinfo (", NROW(value), ") does not match ",
      "number of columns (", mf_ncol(mf), ")"
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
mf_has_rlabels <- function(mf) ncol(mf$strings) > mf_ncol(mf)

#' Create spoof matrix form from a data frame
#'
#' Useful functions for writing tests and examples, and a starting point for
#' more sophisticated custom `matrix_form` methods.
#'
#' @inheritParams open_font_dev
#' @param df (`data.frame`)\cr a data frame.
#' @param indent_rownames (`flag`)\cr whether row names should be indented. Being this
#'   used for testing purposes, it defaults to `FALSE`. If `TRUE`, it assigns label rows
#'   on even lines (also format is `"-"` and value strings are `""`). Indentation works
#'   only if split labels are used (see parameters `split_labels` and `data_labels`).
#' @param parent_path (`string`)\cr parent path that all rows should be "children of".
#'   Defaults to `NULL`, as usually this is not needed. It may be necessary to use `"root"`,
#'   for some specific scenarios.
#' @param ignore_rownames (`flag`)\cr whether row names should be ignored.
#' @param add_decoration (`flag`)\cr whether adds title and footer decorations should
#'   be added to the matrix form.
#' @param split_labels (`string`)\cr indicates which column to use as split labels. If
#'   `NULL`, no split labels are used.
#' @param data_labels (`string`)\cr indicates which column to use as data labels. It is
#'   ignored if no `split_labels` is present and is automatically assigned to
#'   `"Analysis method"` when `split_labels` is present, but `data_labels` is `NULL`.
#'   Its direct column name is used as node name in `"DataRow"` pathing. See [mf_rinfo()]
#'   for more information.
#' @param num_rep_cols (`numeric(1)`)\cr Number of columns to be treated as repeating columns.
#'   Defaults to `0` for `basic_matrix_form` and `length(keycols)` for
#'   `basic_listing_mf`. Note repeating columns are separate from row labels if present.
#'
#' @return A valid `MatrixPrintForm` object representing `df` that is ready for
#'   ASCII rendering.
#'
#' @details
#' If some of the column has a [obj_format] assigned, it will be respected for all column
#' values except for label rows, if present (see parameter `split_labels`).
#'
#' @examples
#' mform <- basic_matrix_form(mtcars)
#' cat(toString(mform))
#'
#' # Advanced test case with label rows
#' library(dplyr)
#' iris_output <- iris %>%
#'   group_by(Species) %>%
#'   summarize("all obs" = round(mean(Petal.Length), 2)) %>%
#'   mutate("DataRow_label" = "Mean")
#' mf <- basic_matrix_form(iris_output,
#'   indent_rownames = TRUE,
#'   split_labels = "Species", data_labels = "DataRow_label"
#' )
#' cat(toString(mf))
#'
#' @name test_matrix_form
#' @export
basic_matrix_form <- function(df,
                              indent_rownames = FALSE,
                              parent_path = NULL,
                              ignore_rownames = FALSE,
                              add_decoration = FALSE,
                              fontspec = font_spec(),
                              split_labels = NULL,
                              data_labels = NULL,
                              num_rep_cols = 0L) {
  checkmate::assert_data_frame(df)
  checkmate::assert_flag(indent_rownames)
  checkmate::assert_character(parent_path, null.ok = TRUE)
  checkmate::assert_flag(ignore_rownames)
  checkmate::assert_flag(add_decoration)
  checkmate::assert_character(split_labels, null.ok = TRUE)
  checkmate::assert_character(data_labels, null.ok = TRUE)

  # Some defaults
  row_classes <- "DataRow" # Default for all rows
  data_row_format <- "xx" # Default if no labels are used
  indent_size <- 2
  indent_space <- paste0(rep(" ", indent_size), collapse = "")

  # Pre-processing the fake split
  if (!is.null(split_labels)) {
    checkmate::assert_choice(split_labels, colnames(df))
    label_rows <- as.character(df[[split_labels]])
    if (is.null(data_labels)) {
      data_rows <- rep("Analysis Method", nrow(df))
      data_labels <- "Analyzed Variable"
    } else {
      checkmate::assert_choice(data_labels, colnames(df))
      data_rows <- as.character(df[[data_labels]])
    }
    rnms_special <- c(rbind(label_rows, data_rows))
    row_classes <- c(rbind(
      rep("LabelRow", length(label_rows)),
      rep("DataRow", length(data_rows))
    ))
    data_colnm <- setdiff(colnames(df), c(split_labels, data_labels))
    tmp_df <- NULL
    for (col_i in seq_along(data_colnm)) {
      lbl_and_dt <- c(rbind(rep("", length(label_rows)), df[[data_colnm[col_i]]]))
      tmp_df <- cbind(tmp_df, lbl_and_dt)
    }
    colnames(tmp_df) <- data_colnm
    rownames(tmp_df) <- NULL
    df <- as.data.frame(tmp_df)
    ignore_rownames <- FALSE
  }

  # Formats
  fmts <- lapply(df, function(x) {
    if (is.null(obj_format(x))) {
      fmt_tmp <- data_row_format
    } else {
      fmt_tmp <- obj_format(x) # Can be assigned for each column
    }
    out <- rep(fmt_tmp, NROW(df))
    if (!is.null(split_labels)) {
      out[row_classes == "LabelRow"] <- "-"
    }
    out
  })

  formats <- rbind("", data.frame(fmts))
  if (!ignore_rownames) {
    formats <- cbind("rnms" = "", formats)
  }

  # Strings
  bodystrs <- mapply(function(x, coli_fmt) {
    coli_fmt[coli_fmt == "-"] <- "xx"
    sapply(seq_along(x), function(y) {
      format_value(x[y], format = coli_fmt[y])
    })
  }, x = df, coli_fmt = fmts)

  if (!ignore_rownames) {
    rnms <- row.names(df)
    if (!is.null(split_labels)) {
      # This overload is done because identical rownames not allowed (e.g. Mean.1 Mean.2)
      rnms <- rnms_special
    } else if (is.null(rnms)) {
      rnms <- as.character(seq_len(NROW(df)))
    }
  }

  cnms <- names(df)
  strings <- rbind(colnames(df), bodystrs)

  rownames(strings) <- NULL
  if (!ignore_rownames) {
    strings <- cbind("rnms" = c("", rnms), strings)
  }
  # colnames(strings) <- NULL # to add after fixing basic_mf for listings

  # Spans
  spans <- matrix(1, nrow = nrow(strings), ncol = ncol(strings))

  # Aligns
  # Default alignment is left for rownames column and center for the rest
  aligns <- matrix("center",
    nrow = NROW(strings),
    ncol = NCOL(strings) - as.numeric(!ignore_rownames)
  )
  if (!ignore_rownames) {
    aligns <- cbind("left", aligns)
  }

  # Row Info: build up fake pagination df
  charcols <- which(sapply(df, is.character))
  if (length(charcols) > 0) {
    exts <- apply(df[, charcols, drop = FALSE], 1, function(x) max(vapply(x, nlines, fontspec = fontspec, 1L)))
  } else {
    exts <- rep(1L, NROW(df))
  }
  # Constructing path roughly
  if (!is.null(split_labels)) {
    paths <- lapply(
      seq_along(rnms),
      function(row_path_i) {
        if (row_classes[row_path_i] == "DataRow") {
          c(
            split_labels,
            rnms[row_path_i - 1], # LabelRow before
            data_labels,
            rnms[row_path_i]
          )
        } else {
          c(split_labels, rnms[row_path_i])
        }
      }
    )
  } else {
    rnms <- row.names(df)
    if (is.null(rnms)) {
      rnms <- as.character(seq_len(NROW(df)))
    }
    paths <- lapply(rnms, function(x) c(parent_path, x))
  }
  rowdf <- basic_pagdf(
    rnames = rnms,
    extents = exts,
    rclass = row_classes,
    parent_path = NULL, # Overloaded by above parent_path lapply
    paths = paths
  )

  # Indentation happens last so to be sure we have all ready (only strings and formats change)
  if (indent_rownames && !is.null(split_labels)) {
    where_to_indent <- which(row_classes == "DataRow") + 1 # +1 because of colnames
    strings[where_to_indent, 1] <- paste0(indent_space, strings[where_to_indent, 1])
    formats[where_to_indent, 1] <- paste0(indent_space, formats[where_to_indent, 1]) # Needs fixing
    rowdf$indent[where_to_indent - 1] <- 1 # -1 because only rows
  }

  ret <- MatrixPrintForm(
    strings = strings,
    aligns = aligns,
    spans = spans,
    formats = formats, ## matrix("xx", nrow = fnr, ncol = fnc),
    row_info = rowdf,
    has_topleft = FALSE,
    nlines_header = 1,
    nrow_header = 1,
    has_rowlabs = isFALSE(ignore_rownames),
    fontspec = fontspec,
    col_gap = 3,
    indent_size = indent_size,
    rep_cols = num_rep_cols
  )

  # Check for ncols
  stopifnot(mf_has_rlabels(ret) == isFALSE(ignore_rownames))

  ret <- mform_build_refdf(ret)

  if (add_decoration) {
    main_title(ret) <- "main title"
    main_footer(ret) <- c("main", "  footer")
    prov_footer(ret) <- "prov footer"
    subtitles(ret) <- c("sub", "titles")
  }

  ret
}

#' @describeIn test_matrix_form Create a `MatrixPrintForm` object from data frame `df` that
#'   respects the default formats for a listing object.
#'
#' @param keycols (`character`)\cr a vector of `df` column names that are printed first and for which
#'   repeated values are assigned `""`. This format is characteristic of a listing matrix form.
#'
#' @return A valid `MatrixPrintForm` object representing `df` as a listing that is ready for ASCII
#'   rendering.
#'
#' @examples
#' mform <- basic_listing_mf(mtcars)
#' cat(toString(mform))
#'
#' @export
basic_listing_mf <- function(df,
                             keycols = names(df)[1],
                             add_decoration = TRUE) {
  checkmate::assert_data_frame(df)
  checkmate::assert_subset(keycols, colnames(df))

  dfmf <- basic_matrix_form(
    df = df,
    indent_rownames = FALSE,
    ignore_rownames = TRUE,
    add_decoration = add_decoration,
    num_rep_cols = length(keycols)
  )

  # keycols addition to MatrixPrintForm (should happen in the constructor)
  dfmf$listing_keycols <- keycols

  # Modifications needed for making it a listings
  mf_strings(dfmf)[1, ] <- colnames(mf_strings(dfmf)) # set colnames

  if (!is.null(keycols)) {
    str_dfmf <- mf_strings(dfmf)[-1, ]
    # Ordering
    ord <- do.call(
      order,
      as.list(
        data.frame(
          str_dfmf[, keycols]
        )
      )
    )
    str_dfmf <- str_dfmf[ord, ]
    # Making keycols with empties
    curkey <- ""
    for (i in seq_along(keycols)) {
      kcol <- keycols[i]
      kcolvec <- str_dfmf[, kcol] # -1 is col label row
      str_dfmf[, kcol] <- ""
      kcolvec <- vapply(kcolvec, format_value, "", format = NULL, na_str = "NA")
      curkey <- paste0(curkey, kcolvec)
      disp <- c(TRUE, tail(curkey, -1) != head(curkey, -1))
      str_dfmf[disp, kcol] <- kcolvec[disp]
    }
    mf_strings(dfmf)[-1, ] <- str_dfmf
    # keycols as first
    mf_strings(dfmf) <- cbind(
      mf_strings(dfmf)[, keycols, drop = FALSE],
      mf_strings(dfmf)[, !colnames(mf_strings(dfmf)) %in% keycols, drop = FALSE]
    )
  }

  dfmf$aligns[seq(2, nrow(dfmf$aligns)), ] <- "center" # the default for listings

  # the default for listings is a 1 double??
  dfmf$formats <- matrix(1, nrow = nrow(dfmf$formats), ncol = ncol(dfmf$formats))

  # row info
  ri <- dfmf$row_info
  rownames(ri) <- ri$abs_rownumber
  ri$label <- ri$name <- ""
  ri$path <- as.list(NA_character_) # same format of listings
  ri$node_class <- "listing_df"
  # l_ri$pos_in_siblings # why is it like this in rlistings?? also n_siblings
  class(ri$path) <- "AsIs" # Artifact from I()
  dfmf$row_info <- ri

  # colwidths need to be sorted too!!
  dfmf$col_widths <- dfmf$col_widths[colnames(mf_strings(dfmf))]

  if (!add_decoration) {
    # This is probably a forced behavior in the original matrix_form in rlistings
    main_title(dfmf) <- character()
    main_footer(dfmf) <- character()
  }

  dfmf
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

.mf_subset_core_mats <- function(mf, i, keycols = NULL, row = TRUE) {
  fillnum <- if (row) nrow(mf_strings(mf)) - mf_nlheader(mf) else mf_ncol(mf)
  if (is.logical(i) || all(i < 0)) {
    i <- seq_len(fillnum)[i]
  }
  nlh <- mf_nlheader(mf)

  if (row) {
    ncolrows <- mf_nrheader(mf)
    i_mat <- c(seq_len(nlh), which(mf_lgrouping(mf) %in% (i + ncolrows)))
    j_mat <- seq_len(ncol(mf_strings(mf)))
  } else {
    nlabcol <- as.integer(mf_has_rlabels(mf))
    i_mat <- seq_len(nrow(mf_strings(mf)))
    j_mat <- c(seq_len(nlabcol), i + nlabcol)
  }

  tmp_strmat <- mf_strings(mf)[i_mat, j_mat, drop = FALSE]

  # Only for listings - Fix pagination with empty values in key columns
  if (nrow(tmp_strmat) > 0 && .is_listing_mf(mf)) { # safe check for empty listings
    ind_keycols <- which(colnames(tmp_strmat) %in% keycols)

    # Fix for missing labels in key columns (only for rlistings)
    empty_keycols <- !nzchar(tmp_strmat[-seq_len(nlh), ind_keycols, drop = FALSE][1, ])

    if (any(empty_keycols)) { # only if there are missing keycol labels
      # find the first non-empty label in the key columns
      keycols_needed <- mf_strings(mf)[, empty_keycols, drop = FALSE]
      first_nonempty <- apply(keycols_needed, 2, function(x) {
        section_ind <- i_mat[-seq_len(nlh)][1]
        sec_ind_no_header <- seq_len(section_ind)[-seq_len(nlh)]
        tail(x[sec_ind_no_header][nzchar(x[sec_ind_no_header])], 1)
      })

      # if there are only "" the previous returns character()
      any_chr_empty <- if (length(first_nonempty) > 1) {
        vapply(first_nonempty, length, numeric(1))
      } else {
        length(first_nonempty)
      }
      if (any(any_chr_empty == 0L)) {
        warning(
          "There are empty key columns in the listing. ",
          "We keep empty strings for each page."
        )
        first_nonempty[any_chr_empty == 0L] <- ""
      }

      # replace the empty labels with the first non-empty label
      tmp_strmat[nlh + 1, empty_keycols] <- unlist(first_nonempty)
    }
  }

  mf_strings(mf) <- tmp_strmat

  mf_lgrouping(mf) <- as.integer(as.factor(mf_lgrouping(mf)[i_mat]))

  if (!row) {
    newspans <- truncate_spans(mf_spans(mf), j_mat) # 'i' is the columns here, bc row is FALSE
  } else {
    newspans <- mf_spans(mf)[i_mat, j_mat, drop = FALSE]
  }

  mf_spans(mf) <- newspans
  mf_formats(mf) <- mf_formats(mf)[i_mat, j_mat, drop = FALSE]

  mf_aligns(mf) <- mf_aligns(mf)[i_mat, j_mat, drop = FALSE]
  if (!row) {
    mf_ncol(mf) <- length(i)
    if (!is.null(mf_cinfo(mf))) {
      mf_cinfo(mf) <- mf_cinfo(mf)[i, ]
    }
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
  if (length(spans[1, ]) == 1 || length(j) == 1) {
    as.matrix(apply(spans, 1, truncate_one_span, j = j))
  } else {
    t(apply(spans, 1, truncate_one_span, j = j))
  }
}

mpf_subset_rows <- function(mf, i, keycols = NULL) {
  nlh <- mf_nlheader(mf)
  lgrps <- mf_lgrouping(mf)
  row_lgrps <- tail(lgrps, -1 * nlh)
  nrs <- length(unique(row_lgrps))
  ncolrows <- length(unique(lgrps[seq_len(nlh)]))

  ncs <- mf_ncol(mf)
  mf <- .mf_subset_core_mats(mf, i, keycols = keycols, row = TRUE)
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
mpf_subset_cols <- function(mf, j, keycols = NULL) {
  nc <- mf_ncol(mf)
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

  mf <- .mf_subset_core_mats(mf, j, keycols = keycols, row = FALSE)

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
