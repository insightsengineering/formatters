## #' Page Dimensions
## #'
## #' Dimensions for mapping page dimensions to text dimensions
## #' @references https://www.ietf.org/rfc/rfc0678.txt
## #' @export
## #' @rdname pagedims
## lpi_vert <- 6
## #' @export
## #' @rdname pagedims
## cpi_horiz <- 10
## #' @export
## #' @rdname pagedims
## horiz_margin_chars <- 13
## #' @export
## #' @rdname pagedims
## horiz_margin_inches <- horiz_margin_chars / cpi_horiz
## #' @export
## #' @rdname pagedims
## vert_margin_lines <- 6
## #' @export
## #' @rdname pagedims
## vert_margin_inches <- vert_margin_lines / lpi_vert

## #' Physical Page dimensions to chars x lines
## #'
## #' Calculate number of lines long and characters wide a page size is,
## #' after excluding margins
## #' @export
## #' @examples
## #' phys_page_to_lc()
## phys_page_to_lc <- function(width = 8.5, len = 11,
##                             h_margin = horiz_margin_inches,
##                             v_margin = vert_margin_inches) {
##     lgl_width <- width - h_margin
##     lgl_len <- len - v_margin
##     c(chars_wide = floor(lgl_width * cpi_horiz),
##       lines_long = floor(lgl_len * lpi_vert))
## }

#' @name pagination_algo
#' @rdname pagination_algo
#' @title Pagination
#' @section Pagination Algorithm:
#'
#' Pagination  is performed independently in  the vertical  and horizontal
#' directions based solely on a *pagination data.frame*, which includes the
#' following information for each row/column:
#'
#'  - number of  lines/characters rendering the row  will take **after
#'    word-wrapping** (`self_extent`)
#'  - the indices (`reprint_inds`)  and number of lines (`par_extent`)
#'    of the rows which act as **context** for the row
#'  - the row's number of siblings and position within its siblings
#'
#' Given `lpp`  (`cpp`) already  adjusted for rendered  elements which
#' are  not rows/columns  and a  dataframe of  pagination information,
#' pagination is  performed via  the following  algorithm, and  with a
#' `start = 1`:
#'
#' Core Pagination Algorithm:
#' 1. Initial guess for pagination point is `start + lpp` (`start + cpp`)
#'
#' 2. While the guess is not a valid pagination position, and `guess >
#'    start`, decrement guess and repeat
#'   - an error is thrown if all possible pagination positions between
#'     `start` and `start + lpp` (`start + cpp`) would ever be `< start`
#'     after decrementing
#' 3. Retain pagination index
#' 4. if pagination  point was less than  `NROW(tt)` (`ncol(tt)`), set
#'    `start` to `pos + 1`, and repeat steps (1) - (4).
#'
#' Validating pagination position:
#'
#' Given an (already adjusted) `lpp` or `cpp` value, a pagination is invalid if:
#'
#'   - The rows/columns on the page would take  more than (adjusted) `lpp` lines/`cpp`
#'     characters to render **including**
#'     - word-wrapping
#'     - (vertical only) context repetition
#'   - (vertical only) footnote messages  and or section divider lines
#'     take up too many lines after rendering rows
#'   - (vertical only) row is a label or content (row-group summary) row
#'   - (vertical only)  row at the pagination point  has siblings, and
#'     it has less than `min_siblings` preceding or following siblings
#'   - pagination would occur within a sub-table listed in `nosplitin`
#'
NULL

#' Create row of pagination data frame
#' @param nm character(1). Name
#' @param lab character(1). Label
#' @param rnum numeric(1). Absolute rownumber
#' @param pth character or NULL. Path within larger table
#' @param sibpos integer(1). Position among sibling rows
#' @param nsibs integer(1). Number of siblings (including self).
#' @param extent numberic(1). Number of lines required to print the row
#' @param colwidths numeric. Column widths
#' @param repext integer(1). Number of lines required to reprint all context for this row if it appears directly
#'   after pagination.
#' @param repind integer. Vector of row numbers to be reprinted if this row appears directly after pagination.
#' @param indent integer. Indent
#' @param rclass character(1). Class of row object.
#' @param nrowrefs integer(1). Number of row referential footnotes for this row
#' @param ncellrefs integer(1). Number of cell referential footnotes for the cells in this row
#' @param nreflines integer(1). Total number of lines required by all referential footnotes
#' @param force_page logical(1). Currently Ignored.
#' @param page_title logical(1). Currently Ignored.
#' @param trailing_sep character(1). The string to used as a separator below this row during printing (or
#' `NA_character_` for no separator).
#' @param row ANY. Object representing the row, which is used for default values of \code{nm}, \code{lab},
#' \code{extent} and \code{rclass} if provided. Must have methods for \code{obj_name}, \code{obj_label},
#' and \code{nlines}, respectively, for default values of \code{nm}, \code{lab} and \code{extent} to
#' be retrieved, respectively.
#'
#' @return a single row data.frame with the columns appropriate for a pagination info data frame.
#' @export
pagdfrow <- function(row,
                     nm = obj_name(row),
                     lab = obj_label(row),
                     rnum,
                     pth,
                     sibpos = NA_integer_,
                     nsibs = NA_integer_,
                     extent = nlines(row, colwidths),
                     colwidths = NULL,
                     repext = 0L,
                     repind = integer(),
                     indent = 0L,
                     rclass = class(row),
                     nrowrefs = 0L,
                     ncellrefs = 0L,
                     nreflines = 0L,
                     force_page = FALSE,
                     page_title = NA_character_,
                     trailing_sep = NA_character_) {
  data.frame(
    label = lab,
    name = nm,
    abs_rownumber = rnum,
    path = I(list(pth)),
    pos_in_siblings = sibpos,
    n_siblings = nsibs,
    self_extent = extent,
    par_extent = repext,
    reprint_inds = I(rep(list(unlist(repind)), length.out = length(nm))),
    node_class = rclass,
    indent = max(0L, indent),
    nrowrefs = nrowrefs,
    ncellrefs = ncellrefs,
    nreflines = nreflines,
    force_page = force_page,
    page_title = page_title,
    trailing_sep = trailing_sep,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


valid_pag <- function(pagdf,
                      guess,
                      start,
                      rlpp,
                      min_sibs,
                      nosplit = NULL,
                      div_height = 1L,
                      verbose = FALSE,
                      row = TRUE,
                      have_col_fnotes = FALSE) {
  rw <- pagdf[guess, ]


  if (verbose) {
    message(
      "Checking pagination after ",
      paste(ifelse(row, "row", "column"), guess)
    )
  }
  rowlines <- sum(pagdf[start:guess, "self_extent"])
  reflines <- sum(pagdf[start:guess, "nreflines"])
  rowlines <- sum(pagdf[start:guess, "self_extent"]) - reflines ## self extent includes reflines
  ## self extent does ***not*** currently include trailing sep
  ## we don't include the trailing_sep for guess because if we paginate here it won't be printed
  sectlines <- if (start == guess) 0L else sum(!is.na(pagdf[start:(guess - 1), "trailing_sep"]))
  if (reflines > 0) reflines <- reflines + if (have_col_fnotes) 0L else div_height + 1L
  lines <- rowlines + reflines + sectlines # guess - start + 1 because inclusive of start
  rep_ext <- pagdf$par_extent[start]
  if (rowlines > rlpp) {
    if (verbose) {
      message(sprintf(
        "\t....................... FAIL: %s take up too much space (%d %s)",
        ifelse(row, "rows", "columns"), rowlines + rep_ext,
        ifelse(row, "lines", "characters")
      ))
    }
    return(FALSE)
  }
  if (lines > rlpp) {
    if (verbose) {
      message(
        "\t....................... FAIL: Referential footnotes (",
        reflines,
        ") or section dividers (",
        sectlines,
        ") take up too much space."
      )
    }
    return(FALSE)
  }
  if (rw[["node_class"]] %in% c("LabelRow", "ContentRow")) {
    if (verbose) {
      message("\t....................... FAIL: last row is a label or content row")
    }
    return(FALSE)
  }

  sibpos <- rw[["pos_in_siblings"]]
  nsib <- rw[["n_siblings"]]
  # okpos <- min(min_sibs + 1, rw[["n_siblings"]])
  if (sibpos != nsib) {
    retfalse <- FALSE
    if (sibpos < min_sibs + 1) {
      retfalse <- TRUE
      if (verbose) {
        message(
          "\t....................... FAIL: last row had only ", sibpos - 1,
          " preceding siblings, needed ", min_sibs
        )
      }
    } else if (nsib - sibpos < min_sibs + 1) {
      retfalse <- TRUE
      if (verbose) {
        message(
          "\t....................... FAIL: last row had only ", nsib - sibpos - 1,
          " following siblings, needed ", min_sibs
        )
      }
    }
    if (retfalse) {
      return(FALSE)
    }
  }
  if (guess < nrow(pagdf)) {
    curpth <- unlist(rw$path)
    nxtpth <- unlist(pagdf$path[[guess + 1]])
    inplay <- nosplit[(nosplit %in% intersect(curpth, nxtpth))]
    if (length(inplay) > 0) {
      curvals <- curpth[match(inplay, curpth) + 1]
      nxtvals <- nxtpth[match(inplay, nxtpth) + 1]
      if (identical(curvals, nxtvals)) {
        if (verbose) {
          message(
            "\t....................... FAIL: values of unsplitable vars before [", curvals,
            "] and after [", nxtvals, "] match"
          )
        }
        return(FALSE)
      }
    }
  }
  if (verbose) {
    message("\t....................... OK [", lines + rep_ext, if (row) " lines]" else " chars]")
  }
  TRUE
}


find_pag <- function(pagdf,
                     start,
                     guess,
                     rlpp,
                     min_siblings,
                     nosplitin = character(),
                     verbose = FALSE,
                     row = TRUE,
                     have_col_fnotes = FALSE,
                     div_height = 1L) {
  origuess <- guess
  while (guess >= start && !valid_pag(pagdf, guess,
    start = start, rlpp = rlpp, min_sibs = min_siblings,
    nosplit = nosplitin, verbose, row = row, have_col_fnotes = have_col_fnotes,
    div_height = div_height
  )) {
    guess <- guess - 1
  }
  if (guess < start) {
    stop("Unable to find any valid pagination between ", start, " and ", origuess)
  }
  guess
}


#' Find Pagination Indices From Pagination Info Dataframe
#'
#' Pagination methods should typically call the `make_row_df` method
#' for their object and then call this function on the resulting
#' pagination info data.frame.
#'
#' @details `pab_indices_inner` implements the Core Pagination Algorithm
#' for a single direction (vertical if `row = TRUE`, the default, horizontal otherwise)
#' based on the pagination dataframe and (already adjusted for non-body rows/columns)
#' lines (or characters) per page.
#'
#' @inheritSection pagination_algo Pagination Algorithm
#' @param pagdf data.frame. A pagination info data.frame as created by
#' either `make_rows_df` or `make_cols_df`.
#' @param rlpp numeric. Maximum number of \emph{row} lines per page (not including header materials), including
#'   (re)printed header and context rows
#' @param min_siblings  numeric. Minimum sibling rows which must appear on either side of pagination row for a
#'   mid-subtable split to be valid. Defaults to 2.
#' @param nosplitin character. List of names of sub-tables where page-breaks are not allowed, regardless of other
#'   considerations. Defaults to none.
#' @param verbose logical(1). Should additional informative messages about the search for
#' pagination breaks be shown. Defaults to \code{FALSE}.
#' @param row logical(1). Is pagination happening in row
#' space (`TRUE`, the default) or column space (`FALSE`)
#' @param have_col_fnotes logical(1). Does the table-like object being rendered have
#' column-associated referential footnotes.
#' @param div_height numeric(1). The height of the divider line when the
#' associated object is rendered. Defaults to `1`.
#' @return A list containing the vector of row numbers, broken up by page
#'
#' @examples
#' mypgdf <- basic_pagdf(row.names(mtcars))
#'
#' paginds <- pag_indices_inner(mypgdf, rlpp = 15, min_siblings = 0)
#' lapply(paginds, function(x) mtcars[x, ])
#'
#' @export
pag_indices_inner <- function(pagdf, rlpp,
                              min_siblings,
                              nosplitin = character(),
                              verbose = FALSE,
                              row = TRUE,
                              have_col_fnotes = FALSE,
                              div_height = 1L) {
  start <- 1
  nr <- nrow(pagdf)
  ret <- list()
  while (start <= nr) {
    adjrlpp <- rlpp - pagdf$par_extent[start]
    if (adjrlpp <= 0) {
      if (row) {
        stop("Lines of repeated context (plus header materials) larger than specified lines per page")
      } else {
        stop("Width of row labels equal to or larger than specified characters per page.")
      }
    }
    guess <- min(nr, start + adjrlpp - 1)
    end <- find_pag(pagdf, start, guess,
      rlpp = adjrlpp,
      min_siblings = min_siblings,
      nosplitin = nosplitin,
      verbose = verbose,
      row = row,
      have_col_fnotes = have_col_fnotes,
      div_height = div_height
    )
    ret <- c(ret, list(c(
      pagdf$reprint_inds[[start]],
      start:end
    )))
    start <- end + 1
  }
  ret
}

#' Find Column Indices for Vertical Pagination
#' @param  obj   ANY.  object   to   be  paginated.   Must  have   a
#'     \code{\link{matrix_form}} method.
#' @param cpp numeric(1). Number of characters per page (width)
#' @param colwidths numeric vector.  Column widths (in characters) for
#'     use with vertical pagination.
#' @param rep_cols numeric(1). Number of \emph{columns} (not including
#'     row labels) to be repeated on every page. Defaults to 0
#' @inheritParams pag_indices_inner
#'
#' @return A list partitioning the vector of column indices
#' into subsets for 1 or more horizontally paginated pages.
#'
#' @examples
#' mf <- basic_matrix_form(df = mtcars)
#' colpaginds <- vert_pag_indices(mf)
#' lapply(colpaginds, function(j) mtcars[, j, drop = FALSE])
#' @export
vert_pag_indices <- function(obj, cpp = 40, colwidths = NULL, verbose = FALSE, rep_cols = 0L) {
  strm <- matrix_form(obj, TRUE)

  clwds <- colwidths %||% propose_column_widths(strm)
  if (!is(rep_cols, "numeric") || is.na(rep_cols) || rep_cols < 0) {
    stop("got invalid number of columns to be repeated: ", rep_cols)
  }
  has_rlabs <- mpf_has_rlabels(strm)
  rlabs_flag <- as.integer(has_rlabs)
  rlab_extent <- if (has_rlabs) clwds[1] else 0L
  # has_repc <- rep_cols > 0L
  sqstart <- rlabs_flag + 1L # rep_cols + 1L
  ## rep_extent <- 0L
  ## if(has_repc) {
  ##     rep_extent <- sum(clwds[rlabs_flag + seq_len(rep_cols)]) +
  ##         strm$col_gap * rep_cols
  ## }
  pdfrows <- lapply(
    (sqstart):ncol(strm$strings),
    function(i) {
      rownum <- i - rlabs_flag
      rep_inds <- seq_len(rep_cols)[seq_len(rep_cols) < rownum]
      rep_extent_i <- sum(0L, clwds[rlabs_flag + rep_inds]) + strm$col_gap * length(rep_inds)
      pagdfrow(
        row = NA,
        nm = rownum,
        lab = rownum,
        rnum = rownum,
        pth = NA,
        extent = clwds[i] + strm$col_gap,
        repext = rep_extent_i, # sum(clwds[rep_cols]) + strm$col_gap * max(0, (length(rep_cols) - 1)),
        repind = rep_inds, # rep_cols,
        rclass = "stuff",
        sibpos = 1 - 1,
        nsibs = 1 - 1
      )
    }
  )
  pdf <- do.call(rbind, pdfrows)
  # rep_extent <- pdf$par_extent[nrow(pdf)]
  rcpp <- cpp - table_inset(strm) - rlab_extent # rep_extent - table_inset(strm) - rlab_extent
  if (verbose) {
    message(
      "Adjusted characters per page: ", rcpp,
      " [original: ", cpp,
      ", table inset: ", table_inset(strm), if (has_rlabs) paste0(", row labels: ", clwds[1]),
      "]"
    )
  }
  res <- pag_indices_inner(pdf,
    rlpp = rcpp, # cpp - sum(clwds[seq_len(rep_cols)]),
    verbose = verbose,
    min_siblings = 1,
    row = FALSE
  )
  res
}


#' Basic/spoof pagination info dataframe
#'
#' Returns a minimal pagination info data.frame (with no sibling/footnote/etc info).
#'
#' @param rnames character. Vector of row names
#' @param labs character. Vector of row labels (defaults to names)
#' @param rnums integer. Vector of row numbers. Defaults to `seq_along(rnames)`.
#' @param extents integer. Number of lines each row will take to print, defaults to 1 for all rows
#' @param rclass character. Class(es) for the rows. Defaults to "NA"
#'
#' @return A data.frame suitable for use in both the `matrix_print_form` constructor and the pagination machinery
#'
#' @examples
#'
#' basic_pagdf(c("hi", "there"))
#' @export
basic_pagdf <- function(rnames, labs = rnames, rnums = seq_along(rnames),
                        extents = 1L,
                        rclass = "NA") {
  rws <- mapply(pagdfrow,
    nm = rnames, lab = labs, extent = extents,
    rclass = rclass, rnum = rnums, pth = rnames,
    SIMPLIFY = FALSE, nsibs = 1, sibpos = 1
  )
  do.call(rbind.data.frame, rws)
}
