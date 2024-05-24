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

#' Pagination
#'
#' @section Pagination Algorithm:
#'
#' Pagination is performed independently in the vertical and horizontal
#' directions based solely on a *pagination data frame*, which includes the
#' following information for each row/column:
#'
#'   - Number of lines/characters rendering the row will take **after
#'     word-wrapping** (`self_extent`)
#'   - The indices (`reprint_inds`) and number of lines (`par_extent`)
#'     of the rows which act as **context** for the row
#'   - The row's number of siblings and position within its siblings
#'
#' Given `lpp` (`cpp`) is already adjusted for rendered elements which
#' are not rows/columns and a data frame of pagination information,
#' pagination is performed via the following algorithm with `start = 1`.
#'
#' Core Pagination Algorithm:
#'
#' 1. Initial guess for pagination position is `start + lpp` (`start + cpp`)
#' 2. While the guess is not a valid pagination position, and `guess > start`,
#'    decrement guess and repeat.
#'      - An error is thrown if all possible pagination positions between
#'        `start` and `start + lpp` (`start + cpp`) would be `< start`
#'        after decrementing
#' 3. Retain pagination index
#' 4. If pagination point was less than `NROW(tt)` (`ncol(tt)`), set
#'    `start` to `pos + 1`, and repeat steps (1) - (4).
#'
#' Validating Pagination Position:
#'
#' Given an (already adjusted) `lpp` or `cpp` value, a pagination is invalid if:
#'
#'   - The rows/columns on the page would take more than (adjusted) `lpp` lines/`cpp`
#'     characters to render **including**:
#'       - word-wrapping
#'       - (vertical only) context repetition
#'   - (vertical only) footnote messages and/or section divider lines
#'     take up too many lines after rendering rows
#'   - (vertical only) row is a label or content (row-group summary) row
#'   - (vertical only) row at the pagination point has siblings, and
#'     it has less than `min_siblings` preceding or following siblings
#'   - pagination would occur within a sub-table listed in `nosplitin`
#'
#' @name pagination_algo
NULL

#' Create a row of a pagination data frame
#'
#' @param nm (`string`)\cr name.
#' @param lab (`string`)\cr label.
#' @param rnum (`numeric(1)`)\cr absolute row number.
#' @param pth (`character` or `NULL`)\cr path within larger table.
#' @param sibpos (`integer(1)`)\cr position among sibling rows.
#' @param nsibs (`integer(1)`)\cr number of siblings (including self).
#' @param extent (`numeric(1)`)\cr number of lines required to print the row.
#' @param colwidths (`numeric`)\cr column widths.
#' @param repext (`integer(1)`)\cr number of lines required to reprint all context for this row if it appears directly
#'   after pagination.
#' @param repind (`integer`)\cr vector of row numbers to be reprinted if this row appears directly after pagination.
#' @param indent (`integer`)\cr indent.
#' @param rclass (`string`)\cr class of row object.
#' @param nrowrefs (`integer(1)`)\cr number of row referential footnotes for this row.
#' @param ncellrefs (`integer(1)`)\cr number of cell referential footnotes for the cells in this row.
#' @param nreflines (`integer(1)`)\cr total number of lines required by all referential footnotes.
#' @param force_page (`flag`)\cr currently ignored.
#' @param page_title (`flag`)\cr currently ignored.
#' @param trailing_sep (`string`)\cr the string to use as a separator below this row during printing.
#'   If `NA_character_`, no separator is used.
#' @param row (`ANY`)\cr object representing the row, which is used for default values of `nm`, `lab`,
#'   `extent`, and `rclass` if provided. Must have methods for `obj_name`, `obj_label`, and `nlines`, to retrieve
#'   default values of `nm`, `lab`, and `extent`, respectively.
#'
#' @return A single row `data.frame` with the appropriate columns for a pagination info data frame.
#'
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
                     # ref_df = .make_ref_df(NULL, NULL),
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
    # ref_info_df = I(list(ref_df)),
    force_page = force_page,
    page_title = page_title,
    trailing_sep = trailing_sep,
    stringsAsFactors = FALSE,
    row.names = NULL,
    check.names = FALSE,
    fix.empty.names = FALSE
  )
}

calc_ref_nlines_df <- function(pagdf) {
  ## XXX XXX XXX this is dangerous and wrong!!!
  if (is.null(pagdf$ref_info_df) && sum(pagdf$nreflines) == 0) {
    return(ref_df_row()[0, ])
  }
  refdf <- do.call(rbind.data.frame, pagdf$ref_info_df)
  if (NROW(refdf) == 0) {
    return(ref_df_row()[0, ])
  }
  unqsyms <- !duplicated(refdf$symbol)
  refdf[unqsyms, , drop = FALSE]
}

build_fail_msg <- function(row, lines, raw_rowlines,
                           allowed_lines, lpp, decoration_lines,
                           start, guess, rep_ext, n_reprint,
                           reflines, n_refs, sectlines) {
  if (row) {
    spacetype <- "lines"
    spacetype_abr <- "lns"
    structtype_abr <- "rws"
    sprintf(
      paste0(
        "  FAIL: rows selected for pagination require %d %s while only %d are available from ",
        "lpp = %d and %d header/footers lines.\n",
        "        details: [raw: %d %s (%d %s), rep. context: %d %s (%d %s), ",
        "refs: %d %s (%d) sect. divs: %d %s]."
      ),
      lines,
      spacetype,
      allowed_lines,
      lpp,
      decoration_lines, # header + footers
      raw_rowlines,
      spacetype_abr,
      guess - start + 1, # because it includes both start and guess
      structtype_abr,
      rep_ext,
      spacetype_abr,
      n_reprint,
      structtype_abr,
      reflines,
      spacetype_abr,
      n_refs,
      sectlines,
      spacetype_abr
    )
  } else { ## !row
    spacetype <- "chars"
    spacetype_abr <- "chars"
    structtype_abr <- "cols"
    sprintf(
      "  FAIL: selected columns require %d %s (%d %s).",
      lines,
      spacetype,
      guess - start + 1, # because it includes both start and guess
      structtype_abr
    )
  }
}

valid_pag <- function(pagdf,
                      guess,
                      start,
                      rlpp,
                      lpp, # for informational purposes only
                      context_lpp, # for informational purposes only (headers/footers)
                      min_sibs,
                      nosplit = NULL,
                      div_height = 1L,
                      verbose = FALSE,
                      row = TRUE,
                      have_col_fnotes = FALSE) {
  # FALSE output from this function means that another guess is taken till success or failure
  rw <- pagdf[guess, ]

  if (verbose) {
    message(
      "-> Attempting pagination between ", start, " and ", guess, " ",
      paste(ifelse(row, "row", "column"))
    )
  }

  # Fix for counting the right number of lines when there is wrapping on a keycols
  if (.is_listing_mf(pagdf) && !is.null(pagdf$self_extent_page_break)) {
    pagdf$self_extent[start] <- pagdf$self_extent_page_break[start]
  }

  raw_rowlines <- sum(pagdf[start:guess, "self_extent"] - pagdf[start:guess, "nreflines"])

  refdf_ii <- calc_ref_nlines_df(pagdf[start:guess, ])
  reflines <- if (row) sum(refdf_ii$nlines, 0L) else 0L
  if (reflines > 0 && !have_col_fnotes) {
    reflines <- reflines + div_height + 1L
  }

  ##  reflines <- sum(pagdf[start:guess, "nreflines"])
  rowlines <- raw_rowlines + reflines ## sum(pagdf[start:guess, "self_extent"]) - reflines
  ## self extent includes reflines
  ## self extent does ***not*** currently include trailing sep
  ## we don't include the trailing_sep for guess because if we paginate here it won't be printed
  sectlines <- if (start == guess) 0L else sum(!is.na(pagdf[start:(guess - 1), "trailing_sep"]))
  lines <- rowlines + sectlines # guess - start + 1 because inclusive of start
  rep_ext <- pagdf$par_extent[start]
  if (lines > rlpp) {
    if (verbose) {
      structtype <- ifelse(row, "rows", "columns")
      structtype_abr <- ifelse(row, "rows", "cols")
      spacetype <- ifelse(row, "lines", "chars")
      spacetype_abr <- ifelse(row, "lns", "chrs")
      msg <- build_fail_msg(
        row, lines, raw_rowlines,
        allowed_lines = rlpp, lpp = lpp, decoration_lines = context_lpp,
        start, guess, rep_ext, length(pagdf$reprint_inds[[start]]),
        reflines, NROW(refdf_ii), sectlines
      )
      message(msg)
    }
    return(FALSE)
  }

  # Special cases: is it a label or content row?
  if (rw[["node_class"]] %in% c("LabelRow", "ContentRow")) {
    # check if it has children; if no children then valid
    has_children <- rw$abs_rownumber %in% unlist(pagdf$reprint_inds)
    if (rw$abs_rownumber == nrow(pagdf)) {
      if (verbose) {
        message("  EXCEPTION: last row is a label or content row but in lpp")
      }
    } else if (!any(has_children)) {
      if (verbose) {
        message(
          "  EXCEPTION: last row is a label or content row\n",
          "but does not have rows and row groups depending on it"
        )
      }
    } else {
      if (verbose) {
        message("  FAIL: last row is a label or content row")
      }
      return(FALSE)
    }
  }

  # Siblings handling
  sibpos <- rw[["pos_in_siblings"]]
  nsib <- rw[["n_siblings"]]
  # okpos <- min(min_sibs + 1, rw[["n_siblings"]])
  if (sibpos != nsib) {
    retfalse <- FALSE
    if (sibpos < min_sibs + 1) {
      retfalse <- TRUE
      if (verbose) {
        message(
          "  FAIL: last row had only ", sibpos - 1,
          " preceding siblings, needed ", min_sibs
        )
      }
    } else if (nsib - sibpos < min_sibs + 1) {
      retfalse <- TRUE
      if (verbose) {
        message(
          "  FAIL: last row had only ", nsib - sibpos - 1,
          " following siblings, needed ", min_sibs
        )
      }
    }
    if (retfalse) {
      return(FALSE)
    }
  }
  if (guess < nrow(pagdf) && length(nosplit > 0)) {
    ## paths end at the leaf name which is *always* different
    curpth <- head(unlist(rw$path), -1)
    nxtpth <- head(unlist(pagdf$path[[guess + 1]]), -1)

    inplay <- nosplit[(nosplit %in% intersect(curpth, nxtpth))]
    if (length(inplay) > 0) {
      ok_split <- vapply(inplay, function(var) {
        !identical(curpth[match(var, curpth) + 1], nxtpth[match(var, nxtpth) + 1])
      }, TRUE)

      curvals <- curpth[match(inplay, curpth) + 1]
      nxtvals <- nxtpth[match(inplay, nxtpth) + 1]
      if (!all(ok_split)) {
        if (verbose) {
          message(
            "  FAIL: nosplit variable [",
            inplay[min(which(!ok_split))], "] would be constant [",
            curvals, "] across this pagebreak."
          )
        }
        return(FALSE)
      }
    }
  }

  # Usual output when found
  if (verbose) {
    message("  OK [", lines + rep_ext, if (row) " lines]" else " chars]")
  }
  TRUE
}

find_pag <- function(pagdf,
                     current_page,
                     start,
                     guess,
                     rlpp,
                     lpp_or_cpp,
                     context_lpp_or_cpp,
                     min_siblings,
                     nosplitin = character(),
                     verbose = FALSE,
                     row = TRUE,
                     have_col_fnotes = FALSE,
                     div_height = 1L,
                     do_error = FALSE) {
  if (verbose) {
    if (row) {
      message("--------- ROW-WISE: Checking possible pagination for page ", current_page)
    } else {
      message("========= COLUMN-WISE: Checking possible pagination for page ", current_page)
    }
  }

  origuess <- guess
  while (guess >= start && !valid_pag(
    pagdf, guess,
    start = start,
    rlpp = rlpp, lpp = lpp_or_cpp, context_lpp = context_lpp_or_cpp, # only lpp goes to row pagination
    min_sibs = min_siblings,
    nosplit = nosplitin, verbose, row = row,
    have_col_fnotes = have_col_fnotes,
    div_height = div_height
  )) {
    guess <- guess - 1
  }
  if (guess < start) {
    # Repeat pagination process to see what went wrong with verbose on
    if (isFALSE(do_error) && isFALSE(verbose)) {
      find_pag(
        pagdf = pagdf,
        current_page = current_page,
        start = start,
        guess = origuess,
        rlpp = rlpp, lpp_or_cpp = lpp_or_cpp, context_lpp_or_cpp = context_lpp_or_cpp,
        min_siblings = min_siblings,
        nosplitin = nosplitin,
        verbose = TRUE,
        row = row,
        have_col_fnotes = have_col_fnotes,
        div_height = div_height,
        do_error = TRUE # only used to avoid loop
      )
    }
    stop(
      "-------------------------------------- Error Summary ----------------------------------------\n",
      "Unable to find any valid pagination split for page ", current_page, " between ",
      ifelse(row, "rows ", "columns "), start, " and ", origuess, ". \n",
      "Inserted ", ifelse(row, "lpp (row-space, lines per page) ", "cpp (column-space, content per page) "),
      ": ", lpp_or_cpp, "\n",
      "Context-relevant additional ", ifelse(row, "header/footers lines", "fixed column characters"),
      ": ", context_lpp_or_cpp, "\n",
      ifelse(row,
        paste("Limit of allowed row lines per page:", rlpp, "\n"),
        paste("Check the minimum allowed column characters per page in the last FAIL(ed) attempt. \n")
      ),
      "Note: take a look at the last FAIL(ed) attempt above to see what went wrong. It could be, for example, ",
      "that the inserted column width induces some wrapping, hence the inserted number of lines (lpp) is not enough."
    )
  }
  guess
}

#' Find pagination indices from pagination info data frame
#'
#' Pagination methods should typically call the `make_row_df` method
#' for their object and then call this function on the resulting
#' pagination info `data.frame`.
#'
#' @param pagdf (`data.frame`)\cr a pagination info `data.frame` as created by
#'   either `make_rows_df` or `make_cols_df`.
#' @param rlpp (`numeric`)\cr maximum number of *row* lines per page (not including header materials), including
#'   (re)printed header and context rows.
#' @param lpp_or_cpp (`numeric`)\cr total maximum number of *row* lines or content (column-wise characters) per page
#'   (including header materials and context rows). This is only for informative results with `verbose = TRUE`.
#'   It will print `NA` if not specified by the pagination machinery.
#' @param context_lpp_or_cpp (`numeric`)\cr total number of context *row* lines or content (column-wise characters)
#'   per page (including header materials). Uses `NA` if not specified by the pagination machinery and is only
#'   for informative results with `verbose = TRUE`.
#' @param min_siblings (`numeric`)\cr minimum sibling rows which must appear on either side of pagination row for a
#'   mid-subtable split to be valid. Defaults to 2 for tables. It is automatically turned off (set to 0) for listings.
#' @param nosplitin (`character`)\cr list of names of subtables where page breaks are not allowed, regardless of other
#'   considerations. Defaults to none.
#' @param verbose (`flag`)\cr whether additional informative messages about the search for
#'   pagination breaks should be shown. Defaults to `FALSE`.
#' @param row (`flag`)\cr whether pagination is happening in row space (`TRUE`, the default) or column
#'   space (`FALSE`).
#' @param have_col_fnotes (`flag`)\cr whether the table-like object being rendered has column-associated
#'   referential footnotes.
#' @param div_height (`numeric(1)`)\cr the height of the divider line when the associated object is rendered.
#'   Defaults to `1`.
#'
#' @details `pab_indices_inner` implements the core pagination algorithm (see below)
#' for a single direction (vertical if `row = TRUE` (the default), horizontal otherwise)
#' based on the pagination data frame and (already adjusted for non-body rows/columns)
#' lines (or characters) per page.
#'
#' @inheritSection pagination_algo Pagination Algorithm
#'
#' @return A `list` containing a vector of row numbers, broken up by page.
#'
#' @examples
#' mypgdf <- basic_pagdf(row.names(mtcars))
#'
#' paginds <- pag_indices_inner(mypgdf, rlpp = 15, min_siblings = 0)
#' lapply(paginds, function(x) mtcars[x, ])
#'
#' @export
pag_indices_inner <- function(pagdf,
                              rlpp,
                              lpp_or_cpp = NA_integer_, context_lpp_or_cpp = NA_integer_, # Context number of lines
                              min_siblings,
                              nosplitin = character(),
                              verbose = FALSE,
                              row = TRUE,
                              have_col_fnotes = FALSE,
                              div_height = 1L) {
  start <- 1
  current_page <- 1
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
    end <- find_pag(
      pagdf = pagdf,
      current_page = current_page, start = start, guess = guess,
      rlpp = adjrlpp, lpp_or_cpp = lpp_or_cpp, context_lpp_or_cpp = context_lpp_or_cpp,
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
    current_page <- current_page + 1
  }
  ret
}

#' Find column indices for vertical pagination
#'
#' @inheritParams pag_indices_inner
#' @param obj (`ANY`)\cr object to be paginated. Must have a [matrix_form()] method.
#' @param cpp (`numeric(1)`)\cr number of characters per page (width).
#' @param colwidths (`numeric`)\cr vector of column widths (in characters) for use in vertical pagination.
#' @param rep_cols (`numeric(1)`)\cr number of *columns* (not including row labels) to be repeated on every page.
#'   Defaults to 0.
#'
#' @return A `list` partitioning the vector of column indices into subsets for 1 or more horizontally paginated pages.
#'
#' @examples
#' mf <- basic_matrix_form(df = mtcars)
#' colpaginds <- vert_pag_indices(mf)
#' lapply(colpaginds, function(j) mtcars[, j, drop = FALSE])
#'
#' @export
vert_pag_indices <- function(obj, cpp = 40, colwidths = NULL, verbose = FALSE, rep_cols = 0L) {
  mf <- matrix_form(obj, TRUE)
  clwds <- colwidths %||% propose_column_widths(mf)
  if (is.null(mf_cinfo(mf))) { ## like always, ugh.
    mf <- mpf_infer_cinfo(mf, colwidths = clwds, rep_cols = rep_cols)
  }

  has_rlabs <- mf_has_rlabels(mf)
  rlabs_flag <- as.integer(has_rlabs)
  rlab_extent <- if (has_rlabs) clwds[1] else 0L

  # rep_extent <- pdf$par_extent[nrow(pdf)]
  rcpp <- cpp - table_inset(mf) - rlab_extent # rep_extent - table_inset(mf) - rlab_extent
  if (verbose) {
    message(
      "Adjusted characters per page: ", rcpp,
      " [original: ", cpp,
      ", table inset: ", table_inset(mf), if (has_rlabs) paste0(", row labels: ", clwds[1]),
      "]"
    )
  }
  res <- pag_indices_inner(mf_cinfo(mf),
    rlpp = rcpp, lpp_or_cpp = cpp, context_lpp_or_cpp = cpp - rcpp,
    # cpp - sum(clwds[seq_len(rep_cols)]),
    verbose = verbose,
    min_siblings = 1,
    row = FALSE
  )
  res
}

mpf_infer_cinfo <- function(mf, colwidths = NULL, rep_cols = num_rep_cols(mf)) {
  if (!is(rep_cols, "numeric") || is.na(rep_cols) || rep_cols < 0) {
    stop("got invalid number of columns to be repeated: ", rep_cols)
  }
  clwds <- (colwidths %||% mf_col_widths(mf)) %||% propose_column_widths(mf)
  has_rlabs <- mf_has_rlabels(mf)
  rlabs_flag <- as.integer(has_rlabs)
  rlab_extent <- if (has_rlabs) clwds[1] else 0L
  sqstart <- rlabs_flag + 1L # rep_cols + 1L

  pdfrows <- lapply(
    (sqstart):ncol(mf$strings),
    function(i) {
      rownum <- i - rlabs_flag
      rep_inds <- seq_len(rep_cols)[seq_len(rep_cols) < rownum]
      rep_extent_i <- sum(0L, clwds[rlabs_flag + rep_inds]) + mf$col_gap * length(rep_inds)
      pagdfrow(
        row = NA,
        nm = rownum,
        lab = rownum,
        rnum = rownum,
        pth = NA,
        extent = clwds[i] + mf$col_gap,
        repext = rep_extent_i, # sum(clwds[rep_cols]) + mf$col_gap * max(0, (length(rep_cols) - 1)),
        repind = rep_inds, # rep_cols,
        rclass = "stuff",
        sibpos = 1 - 1,
        nsibs = 1 - 1
      )
    }
  )
  pdf <- do.call(rbind, pdfrows)

  refdf <- mf_fnote_df(mf)
  pdf <- splice_fnote_info_in(pdf, refdf, row = FALSE)
  mf_cinfo(mf) <- pdf
  mf
}

#' Basic/spoof pagination info data frame
#'
#' Returns a minimal pagination info `data.frame` (with no info on siblings, footnotes, etc.).
#'
#' @inheritParams test_matrix_form
#' @param rnames (`character`)\cr vector of row names.
#' @param labs (`character`)\cr vector of row labels. Defaults to `rnames`.
#' @param rnums (`integer`)\cr vector of row numbers. Defaults to `seq_along(rnames)`.
#' @param extents (`integer`)\cr number of lines each row requires to print. Defaults to 1 for all rows.
#' @param rclass (`character`)\cr class(es) for the rows. Defaults to `"DataRow"`.
#' @param paths (`list`)\cr list of paths to the rows. Defaults to `lapply(rnames, function(x) c(parent_path, x))`.
#'
#' @return A `data.frame` suitable for use in both the `MatrixPrintForm` constructor and the pagination machinery.
#'
#' @examples
#' basic_pagdf(c("hi", "there"))
#'
#' @export
basic_pagdf <- function(rnames,
                        labs = rnames,
                        rnums = seq_along(rnames),
                        extents = 1L,
                        rclass = "DataRow",
                        parent_path = NULL,
                        paths = lapply(rnames, function(x) c(parent_path, x))) {
  rws <- mapply(pagdfrow,
    nm = rnames, lab = labs, extent = extents,
    rclass = rclass, rnum = rnums, pth = paths,
    SIMPLIFY = FALSE, nsibs = 1, sibpos = 1
  )
  res <- do.call(rbind.data.frame, rws)
  res$n_siblings <- nrow(res)
  res$pos_in_siblings <- seq_along(res$n_siblings)

  if (!all(rclass == "DataRow")) {
    # These things are used in the simple case of a split, hence having labels.
    # To improve and extend to other cases
    res$pos_in_siblings <- NA
    res$pos_in_siblings[rclass == "DataRow"] <- 1
    res$par_extent[rclass == "DataRow"] <- 1 # the rest is 0
    res$n_siblings <- res$pos_in_siblings
    res$reprint_inds[which(rclass == "DataRow")] <- res$abs_rownumber[which(rclass == "DataRow") - 1]
  }
  res
}

## write paginate() which operates **solely** on a MatrixPrintForm obj

page_size_spec <- function(lpp, cpp, max_width) {
  structure(list(
    lpp = lpp,
    cpp = cpp,
    max_width = max_width
  ), class = "page_size_spec")
}

non_null_na <- function(x) !is.null(x) && is.na(x)

calc_lcpp <- function(page_type = NULL,
                      landscape = FALSE,
                      pg_width = page_dim(page_type)[if (landscape) 2 else 1],
                      pg_height = page_dim(page_type)[if (landscape) 1 else 2],
                      font_family = "Courier",
                      font_size = 8, # grid parameters
                      cpp = NA_integer_,
                      lpp = NA_integer_,
                      tf_wrap = TRUE,
                      max_width = NULL,
                      lineheight = 1,
                      margins = c(bottom = .5, left = .75, top = .5, right = .75),
                      colwidths,
                      col_gap,
                      inset) {
  pg_lcpp <- page_lcpp(
    page_type = page_type,
    landscape = landscape,
    font_family = font_family,
    font_size = font_size,
    lineheight = lineheight,
    margins = margins,
    pg_width = pg_width,
    pg_height = pg_height
  )

  if (non_null_na(lpp)) {
    lpp <- pg_lcpp$lpp
  }
  if (non_null_na(cpp)) {
    cpp <- pg_lcpp$cpp
  }
  stopifnot(!is.na(cpp))

  max_width <- .handle_max_width(tf_wrap, max_width, cpp, colwidths, col_gap, inset)

  page_size_spec(lpp = lpp, cpp = cpp, max_width = max_width)
}

calc_rlpp <- function(pg_size_spec, mf, colwidths, tf_wrap, verbose) {
  lpp <- pg_size_spec$lpp
  max_width <- pg_size_spec$max_width

  dh <- divider_height(mf)
  if (any(nzchar(all_titles(mf)))) {
    ## +1 is for blank line between subtitles and divider
    ## dh is for divider line **between subtitles and column labels**
    ## other divider line is accounted for in cinfo_lines
    if (!tf_wrap) {
      tlines <- length(all_titles(mf))
    } else {
      tlines <- sum(nlines(all_titles(mf), colwidths = colwidths, max_width = max_width))
    }
    tlines <- tlines + dh + 1L
  } else {
    tlines <- 0
  }

  ## dh for divider line between column labels and table body
  cinfo_lines <- mf_nlheader(mf) + dh

  if (verbose) {
    message(
      "Determining lines required for header content: ",
      tlines, " title and ", cinfo_lines, " table header lines"
    )
  }

  refdf <- mf_fnote_df(mf)
  cfn_df <- refdf[is.na(refdf$row) & !is.na(refdf$col), ]

  flines <- 0L
  mnfoot <- main_footer(mf)
  havemn <- length(mnfoot) && any(nzchar(mnfoot))
  if (havemn) {
    flines <- nlines(
      mnfoot,
      colwidths = colwidths,
      max_width = max_width - table_inset(mf)
    )
  }
  prfoot <- prov_footer(mf)
  if (length(prfoot) && any(nzchar(prfoot))) {
    flines <- flines + nlines(prov_footer(mf), colwidths = colwidths, max_width = max_width)
    if (havemn) {
      flines <- flines + 1L
    } ## space between main and prov footer.
  }
  ## this time its for the divider between the footers and whatever is above them
  ## (either table body or referential footnotes)
  if (flines > 0) {
    flines <- flines + dh + 1L
  }
  ## this time its for the divider between the referential footnotes and
  ## the table body IFF we have any, otherwise that divider+blanks pace doesn't get drawn
  if (NROW(cfn_df) > 0) {
    cinfo_lines <- cinfo_lines + sum(cfn_df$nlines)
    flines <- flines + dh + 1L
  }

  if (verbose) {
    message(
      "Determining lines required for footer content",
      if (NROW(cfn_df) > 0) " [column fnotes present]",
      ": ", flines, " lines"
    )
  }

  ret <- lpp - flines - tlines - cinfo_lines

  if (verbose) {
    message("Lines per page available for tables rows: ", ret, " (original: ", lpp, ")")
  }
  ret
}

calc_rcpp <- function(pg_size_spec, mf, colwidths) {
  cpp <- pg_size_spec$cpp

  cpp - table_inset(mf) - colwidths[1] - mf_colgap(mf)
}

splice_idx_lists <- function(lsts) {
  list(
    pag_row_indices = do.call(c, lapply(lsts, function(xi) xi$pag_row_indices)),
    pag_col_indices = do.call(c, lapply(lsts, function(yi) yi$pag_col_indices))
  )
}

#' Paginate a table-like object for rendering
#'
#' These functions perform or diagnose bi-directional pagination on an object.
#'
#' `paginate_indices` renders `obj` into a `MatrixPrintForm` (MPF), then uses that representation to
#' calculate the rows and columns of `obj` corresponding to each page of the pagination of `obj`, but
#' simply returns these indices rather than paginating `obj` itself (see Details for an important caveat).
#'
#' `paginate_to_mpfs` renders `obj` into its MPF intermediate representation, then paginates that MPF into
#' component MPFs each corresponding to an individual page and returns those in a `list`.
#'
#' `diagnose_pagination` attempts pagination via `paginate_to_mpfs`, then returns diagnostic information
#' which explains why page breaks were positioned where they were, or alternatively why no valid pagination
#' could be found.
#'
#' @details
#' All three of these functions generally support all classes which have a corresponding [matrix_form()]
#' method which returns a valid `MatrixPrintForm` object (including `MatrixPrintForm` objects themselves).
#'
#' `paginate_indices` is directly called by `paginate_to_mpfs` (and thus `diagnose_pagination`). For most
#' classes, and most tables represented by supported classes, calling `paginate_to_mpfs` is equivalent to a
#' manual `paginate_indices -> subset obj into pages -> matrix_form` workflow.
#'
#' The exception to this equivalence is objects which support "forced pagination", or pagination logic which
#' is built into the object itself rather than being a function of space on a page. Forced pagination
#' generally involves the creation of, e.g., page-specific titles which apply to these forced paginations.
#' `paginate_to_mpfs` and `diagnose_pagination` support forced pagination by automatically calling the
#' [do_forced_paginate()] generic on the object and then paginating each object returned by that generic
#' separately. The assumption here, then, is that page-specific titles and such are handled by the class'
#' [do_forced_paginate()] method.
#'
#' `paginate_indices`, on the other hand, *does not support forced pagination*, because it returns only a
#' set of indices for row and column subsetting for each page, and thus cannot retain any changes, e.g.,
#' to titles, done within [do_forced_paginate()]. `paginate_indices` does call [do_forced_paginate()], but
#' instead of continuing it throws an error in the case that the result is larger than a single "page".
#'
#' @inheritParams vert_pag_indices
#' @inheritParams pag_indices_inner
#' @inheritParams page_lcpp
#' @inheritParams toString
#' @inheritParams propose_column_widths
#' @param lpp (`numeric(1)` or `NULL`)\cr lines per page. If `NA` (the default), this is calculated automatically
#'   based on the specified page size). `NULL` indicates no vertical pagination should occur.
#' @param cpp (`numeric(1)` or `NULL`)\cr width (in characters) per page. If `NA` (the default), this is calculated
#'   automatically based on the specified page size). `NULL` indicates no horizontal pagination should occur.
#' @param pg_size_spec (`page_size_spec`)\cr. a pre-calculated page size specification. Typically this is not set by
#'   end users.
#' @param col_gap (`numeric(1)`)\cr currently ignored.
#' @param page_num (`string`)\cr placeholder string for page numbers. See [default_page_number] for more
#'   information. Defaults to `NULL`.
#'
#' @return
#' * `paginate_indices` returns a `list` with two elements of the same length: `pag_row_indices` and `pag_col_indices`.
#' * `paginate_to_mpfs` returns a `list` of  `MatrixPrintForm` objects representing each individual page after
#'   pagination (including forced pagination if necessary).
#'
#' @examples
#' mpf <- basic_matrix_form(mtcars)
#'
#' paginate_indices(mpf, pg_width = 5, pg_height = 3)
#'
#' paginate_to_mpfs(mpf, pg_width = 5, pg_height = 3)
#'
#' @aliases paginate pagination
#' @export
paginate_indices <- function(obj,
                             page_type = "letter",
                             font_family = "Courier",
                             font_size = 8,
                             lineheight = 1,
                             landscape = FALSE,
                             pg_width = NULL,
                             pg_height = NULL,
                             margins = c(top = .5, bottom = .5, left = .75, right = .75),
                             lpp = NA_integer_,
                             cpp = NA_integer_,
                             min_siblings = 2,
                             nosplitin = character(),
                             colwidths = NULL,
                             tf_wrap = FALSE,
                             max_width = NULL,
                             indent_size = 2,
                             pg_size_spec = NULL,
                             rep_cols = num_rep_cols(obj),
                             col_gap = 3,
                             verbose = FALSE) {
  ## this MUST alsways return a list, inluding list(obj) when
  ## no forced pagination is needed! otherwise stuff breaks for things
  ## based on s3 classes that are lists underneath!!!
  fpags <- do_forced_paginate(obj)

  ## if we have more than one forced "page",
  ## paginate each of them individually and return the result.
  ## forced pagination is ***currently*** only vertical, so
  ## we don't have to worry about divying up colwidths here,
  ## but we will if we ever allow force_paginate to do horiz
  ## pagination.
  if (length(fpags) > 1) {
    stop(
      "forced pagination is required for this object (class: ", class(obj)[1],
      ") this is not supported in paginate_indices. Use paginate_to_mpfs or call ",
      "do_forced_paginate on your object and paginate each returned section separately."
    )
  }

  ## I'm not sure this is worth doing.
  ## ## We can't support forced pagination here, but we can support calls to,
  ## ## e.g., paginate_indices(do_forced_pag(tt))
  ## if(is.list(obj) && !is.object(obj)) {
  ##     res <- lapply(obj, paginate_indices,
  ##                   page_type = page_type,
  ##                   font_family = font_family,
  ##                   font_size = font_size,
  ##                   lineheight = lineheight,
  ##                   landscape = landscape,
  ##                   pg_width = pg_width,
  ##                   pg_height = pg_height,
  ##                   margins = margins,
  ##                   lpp = lpp,
  ##                   cpp = cpp,
  ##                   tf_wrap = tf_wrap,
  ##                   max_width = max_width,
  ##                   colwidths = colwidths,
  ##                   min_siblings = min_siblings,
  ##                   nosplitin = nosplitin,
  ##                   col_gap = col_gap,
  ##                   ## not setting num_rep_cols here cause it wont' get it right
  ##                   verbose = verbose)
  ##     return(splice_idx_lists(res))
  ## }
  ## order is annoying here, since we won't actually need the mpf if
  ## we run into forced pagination, but life is short and this should work fine.
  mpf <- matrix_form(obj, TRUE, TRUE, indent_size = indent_size)
  if (is.null(colwidths)) {
    colwidths <- mf_col_widths(mpf) %||% propose_column_widths(mpf)
  } else {
    mf_col_widths(mpf) <- colwidths
  }
  if (NROW(mf_cinfo(mpf)) == 0) {
    mpf <- mpf_infer_cinfo(mpf, colwidths, rep_cols)
  }

  if (is.null(pg_size_spec)) {
    pg_size_spec <- calc_lcpp(
      page_type = page_type,
      font_family = font_family,
      font_size = font_size,
      lineheight = lineheight,
      landscape = landscape,
      pg_width = pg_width,
      pg_height = pg_height,
      margins = margins,
      lpp = lpp,
      cpp = cpp,
      tf_wrap = tf_wrap,
      max_width = max_width,
      colwidths = colwidths,
      inset = table_inset(mpf),
      col_gap = col_gap
    )
  }

  ## we can't support forced pagination in paginate_indices because
  ## forced pagination is generally going to set page titles, which
  ## we can't preserve when just returning lists of indices.
  ## Instead we make a hard assumption here that any forced pagination
  ## has already occurred.

  ## this wraps the cell contents AND shoves referential footnote
  ## info mf_rinfo(mpf)
  mpf <- do_cell_fnotes_wrap(mpf, colwidths, max_width, tf_wrap = tf_wrap)

  # rlistings note: if there is a wrapping in a keycol, it is not calculated correctly
  #                 in the above call, so we need to keep this information in mf_rinfo
  #                 and use it here.
  mfri <- mf_rinfo(mpf)
  keycols <- .get_keycols_from_listing(obj)
  if (NROW(mfri) > 1 && .is_listing_mf(mpf) && length(keycols) > 0) {
    # Lets determine the groupings created by keycols
    keycols_grouping_df <- NULL
    for (i in seq_along(keycols)) {
      kcol <- keycols[i]
      if (is(obj, "MatrixPrintForm")) {
        # This makes the function work also in the case we have only matrix form (mainly for testing purposes)
        kcolvec <- mf_strings(obj)[, mf_strings(obj)[1, , drop = TRUE] == kcol][-1]
        while (any(kcolvec == "")) {
          kcolvec[which(kcolvec == "")] <- kcolvec[which(kcolvec == "") - 1]
        }
      } else {
        kcolvec <- obj[[kcol]]
        kcolvec <- vapply(kcolvec, format_value, "", format = obj_format(kcolvec), na_str = obj_na_str(kcolvec))
      }
      groupings <- as.numeric(factor(kcolvec, levels = unique(kcolvec)))
      where_they_start <- which(c(1, diff(groupings)) > 0)
      keycols_grouping_df <- cbind(
        keycols_grouping_df,
        where_they_start[groupings]
      ) # take the groupings
    }

    # Creating the real self_extend for mf_rinfo (if the line is chosen for pagination start)
    self_extent_df <- apply(keycols_grouping_df, 2, function(x) mfri$self_extent[x])
    mf_rinfo(mpf) <- cbind(mfri, "self_extent_page_break" = apply(self_extent_df, 1, max))
  }

  if (is.null(pg_size_spec$lpp)) {
    pag_row_indices <- list(seq_len(mf_nrow(mpf)))
  } else {
    rlpp <- calc_rlpp(
      pg_size_spec, mpf,
      colwidths = colwidths,
      tf_wrap = tf_wrap, verbose = verbose
    )
    pag_row_indices <- pag_indices_inner(
      pagdf = mf_rinfo(mpf),
      rlpp = rlpp,
      lpp_or_cpp = pg_size_spec$lpp,
      context_lpp_or_cpp = pg_size_spec$lpp - rlpp,
      verbose = verbose,
      min_siblings = min_siblings,
      nosplitin = nosplitin
    )
  }

  if (is.null(pg_size_spec$cpp)) {
    pag_col_indices <- list(seq_len(mf_ncol(mpf)))
  } else {
    pag_col_indices <- vert_pag_indices(
      mpf,
      cpp = pg_size_spec$cpp, colwidths = colwidths,
      rep_cols = rep_cols, verbose = verbose
    )
  }

  list(pag_row_indices = pag_row_indices, pag_col_indices = pag_col_indices)
}

setGeneric("has_page_title", function(obj) standardGeneric("has_page_title"))

setMethod("has_page_title", "ANY", function(obj) length(page_titles(obj)) > 0)

#' @rdname paginate_indices
#' @export
paginate_to_mpfs <- function(obj,
                             page_type = "letter",
                             font_family = "Courier",
                             font_size = 8,
                             lineheight = 1,
                             landscape = FALSE,
                             pg_width = NULL,
                             pg_height = NULL,
                             margins = c(top = .5, bottom = .5, left = .75, right = .75),
                             lpp = NA_integer_,
                             cpp = NA_integer_,
                             min_siblings = 2,
                             nosplitin = character(),
                             colwidths = NULL,
                             tf_wrap = FALSE,
                             max_width = NULL,
                             indent_size = 2,
                             pg_size_spec = NULL,
                             page_num = default_page_number(),
                             rep_cols = NULL,
                             col_gap = 2,
                             verbose = FALSE) {
  if (isTRUE(page_num)) {
    page_num <- "page {i}/{n}"
  }
  checkmate::assert_string(page_num, null.ok = TRUE, min.chars = 1)

  # We can return a list of paginated tables and listings
  if (.is_list_of_tables_or_listings(obj)) {
    cur_call <- match.call(expand.dots = FALSE)
    mpfs <- unlist(
      lapply(obj, function(obj_i) {
        cur_call[["obj"]] <- obj_i
        eval(cur_call, envir = parent.frame(3L))
      }),
      recursive = FALSE
    )

    if (!is.null(page_num)) {
      extracted_cpp <- max(
        sapply(mpfs, function(mpf) {
          pf <- prov_footer(mpf)
          nchar(pf[length(pf)])
        })
      )
      mpfs <- .modify_footer_for_page_nums(mpfs, page_num, extracted_cpp)
    }

    return(mpfs)
  }

  if (!is.null(page_num) && (length(prov_footer(obj)) == 0 || !grepl(page_num, prov_footer(obj), perl = TRUE))) {
    # Only adding a line for pagination -> lpp - 1 would have worked too
    prov_footer(obj) <- c(prov_footer(obj), page_num)
  }

  mpf <- matrix_form(obj, TRUE, TRUE, indent_size = indent_size)

  # Turning off min_siblings for listings
  if (.is_listing_mf(mpf)) {
    min_siblings <- 0
  }

  # Checking colwidths
  if (is.null(colwidths)) {
    colwidths <- mf_col_widths(mpf) %||% propose_column_widths(mpf)
  } else {
    cur_ncol <- ncol(mpf)
    if (!.is_listing_mf(mpf)) {
      cur_ncol <- cur_ncol + as.numeric(mf_has_rlabels(mpf))
    }
    if (length(colwidths) != cur_ncol) {
      stop(
        "non-null colwidths argument must have length ncol(x) (+ 1 if row labels are present and if it is a table) [",
        cur_ncol, "], got length ", length(colwidths)
      )
    }
    mf_col_widths(mpf) <- colwidths
  }

  # For listings, keycols are mandatory rep_num_cols
  if (is.null(rep_cols)) {
    rep_cols <- num_rep_cols(obj)
  }

  if (NROW(mf_cinfo(mpf)) == 0) {
    mpf <- mpf_infer_cinfo(mpf, colwidths, rep_cols)
  }

  if (is.null(pg_size_spec)) {
    pg_size_spec <- calc_lcpp(
      page_type = page_type,
      font_family = font_family,
      font_size = font_size,
      lineheight = lineheight,
      landscape = landscape,
      pg_width = pg_width,
      pg_height = pg_height,
      margins = margins,
      lpp = lpp,
      cpp = cpp,
      tf_wrap = tf_wrap,
      max_width = max_width,
      colwidths = colwidths,
      inset = table_inset(mpf),
      col_gap = col_gap
    )
  }
  ## this MUST alsways return a list, inluding list(obj) when
  ## no forced pagination is needed! otherwise stuff breaks for things
  ## based on s3 classes that are lists underneath!!!
  fpags <- do_forced_paginate(obj)

  ## if we have more than one forced "page",
  ## paginate each of them individually and return the result.
  ## forced pagination is ***currently*** only vertical, so
  ## we don't have to worry about divying up colwidths here,
  ## but we will if we ever allow force_paginate to do horiz
  ## pagination.
  if (length(fpags) > 1) {
    deep_pag <- lapply(
      fpags, paginate_to_mpfs,
      pg_size_spec = pg_size_spec,
      colwidths = colwidths,
      min_siblings = min_siblings,
      nosplitin = nosplitin,
      verbose = verbose,
      page_num = page_num
    )
    return(unlist(deep_pag, recursive = FALSE))
  } else if (has_page_title(fpags[[1]])) {
    obj <- fpags[[1]]
  }

  ## we run into forced pagination, but life is short and this should work fine.
  mpf <- matrix_form(obj, TRUE, TRUE, indent_size = indent_size)
  if (is.null(colwidths)) {
    colwidths <- mf_col_widths(mpf) %||% propose_column_widths(mpf)
  }
  mf_col_widths(mpf) <- colwidths

  page_indices <- paginate_indices(
    obj = obj,
    ## page_type = page_type,
    ## font_family = font_family,
    ## font_size = font_size,
    ## lineheight = lineheight,
    ## landscape = landscape,
    ## pg_width = pg_width,
    ## pg_height = pg_height,
    ## margins = margins,
    pg_size_spec = pg_size_spec,
    ## lpp = lpp,
    ## cpp = cpp,
    min_siblings = min_siblings,
    nosplitin = nosplitin,
    colwidths = colwidths,
    tf_wrap = tf_wrap,
    ## max_width = max_width,
    rep_cols = rep_cols,
    verbose = verbose
  )

  pagmats <- lapply(page_indices$pag_row_indices, function(ii) {
    mpf_subset_rows(mpf, ii, keycols = .get_keycols_from_listing(obj))
  })
  ## these chunks now carry around their (correctly subset) col widths...
  res <- lapply(pagmats, function(matii) {
    lapply(page_indices$pag_col_indices, function(jj) {
      mpf_subset_cols(matii, jj, keycols = .get_keycols_from_listing(obj))
    })
  })

  res <- unlist(res, recursive = FALSE)

  # Adding page numbers if needed
  if (!is.null(page_num)) {
    res <- .modify_footer_for_page_nums(
      mf_list = res,
      page_num_format = page_num,
      current_cpp = pg_size_spec$cpp
    )
  }

  res
}

.modify_footer_for_page_nums <- function(mf_list, page_num_format, current_cpp) {
  total_pages <- length(mf_list)
  page_str <- gsub("\\{n\\}", total_pages, page_num_format)
  page_nums <- vapply(
    seq_len(total_pages),
    function(x) {
      gsub("\\{i\\}", x, page_str)
    },
    FUN.VALUE = character(1)
  )
  page_footer <- sprintf(paste0("%", current_cpp, "s"), page_nums)
  if (any(nchar(page_footer) > current_cpp)) {
    stop("Page numbering string (page_num) is too wide to fit the desired page size width (cpp).")
  }

  lapply(seq_along(mf_list), function(pg_i) {
    prov_footer(mf_list[[pg_i]]) <- c(head(prov_footer(mf_list[[pg_i]]), -1), page_footer[pg_i])
    mf_list[[pg_i]]
  })
}

# This works only with matrix_form objects
.is_listing_mf <- function(mf) {
  all(mf_rinfo(mf)$node_class == "listing_df")
}

# Extended copy of get_keycols
.get_keycols_from_listing <- function(obj) {
  if (is(obj, "listing_df")) {
    names(which(sapply(obj, is, class2 = "listing_keycol")))
  } else if (is(obj, "MatrixPrintForm") && .is_listing_mf(obj)) {
    obj$listing_keycols
  } else {
    NULL # table case
  }
}

#' @importFrom utils capture.output
#' @details
#' `diagnose_pagination` attempts pagination and then, regardless of success or failure, returns diagnostic
#' information about pagination attempts (if any) after each row and column.
#'
#' The diagnostics data reflects the final time the pagination algorithm evaluated a page break at the
#' specified location, regardless of how many times the position was assessed in total.
#'
#' To get information about intermediate attempts, perform pagination with `verbose = TRUE` and inspect
#' the messages in order.
#'
#' @importFrom utils capture.output
#'
#' @return
#' * `diagnose_pagination` returns a `list` containing:
#'
#'   \describe{
#'     \item{`lpp_diagnostics`}{Diagnostic information regarding lines per page.}
#'     \item{`row_diagnostics`}{Basic information about rows, whether pagination was attempted
#'       after each row, and the final result of such an attempt, if made.}
#'     \item{`cpp_diagnostics`}{Diagnostic information regarding columns per page.}
#'     \item{`col_diagnostics`}{Very basic information about leaf columns, whether pagination
#'       was attempted after each leaf column, ad the final result of such attempts, if made.}
#'   }
#'
#' @note
#' For `diagnose_pagination`, the column labels are not displayed in the `col_diagnostics` element
#' due to certain internal implementation details; rather the diagnostics are reported in terms of
#' absolute (leaf) column position. This is a known limitation, and may eventually be changed, but the
#' information remains useful as it is currently reported.
#'
#' `diagnose_pagination` is intended for interactive debugging use and *should not be programmed against*,
#' as the exact content and form of the verbose messages it captures and returns is subject to change.
#'
#' Because `diagnose_pagination` relies on `capture.output(type = "message")`, it cannot be used within the
#' `testthat` (and likely other) testing frameworks, and likely cannot be used within `knitr`/`rmarkdown`
#' contexts either, as this clashes with those systems' capture of messages.
#'
#' @examples
#' diagnose_pagination(mpf, pg_width = 5, pg_height = 3)
#' clws <- propose_column_widths(mpf)
#' clws[1] <- floor(clws[1] / 3)
#' dgnost <- diagnose_pagination(mpf, pg_width = 5, pg_height = 3, colwidths = clws)
#' try(diagnose_pagination(mpf, pg_width = 1)) # fails
#'
#' @rdname paginate_indices
#' @export
diagnose_pagination <- function(obj,
                                page_type = "letter",
                                font_family = "Courier",
                                font_size = 8,
                                lineheight = 1,
                                landscape = FALSE,
                                pg_width = NULL,
                                pg_height = NULL,
                                margins = c(top = .5, bottom = .5, left = .75, right = .75),
                                lpp = NA_integer_,
                                cpp = NA_integer_,
                                min_siblings = 2,
                                nosplitin = character(),
                                colwidths = propose_column_widths(matrix_form(obj, TRUE)),
                                tf_wrap = FALSE,
                                max_width = NULL,
                                indent_size = 2,
                                pg_size_spec = NULL,
                                rep_cols = num_rep_cols(obj),
                                col_gap = 2,
                                verbose = FALSE,
                                ...) {
  fpag <- do_forced_paginate(obj)
  if (length(fpag) > 1) {
    return(lapply(
      fpag,
      diagnose_pagination,
      page_type = page_type,
      font_family = font_family,
      font_size = font_size,
      lineheight = lineheight,
      landscape = landscape,
      pg_width = pg_width,
      pg_height = pg_height,
      margins = margins,
      lpp = lpp,
      cpp = cpp,
      tf_wrap = tf_wrap,
      max_width = max_width,
      colwidths = colwidths,
      col_gap = col_gap,
      min_siblings = min_siblings,
      nosplitin = nosplitin
    ))
  }

  mpf <- matrix_form(obj, TRUE)
  msgres <- capture.output(
    {
      tmp <- try(
        paginate_to_mpfs(
          obj,
          page_type = page_type,
          font_family = font_family,
          font_size = font_size,
          lineheight = lineheight,
          landscape = landscape,
          pg_width = pg_width,
          pg_height = pg_height,
          margins = margins,
          lpp = lpp,
          cpp = cpp,
          tf_wrap = tf_wrap,
          max_width = max_width,
          colwidths = colwidths,
          col_gap = col_gap,
          min_siblings = min_siblings,
          nosplitin = nosplitin,
          verbose = TRUE
        )
      )
    },
    type = "message"
  )
  if (is(tmp, "try-error") && grepl("Width of row labels equal to or larger", tmp)) {
    cond <- attr(tmp, "condition")
    stop(conditionMessage(cond), call. = conditionCall(cond))
  }

  lpp_diagnostic <- grep("^(Determining lines|Lines per page available).*$", msgres, value = TRUE)
  cpp_diagnostic <- unique(grep("^Adjusted characters per page.*$", msgres, value = TRUE))

  mpf <- do_cell_fnotes_wrap(mpf, widths = colwidths, max_width = max_width, tf_wrap = tf_wrap)
  mpf <- mpf_infer_cinfo(mpf, colwidths = colwidths)

  rownls <- grep("Checking pagination after row", msgres, fixed = TRUE)
  rownum <- as.integer(gsub("[^[:digit:]]*(.*)$", "\\1", msgres[rownls]))
  rowmsgs <- vapply(unique(rownum), function(ii) {
    idx <- max(which(rownum == ii))
    gsub("\\t[.]*", "", msgres[rownls[idx] + 1])
  }, "")

  msgdf <- data.frame(
    abs_rownumber = unique(rownum),
    final_pag_result = rowmsgs, stringsAsFactors = FALSE
  )
  rdf <- mf_rinfo(mpf)[, c("abs_rownumber", "label", "self_extent", "par_extent", "node_class")]
  rdf$pag_attempted <- rdf$abs_rownumber %in% rownum
  row_diagnose <- merge(rdf, msgdf, by = "abs_rownumber", all.x = TRUE)

  colnls <- grep("Checking pagination after column", msgres, fixed = TRUE)
  colnum <- as.integer(gsub("[^[:digit:]]*(.*)$", "\\1", msgres[colnls]))
  colmsgs <- vapply(unique(colnum), function(ii) {
    idx <- max(which(colnum == ii))
    gsub("\\t[.]*", "", msgres[colnls[idx] + 1])
  }, "")

  colmsgdf <- data.frame(
    abs_rownumber = unique(colnum),
    final_pag_result = colmsgs,
    stringsAsFactors = FALSE
  )
  cdf <- mf_cinfo(mpf)[, c("abs_rownumber", "self_extent")]
  cdf$pag_attempted <- cdf$abs_rownumber %in% colnum
  col_diagnose <- merge(cdf, colmsgdf, by = "abs_rownumber", all.x = TRUE)
  names(col_diagnose) <- gsub("^abs_rownumber$", "abs_colnumber", names(col_diagnose))
  list(
    lpp_diagnostics = lpp_diagnostic,
    row_diagnostics = row_diagnose,
    cpp_diagnostics = cpp_diagnostic,
    col_diagnostics = col_diagnose
  )
}
