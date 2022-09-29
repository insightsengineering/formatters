# toString ----

#' @importFrom stats na.omit
#' @importFrom utils head tail localeToCharset

d_hsep_factory <- function() {
  warn_sent <- FALSE
  function() {
    if (any(grepl("^UTF", localeToCharset()))) {
      "\u2014"
    } else {
      if (!warn_sent && interactive()) {
        message(
          "Detected non-UTF charset. Falling back to '-' ",
          "as default header/body separator. This warning ",
          "will only be shown once per R session."
        )
        warn_sent <<- TRUE
      }
      "-"
    }
  }
}

#' Default horizontal Separator
#'
#' The default horizontal separator character which can be
#' displayed in the current charset for use in rendering table-likes.
#'
#' @return unicode 2014 (long dash for generating solid horizontal line)
#' if in a locale that uses a UTF character set, otherwise an ASCII hyphen
#' with a once-per-session warning.
#' @export
#'
#' @examples
#' default_hsep()
#'
default_hsep <- d_hsep_factory()

#' @rdname tostring
#'
#' @inheritParams MatrixPrintForm
#' @param widths (proposed) widths for the columns of \code{x}
#' @param hsep character(1). Characters to repeat to create header/body
#' separator line.
#'
#' @exportMethod toString
#' @return A character string containing the ASCII rendering
#' of the table-like object reprseented by `x`
#'
#' @examples
#' mform <- basic_matrix_form(mtcars)
#' cat(toString(mform))
#'
# nolint start: cyclocomp_linter.
setMethod("toString", "MatrixPrintForm", function(x,
                                                  widths = NULL,
                                                  col_gap = x$col_gap,
                                                  hsep = default_hsep()) {
  mat <- x
  # we create a matrix with the formatted cell contents
  # mat <- matrix_form(x, indent_rownames = TRUE) # nolint

  if (is.null(widths)) {
    widths <- propose_column_widths(x)
  }
  stopifnot(length(widths) == ncol(mat$strings))

  # format the to ASCII
  body <- mat$strings
  aligns <- mat$aligns
  keep_mat <- mat$display
  spans <- mat$spans
  # ri <- mat$row_info # nolint
  ref_fnotes <- mat$ref_footnotes

  nr <- nrow(body)
  nl_header <- attr(mat, "nlines_header")

  cell_widths_mat <- matrix(rep(widths, nr), nrow = nr, byrow = TRUE)
  nc <- ncol(cell_widths_mat)

  for (i in seq_len(nrow(body))) {
    if (any(!keep_mat[i, ])) { # any spans?
      j <- 1
      while (j <= nc) {
        nj <- spans[i, j]
        j <- if (nj > 1) {
          js <- seq(j, j + nj - 1)
          cell_widths_mat[i, js] <- sum(cell_widths_mat[i, js]) + col_gap * (nj - 1)
          j + nj
        } else {
          j + 1
        }
      }
    }
  }

  content <- matrix(mapply(padstr, body, cell_widths_mat, aligns), ncol = ncol(body))
  content[!keep_mat] <- NA
  # apply(content, 1, function(x) sum(nchar(x), na.rm = TRUE)) # nolint

  gap_str <- strrep(" ", col_gap)

  ncchar <- sum(widths) + (length(widths) - 1) * col_gap
  div <- substr(strrep(hsep, ncchar), 1, ncchar)
  txt_head <- apply(head(content, nl_header), 1, .paste_no_na, collapse = gap_str)
  sec_seps_df <- x$row_info[, c("abs_rownumber", "trailing_sep"), drop = FALSE]
  if (!is.null(sec_seps_df) && any(!is.na(sec_seps_df$trailing_sep))) {
    bdy_cont <- tail(content, -nl_header)
    # unfortunately we count "header rows" wrt linegrouping so it
    # doesn't match the real (ie. body) rows as is
    row_grouping <- tail(x$line_grouping, -nl_header) - attr(x, "nrow_header")
    nrbody <- NROW(bdy_cont)
    stopifnot(length(row_grouping) == nrbody)
    sec_seps_df <- sec_seps_df[!is.na(sec_seps_df$trailing_sep), ]
    txt_body <- character()
    sec_strt <- 1
    section_rws <- sec_seps_df$abs_rownumber
    for (i in seq_len(NROW(section_rws))) {
      cur_rownum <- section_rws[i]
      sec_end <- max(which(row_grouping == cur_rownum))
      txt_body <- c(
        txt_body,
        apply(bdy_cont[seq(sec_strt, sec_end), , drop = FALSE],
          1,
          .paste_no_na,
          collapse = gap_str
        ),
        # don't print section dividers if they would be the last thing before the footer divider
        if (sec_end < nrbody) substr(strrep(sec_seps_df$trailing_sep[i], ncchar), 1, ncchar)
      )
      sec_strt <- sec_end + 1
    }
  } else {
    txt_body <- apply(tail(content, -nl_header), 1, .paste_no_na, collapse = gap_str)
  }
  allts <- all_titles(x)
  titles_txt <- if (any(nzchar(allts))) c(allts, "", div) else NULL
  # TODO make titles affect width...

  allfoots <- all_footers(x)

  footer_txt <- c(
    if (length(ref_fnotes) > 0) c(div, "", ref_fnotes),
    if (any(nzchar(allfoots))) c(div, "", allfoots)
  )
  paste0(paste(c(titles_txt, txt_head, div, txt_body, footer_txt), collapse = "\n"), "\n")
})
# nolint end

# nolint start
# pad_vert_center <- function(x, len) {
#     needed <- len - length(x)
#     if(needed < 0) stop("got vector already longer than target length this shouldn't happen")
#     if(needed > 0) {
#         bf <- ceiling(needed/2)
#         af <- needed - bf
#         x <- c(if(bf > 0) rep("", bf), x, if(af > 0) rep("", af))
#     }
#     x
# }
# nolint end

pad_vert_top <- function(x, len) {
  c(x, rep("", len - length(x)))
}

pad_vert_bottom <- function(x, len) {
  c(rep("", len - length(x)), x)
}

pad_vec_to_len <- function(vec, len, cpadder = pad_vert_top, rlpadder = cpadder) {
  dat <- unlist(lapply(vec[-1], cpadder, len = len))
  dat <- c(rlpadder(vec[[1]], len = len), dat)
  matrix(dat, nrow = len)
}

rep_vec_to_len <- function(vec, len, ...) {
  matrix(unlist(lapply(vec, rep, times = len)),
    nrow = len
  )
}

safe_strsplit <- function(x, split, ...) {
  ret <- strsplit(x, split, ...)
  lapply(ret, function(reti) if (length(reti) == 0) "" else reti)
}

.expand_mat_rows_inner <- function(i, mat, row_nlines, expfun, ...) {
  leni <- row_nlines[i]
  rw <- mat[i, ]
  if (is.character(rw)) {
    rw <- safe_strsplit(rw, "\n", fixed = TRUE)
  }
  expfun(rw, len = leni, ...)
}

expand_mat_rows <- function(mat, row_nlines = apply(mat, 1, nlines), expfun = pad_vec_to_len, ...) {
  rinds <- seq_len(nrow(mat))
  exprows <- lapply(rinds, .expand_mat_rows_inner,
    mat = mat,
    row_nlines = row_nlines,
    expfun = expfun,
    ...
  )
  do.call(rbind, exprows)
}

#' Transform vectors of spans (with dupblication) to Visibility vector
#'
#' @param spans numeric. Vector of spans, with each span value repeated
#' for the cells it covers.
#'
#' @details
#' The values of \code{spans} are assumed to be repeated to such that
#' each individual position covered by the span has the repeated value.
#'
#' This means that each block of values in \code{span} must be of a length
#' at least equal to its value (ie two 2s, three 3s, etc).
#'
#' This function correctly handles cases where two spans of the same size
#' are next to eachother; i.e., a block of four 2s represents two large
#' cells each of which span two individual cells.
#' @export
#'
#' @note
#' Currently no  checking or  enforcement is done  that the  vector of
#' spans is valid in the sense described in the Details section above.
#' @return a logical vector the same length as `spans` indicating
#' whether the contents of a string vector with those spans
#'
#' @examples
#' spans_to_viscell(c(2, 2, 2, 2, 1, 3, 3, 3))
#'
spans_to_viscell <- function(spans) {
  if (!is.vector(spans)) {
    spans <- as.vector(spans)
  }
  myrle <- rle(spans)
  unlist(mapply(function(vl, ln) {
    rep(c(TRUE, rep(FALSE, vl - 1L)),
      times = ln / vl
    )
  },
  SIMPLIFY = FALSE,
  vl = myrle$values,
  ln = myrle$lengths
  ),
  recursive = FALSE
  )
}

#' Propose Column Widths based on an object's `MatrixPrintForm` form
#'
#' The row names are also considered a column for the output
#'
#' @param x MatrixPrintForm object, or an object with a `matrix_form`
#' method.
#' @param indent_size numeric(1). Indent size in characters. Ingored
#' when `x` is already a MatrixPrintForm object in favor of information
#' there.
#'
#' @export
#' @return a vector of column widths based on the content of \code{x}
#' for use in printing and pagination.
#'
#' @examples
#' mf <- basic_matrix_form(mtcars)
#' propose_column_widths(mf)
#'
#' # ' @examples
#' # ' library(dplyr)
#' # ' library(rtables)
#' # ' iris2 <- iris %>%
#' # '   group_by(Species) %>%
#' # '   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
#' # '   ungroup()
#' # '
#' # ' l <- basic_table() %>%
#' # '   split_cols_by("Species") %>%
#' # '   split_cols_by("group") %>%
#' # '   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx")
#' # '
#' # ' tbl <- build_table(l, iris2)
#' # ' mf <- matrix_form(tbl)
#' # ' propose_column_widths(mf)
# nolint start: cyclocomp_linter.
propose_column_widths <- function(x, indent_size = 2) {
  # stopifnot(is(x, "VTableTree")) # nolint
  if (!is(x, "MatrixPrintForm")) {
    x <- matrix_form(x, indent_rownames = TRUE, indent_size = indent_size)
  }
  body <- x$strings
  spans <- x$spans
  # aligns <- x$aligns # nolint
  display <- x$display

  chars <- nchar(body)

  # first check column widths without colspan
  has_spans <- spans != 1
  chars_ns <- chars
  chars_ns[has_spans] <- 0
  widths <- apply(chars_ns, 2, max)

  # now check if the colspans require extra width
  if (any(has_spans)) {
    has_row_spans <- apply(has_spans, 1, any)

    chars_sp <- chars[has_row_spans, , drop = FALSE]
    spans_sp <- spans[has_row_spans, , drop = FALSE]
    disp_sp <- display[has_row_spans, , drop = FALSE]

    nc <- ncol(spans)
    for (i in seq_len(nrow(chars_sp))) {
      for (j in seq_len(nc)) {
        if (disp_sp[i, j] && spans_sp[i, j] != 1) {
          i_cols <- seq(j, j + spans_sp[i, j] - 1)

          nchar_i <- chars_sp[i, j]
          cw_i <- widths[i_cols]
          available_width <- sum(cw_i)

          if (nchar_i > available_width) {
            # need to update widths to fit content with colspans
            # spread width among columns
            widths[i_cols] <- cw_i + spread_integer(nchar_i - available_width, length(cw_i))
          }
        }
      }
    }
  }
  widths
}
# nolint end

#' Pad a string and align within string
#'
#' @param x string
#' @param n number  of  character  of the  output  string,  if `n  <
#'     nchar(x)` an error is thrown
#' @param just  character(1).   Text  alignment   justification  to
#'     use. Defaults to center. Must be center, right or left.
#'
#' @export
#' @return `x`, padded to be a string of `n` characters
#'
#' @examples
#' padstr("abc", 3)
#' padstr("abc", 4)
#' padstr("abc", 5)
#' padstr("abc", 5, "left")
#' padstr("abc", 5, "right")
#'
#' if (interactive()) {
#'   padstr("abc", 1)
#' }
#'
padstr <- function(x, n, just = c("center", "left", "right")) {
  just <- match.arg(just)

  if (length(x) != 1) stop("length of x needs to be 1 and not", length(x))
  if (is.na(n) || !is.numeric(n) || n < 0) stop("n needs to be numeric and > 0")

  if (is.na(x)) x <- "<NA>"

  nc <- nchar(x)

  if (n < nc) stop("\"", x, "\" has more than ", n, " characters")

  switch(just,
    center = {
      pad <- (n - nc) / 2
      paste0(spaces(floor(pad)), x, spaces(ceiling(pad)))
    },
    left = paste0(x, spaces(n - nc)),
    right = paste0(spaces(n - nc), x)
  )
}

spaces <- function(n) {
  strrep(" ", n)
}

.paste_no_na <- function(x, ...) {
  paste(na.omit(x), ...)
}

#' spread x into len elements
#'
#' @param x numeric(1). The number to spread
#' @param len numeric(1). The number of times to repeat \code{x}
#'
#' @export
#' @return if \code{x} is a scalar "whole number" value (see \code{\link{is.wholenumber}}),
#' the value \code{x} repeated \code{len} times. If not, an error is thrown.
#'
#' @examples
#' spread_integer(3, 1)
#' spread_integer(0, 3)
#' spread_integer(1, 3)
#' spread_integer(2, 3)
#' spread_integer(3, 3)
#' spread_integer(4, 3)
#' spread_integer(5, 3)
#' spread_integer(6, 3)
#' spread_integer(7, 3)
#'
spread_integer <- function(x, len) {
  stopifnot(
    is.wholenumber(x), length(x) == 1, x >= 0,
    is.wholenumber(len), length(len) == 1, len >= 0,
    !(len == 0 && x > 0)
  )

  if (len == 0) {
    integer(0)
  } else {
    y <- rep(floor(x / len), len)
    i <- 1
    while (sum(y) < x) {
      y[i] <- y[i] + 1
      if (i == len) {
        i <- 1
      } else {
        i <- i + 1
      }
    }
    y
  }
}

#' is.wholenumber
#'
#' @param x numeric(1). A numeric value
#' @param tol numeric(1). A precision tolerance.
#'
#' @return \code{TRUE} if \code{x} is within \code{tol} of zero,
#' \code{FALSE} otherwise.
#'
#' @export
#' @examples
#' is.wholenumber(5)
#' is.wholenumber(5.00000000000000001)
#' is.wholenumber(.5)
#'
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) { # nolint
  abs(x - round(x)) < tol
}
