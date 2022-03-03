# toString ----

#' @importFrom stats na.omit
#' @importFrom utils head tail



#' @rdname tostring
#' @param widths (proposed) widths for the columns of \code{x}
#' @param col_gap numeric(1). Space between columns
#' @param linesep character(1). Characters to repeat to create header/body
#' separator line.
#' @exportMethod toString
setMethod("toString", "MatrixPrintForm", function(x,
                                                  widths = NULL,
                                                  col_gap = 3,
                                                  linesep = "\u2014") {
    mat <- x

  ## we create a matrix with the formatted cell contents
##  mat <- matrix_form(x, indent_rownames = TRUE)


  if (is.null(widths)) {
    widths <- propose_column_widths(x)
  }
  stopifnot(length(widths) == ncol(mat$strings))

  # format the to ASCII
  body <- mat$strings
  aligns <- mat$aligns
  keep_mat <- mat$display
  spans <- mat$spans
#    ri <- mat$row_info
    ref_fnotes <- mat$ref_footnotes


  # xxxx <- 1
  # # get rowgap position
  # insert_gap_before <- which(ri$depth == sort(ri$depth)[seq_len(xxxx)])
  # # which(ri$rowtype == "LabelRow" & ri$depth == min(ri$depth))
  # insert_gap_before <- remove_consecutive_numbers(insert_gap_before)
  # insert_gap_before <- insert_gap_before[insert_gap_before != 1]

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

  # Are the total characters per row the same?
  # A <- cell_widths_mat
  # A[!keep_mat] <- 0
  # apply(A, 1, sum)

  content <- matrix(mapply(padstr, body, cell_widths_mat, aligns), ncol = ncol(body))
  content[!keep_mat] <- NA
  # apply(content, 1, function(x) sum(nchar(x), na.rm = TRUE))

  gap_str <- strrep(" ", col_gap)

  ncchar <-  sum(widths) + (length(widths) - 1) * col_gap
  div <- substr(strrep(linesep, ncchar), 1, ncchar)

  txt_head <- apply(head(content, nl_header), 1, .paste_no_na, collapse = gap_str)
  txt_body <- apply(tail(content, -nl_header), 1, .paste_no_na, collapse = gap_str)

  allts <- all_titles(x)
  titles_txt <- if(any(nzchar(allts))) c(allts, "", div)  else NULL
    ## TODO make titles affect width...

    allfoots <- all_footers(x)


    footer_txt <- c(if(length(ref_fnotes) > 0) c(div, "", ref_fnotes),
                    if(any(nzchar(allfoots))) c(div, "", allfoots))
  paste0(paste(c(titles_txt, txt_head, div, txt_body, footer_txt), collapse = "\n"), "\n")

})

pad_vert_center <- function(x, len) {
    needed <- len - length(x)
    if(needed < 0) stop("got vector already longer than target length this shouldn't happen")
    if(needed > 0) {
        bf <- ceiling(needed/2)
        af <- needed - bf
        x <- c(if(bf > 0) rep("", bf), x, if(af > 0) rep("", af))
    }
    x
}

pad_vert_top <- function(x, len) {
    c(x, rep("", len - length(x)))
}

pad_vert_bottom <- function(x, len) {
    c(rep("", len - length(x)), x)
}

pad_vec_to_len <- function(vec, len, cpadder = pad_vert_top, rlpadder = cpadder) {
    dat <- unlist(lapply(vec[-1], cpadder, len = len))
    dat <- c(rlpadder(vec[[1]], len = len), dat)
    matrix(dat,  nrow = len)
}

rep_vec_to_len <- function(vec, len, ...) {
    matrix(unlist(lapply(vec, rep, times = len)),
           nrow = len)
}


safe_strsplit <- function(x, split, ...) {
    ret <- strsplit(x, split, ...)
    lapply(ret, function(reti) if(length(reti) == 0) "" else reti)
}

.expand_mat_rows_inner <- function(i, mat, row_nlines, expfun, ...) {
    leni <- row_nlines[i]
    rw <- mat[i,]
    if(is.character(rw))
        rw <- safe_strsplit(rw, "\n", fixed = TRUE)
    expfun(rw, len = leni, ...)
}

expand_mat_rows <- function(mat, row_nlines = apply(mat, 1, nlines), expfun = pad_vec_to_len, ...) {

    rinds <- 1:nrow(mat)
    exprows <- lapply(rinds, .expand_mat_rows_inner,
                      mat = mat,
                      row_nlines = row_nlines,
                      expfun = expfun,
                      ...)
    do.call(rbind, exprows)

}


#' Transform vectors of spans (with dupblication) to Visibility vector
#'
#' @param spans numeric. Vector of spans, with each span value repeated
#' for the cells it covers.
#'
#' @details
#'
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
#' @note
#'
#' Currently no  checking or  enforcement is done  that the  vector of
#' spans is valid in the sense described in the Details section above.
#' @examples
#'
#' spans_to_viscell(c(2, 2, 2, 2, 1, 3, 3, 3))
spans_to_viscell <- function(spans) {
    if(!is.vector(spans))
        spans <- as.vector(spans)
    myrle <- rle(spans)
    unlist(mapply(function(vl, ln) rep(c(TRUE, rep(FALSE, vl - 1L)),
                                                                  times = ln/vl),
                                             SIMPLIFY = FALSE,
                                             vl = myrle$values,
                                             ln = myrle$lengths),
                                      recursive = FALSE)
}


#' Propose Column Widths based on an object's `MatrixPrintForm` form
#'
#' The row names are also considered a column for the output
#'
#' @param x MatrixPrintForm object
#'
#' @export
#' @return a vector of column widths based on the content of \code{x}
#' for use in printing and pagination.
## ' @examples
## ' library(dplyr)
## ' library(rtables)
## ' iris2 <- iris %>%
## '   group_by(Species) %>%
## '   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
## '   ungroup()
## '
## ' l <- basic_table() %>%
## '   split_cols_by("Species") %>%
## '   split_cols_by("group") %>%
## '   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx")
## '
## ' tbl <- build_table(l, iris2)
## ' mf <- matrix_form(tbl)
## ' propose_column_widths(mf)
propose_column_widths <- function(x) {

    ##stopifnot(is(x, "VTableTree"))

  body <- x$strings
  spans <- x$spans
  aligns <- x$aligns
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




#' Pad a string and align within string
#'
#' @param x string
#' @param n number  of  character  of the  output  string,  if `n  <
#'     nchar(x)` an error is thrown
#' @param just  character(1).   Text  alignment   justification  to
#'     use. Defaults to center. Must be center, right or left.
#'
#' @export
#' @examples
#'
#' padstr("abc", 3)
#' padstr("abc", 4)
#' padstr("abc", 5)
#' padstr("abc", 5, "left")
#' padstr("abc", 5, "right")
#'
#' if(interactive()){
#' padstr("abc", 1)
#' }
#'
padstr <- function(x, n, just = c("center", "left", "right")) {

  just <- match.arg(just)

  if (length(x) != 1) stop("length of x needs to be 1 and not", length(x))
  if (is.na(n) || !is.numeric(n) || n < 0) stop("n needs to be numeric and > 0")

  if (is.na(x)) x <- "<NA>"

  nc <- nchar(x)

  if (n < nc) stop("\"", x, "\" has more than ", n, " characters")

  switch(
    just,
    center = {
      pad <- (n - nc)/2
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
spread_integer <- function(x, len) {
  stopifnot(
    is.wholenumber(x), length(x) == 1, x >= 0,
    is.wholenumber(len), length(len) == 1, len >= 0,
    !(len == 0 && x > 0)
  )


  if (len == 0) {
    integer(0)
  } else {
    y <- rep(floor(x/len), len)
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
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}
