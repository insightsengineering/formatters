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
#'
#' @export
#' @examples
#' default_hsep()
default_hsep <- d_hsep_factory()

.calc_cell_widths <- function(mat, colwidths, col_gap) {
  spans <- mat$spans
  keep_mat <- mat$display
  body <- mat$strings

  nr <- nrow(body)

  cell_widths_mat <- matrix(rep(colwidths, nr), nrow = nr, byrow = TRUE)
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
  cell_widths_mat
}

#' @rdname tostring
#'
#' @inheritParams MatrixPrintForm
#' @param widths numeric (or  NULL). (proposed) widths for the columns
#'     of \code{x}. The expected length  of this numeric vector can be
#'     retrieved with  `ncol() + 1`  as the  column of row  names must
#'     also be considered.
#' @param  hsep  character(1).   Characters  to  repeat   to  create
#'     header/body separator line.
#' @param tf_wrap logical(1). Should  the texts for  title, subtitle,
#'     and footnotes be wrapped?
#' @param max_width integer(1), character(1) or NULL. Width that title
#'     and   footer   (including   footnotes)  materials   should   be
#'     word-wrapped to. If NULL, it is  set to the current print width
#'     of the  session (`getOption("width")`). If set to `"auto"`,
#'     the width of the table (plus any table inset) is used. Ignored
#'     completely if `tf_wrap` is `FALSE`.
#'
#' @details
#'
#' Manual insertion of newlines is not supported when `tf_wrap` is on
#' and will result in a warning and undefined wrapping behavior. Passing
#' vectors of already split strings remains supported, however in this
#' case each string is word-wrapped separately with the behavior
#' described above.
#'
#' @examples
#' mform <- basic_matrix_form(mtcars)
#' cat(toString(mform))
#'
#' @return A character string containing the ASCII rendering
#' of the table-like object represented by `x`
#'
#' @exportMethod toString
setMethod("toString", "MatrixPrintForm", function(x,
                                                  widths = NULL,
                                                  tf_wrap = FALSE,
                                                  max_width = NULL,
                                                  col_gap = x$col_gap,
                                                  hsep = default_hsep()) {
  mat <- matrix_form(x, TRUE)

  inset <- table_inset(mat)

  ## if(tf_wrap)
  ##     mat <- wrap_title_footer(mat, max_width = max_width, inset = inset)

  if (is.null(widths)) {
    widths <- propose_column_widths(x)
  }

  ncchar <- sum(widths) + (length(widths) - 1) * col_gap
  ## Text wrapping checks
  stopifnot(is.logical(tf_wrap))
  if (tf_wrap) {
    if (is.null(max_width)) {
      max_width <- getOption("width", 80L)
    } else if (is.character(max_width) && identical(max_width, "auto")) {
      max_width <- ncchar + inset
    }
    stopifnot(is.numeric(max_width), max_width > 0)
  }


  stopifnot(length(widths) == ncol(mat$strings))



  ## format the to ASCII
  cell_widths_mat <- .calc_cell_widths(mat, widths, col_gap)
  ## wrap_string calls strwrap, which destroys whitespace so we need to make sure to put the indents back in

  old_indent <- gsub("^([[:space:]]*).*", "\\1", mat$strings[, 1])
  need_reindent <- nzchar(old_indent)
  reindent_old_idx <- mf_lgrouping(mat)[need_reindent]
  new_strings <- matrix(unlist(mapply(wrap_string, str = mat$strings, max_width = cell_widths_mat, hard = TRUE)),
    ncol = ncol(mat$strings)
  )
  mat$strings <- new_strings
  ## XXXXX this is wrong and will break for listings cause we don't know when
  ## we need has_topleft to be FALSE!!!!!!!!!!
  mat <- mform_handle_newlines(mat)
  reindent_new_idx <- match(reindent_old_idx, mf_lgrouping(mat))
  if (anyNA(reindent_new_idx)) {
    stop("Unable to remap indenting after cell content text wrapping. Please contact the maintainer, this should not happen")
  } # nocov
  mat$strings[reindent_new_idx, 1] <- paste0(old_indent[need_reindent], mat$strings[reindent_new_idx, 1])
  body <- mat$strings
  aligns <- mat$aligns
  keep_mat <- mat$display
  spans <- mat$spans
  ##    ri <- mat$row_info
  ref_fnotes <- mat$ref_footnotes
  nl_header <- mf_nlheader(mat)

  cell_widths_mat <- .calc_cell_widths(mat, widths, col_gap)

  ## nr <- nrow(body)
  ## cell_widths_mat <- matrix(rep(widths, nr), nrow = nr, byrow = TRUE)
  ## nc <- ncol(cell_widths_mat)

  ## for (i in seq_len(nrow(body))) {
  ##   if (any(!keep_mat[i, ])) { # any spans?
  ##     j <- 1
  ##     while (j <= nc) {
  ##       nj <- spans[i, j]
  ##       j <- if (nj > 1) {
  ##         js <- seq(j, j + nj - 1)
  ##         cell_widths_mat[i, js] <- sum(cell_widths_mat[i, js]) + col_gap * (nj - 1)
  ##         j + nj
  ##       } else {
  ##         j + 1
  ##       }
  ##     }
  ##   }
  ## }


  content <- matrix(mapply(padstr, body, cell_widths_mat, aligns), ncol = ncol(body))
  content[!keep_mat] <- NA
  # apply(content, 1, function(x) sum(nchar(x), na.rm = TRUE))

  gap_str <- strrep(" ", col_gap)

  div <- substr(strrep(hsep, ncchar), 1, ncchar)
  txt_head <- apply(head(content, nl_header), 1, .paste_no_na, collapse = gap_str)
  sec_seps_df <- x$row_info[, c("abs_rownumber", "trailing_sep"), drop = FALSE]
  if (!is.null(sec_seps_df) && any(!is.na(sec_seps_df$trailing_sep))) {
    bdy_cont <- tail(content, -nl_header)
    ## unfortunately we count "header rows" wrt lihnegrouping so it
    ## doesn't match the real (ie body) rows as is
    row_grouping <- tail(x$line_grouping, -nl_header) - mf_nrheader(x)
    nrbody <- NROW(bdy_cont)
    stopifnot(length(row_grouping) == nrbody)
    ## all rows with non-NA section divs and the final row (regardless of NA status)
    ## fixes #77
    sec_seps_df <- sec_seps_df[unique(c(which(!is.na(sec_seps_df$trailing_sep)),
                                      NROW(sec_seps_df))), ]
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
        ## don't print section dividers if they would be the last thing before the
        ## footer divider
        ## this also ensures an extraneous sec div won't be printed if we have non-sec-div
        ## rows after the last sec div row (#77)
        if (sec_end < nrbody) {
          substr(
            strrep(sec_seps_df$trailing_sep[i], ncchar), 1,
            ncchar - inset
          )
        }
      )
      sec_strt <- sec_end + 1
    }
  } else {
    txt_body <- apply(tail(content, -nl_header), 1, .paste_no_na, collapse = gap_str)
  }


  allts <- all_titles(x)

  allfoots <- list(
    "main_footer" = main_footer(x),
    "prov_footer" = prov_footer(x),
    "ref_footnotes" = ref_fnotes
  )
  allfoots <- allfoots[!sapply(allfoots, is.null)]


  ## Wrapping titles if they go beyond the horizontally allowed space
  if (tf_wrap) {
    new_line_warning(allts)
    allts <- wrap_txt(allts, max_width = max_width)
  }

  titles_txt <- if (any(nzchar(allts))) c(allts, "", .do_inset(div, inset)) else NULL

  # Wrapping footers if they go beyond the horizontally allowed space
  if (tf_wrap) {
    new_line_warning(allfoots)
    allfoots$main_footer <- wrap_txt(allfoots$main_footer, max_width - inset)
    allfoots$ref_footnotes <- wrap_txt(allfoots$ref_footnotes, max_width - inset)
    ## no - inset here because the prov_footer is not inset
    allfoots$prov_footer <- wrap_txt(allfoots$prov_footer, max_width)
  }

  paste0(paste(
    c(
      titles_txt,
      .do_inset(txt_head, inset),
      .do_inset(div, inset),
      .do_inset(txt_body, inset),
      .footer_inset_helper(allfoots, div, inset)
    ),
    collapse = "\n"
  ), "\n")
})

.do_inset <- function(x, inset) {
  if (inset == 0 || !any(nzchar(x))) {
    return(x)
  }
  padding <- strrep(" ", inset)
  if (is.character(x)) {
    x <- paste0(padding, x)
  } else if (is(x, "matrix")) {
    x[, 1] <- .do_inset(x[, 1, drop = TRUE], inset)
  }
  x
}


.inset_div <- function(txt, div, inset) {
  c(.do_inset(div, inset), "", txt)
}

.footer_inset_helper <- function(footers_v, div, inset) {
  div_done <- FALSE # nolint
  fter <- footers_v$main_footer
  prvf <- footers_v$prov_footer
  rfn <- footers_v$ref_footnotes
  footer_txt <- .do_inset(rfn, inset)
  if (any(nzchar(footer_txt))) {
    footer_txt <- .inset_div(footer_txt, div, inset)
  }
  if (any(vapply(
    footers_v, function(x) any(nzchar(x)),
    TRUE
  ))) {
    if (any(nzchar(prvf))) {
      provtxt <- c(
        if (any(nzchar(fter))) "",
        prvf
      )
    } else {
      provtxt <- character()
    }
    footer_txt <- c(
      footer_txt,
      .inset_div(
        c(
          .do_inset(fter, inset),
          provtxt
        ),
        div,
        inset
      )
    )
  }
  footer_txt
}

## pad_vert_center <- function(x, len) {
##     needed <- len - length(x)
##     if(needed < 0) stop("got vector already longer than target length this shouldn't happen")
##     if(needed > 0) {
##         bf <- ceiling(needed/2)
##         af <- needed - bf
##         x <- c(if(bf > 0) rep("", bf), x, if(af > 0) rep("", af))
##     }
##     x
## }

new_line_warning <- function(str_v) {
  if (any(unlist(sapply(str_v, grepl, pattern = "\n")))) {
    msg <- c(
      "Detected manual newlines when automatic title/footer word-wrapping is on.",
      "This is unsupported and will result in undefined behavior. Please either ",
      "utilize automatic word-wrapping with newline characters inserted, or ",
      "turn off automatic wrapping and wordwrap all contents manually by inserting ",
      "newlines."
    )
    warning(paste0(msg, collapse = ""))
  }
}

#' Wrap a string to within a maximum width
#' @param str character(1). String to be wrapped
#' @param max_width numeric(1). Maximum width, in characters, that the
#' text should be wrapped at.
#' @param hard logical(1). Should hard wrapping (embedding newlines in
#' the incoming strings) or soft (breaking wrapped strings into vectors
#' of length >1) be used. Defaults to `FALSE` (ie soft wrapping).
#'
#' @details      word      wrapping     happens      as      with
#'     \link[base:strwrap]{base::strwrap}    with     the    following
#'     exception: individual  words which are longer  than `max_width`
#'     are  broken  up  in a  way  that  fits  with  the rest  of  the
#'     wordwrapping.
#'
#' @return a string (`wrap_string` or character vector (`wrap_txt`) containing
#' the hard or soft word-wrapped content.
#' @export
wrap_string <- function(str, max_width, hard = FALSE) {
  stopifnot(is.character(str) && length(str) == 1)
  naive <- strwrap(str, max_width + 1)
  while (any(nchar(naive) > max_width)) {
    good <- character()
    bwi <- which(nchar(naive) > max_width)[1]
    curbw <- naive[bwi]
    if (bwi > 2) {
      good <- c(good, naive[1:(bwi - 2)])
    }
    if (bwi > 1) {
      str_before <- naive[bwi - 1]
    } else {
      str_before <- ""
    }
    room <- max_width - nchar(str_before) - (bwi > 1)
    if (room <= 0) {
      toadd <- c(str_before, substr(curbw, 1, max_width))
      room <- 0
      leftover <- substr(curbw, max_width + 1, nchar(curbw))
    } else {
      goodpart <- substr(curbw, 1, room)
      if (nzchar(str_before)) {
        toadd <- paste(str_before, goodpart)
      } else {
        toadd <- goodpart
      }
      leftover <- substr(curbw, room + 1, nchar(curbw))
    }
    good <- c(good, toadd)
    if (bwi == length(naive)) {
      good <- c(good, leftover)
    } else {
      good <- c(
        good,
        paste(leftover, naive[bwi + 1]),
        if (bwi < length(naive) - 1) naive[seq(bwi + 2, length(naive))]
      )
    }
    str <- paste(good, collapse = " ")
    naive <- strwrap(str, max_width + 1)
  }
  if (hard) {
    naive <- paste(naive, collapse = "\n")
  }
  naive
}

#' @param txt character. Vector of strings that should be (independently)
#' text-wrapped.
#' @rdname wrap_string
#' @export
wrap_txt <- function(txt, max_width, hard = FALSE) {
  unlist(lapply(txt, wrap_string, max_width = max_width, hard = hard), use.names = FALSE)
}

## #' Wrap title and footer materials on a table-like object
## #'
## #' @inheritParams wrap_string
## #' @param obj. Table-like object. The MatrixPrintForm or Table-like object to modify
## #' @param inset numeric(1). Inset amount in characters for the body. Defaults to
## #' `table_inset(obj)` and so generally should not be set manually for objects that
## #' whose class has a defined `table_inset` method.
## #' @return `obj` with the main title, subtitles, referential footnotes, main footer
## #' and prov footers word-wrapped.
## #' @export
## wrap_title_footer <- function(obj, max_width = NULL, inset = table_inset(obj) %||% 0L) {

##     if(is.null(max_width))
##         return(obj)

##     main_title(obj) <- wrap_txt(main_title(obj), max_width = max_width)
##     subtitles(obj) <- wrap_txt(subtitles(obj), max_width = max_width)
##     ref_footnotes(obj) <- wrap_txt(ref_footnotes(obj), max_width = max_width - inset)
##     main_footer(obj) <- wrap_txt(main_footer(obj), max_width = max_width - inset)
##     prov_footer(obj) <- wrap_txt(prov_footer(obj), max_width = max_width)
##     obj
## }

##   if (any(sapply(txt, function(x) nchar(x) > max_width))) {

##     # Checking if any word is larger than max_width
##     spl_txt_l <- unname(sapply(txt, function(x) {
##       if (nchar(x) > 0) {
##         strsplit(x, split = " ")
##       } else {
##         x
##       }
##     }))
##     which_large_txt <- sapply(spl_txt_l, function(x) any(nchar(x) > max_width))

##     if (any(which_large_txt)) {
##       warning(
##         "Found the following txt that could not be wrapped with max_width of ",
##         max_width,
##         ":\n- ",
##         paste0(txt[which_large_txt], collapse = "\n- "),
##         "\nAttempting at a split after ", max_width, " characters."
##       )

##       spl_txt_l[which_large_txt] <- lapply(
##         spl_txt_l[which_large_txt],
##         function(x) {
##           unname(unlist(lapply(x, function(y) {
##             sapply(
##               seq(from = 1, to = nchar(y), by = max_width),
##               function(i) substr(y, i, i + max_width - 1)
##             )
##           })))
##         }
##       )
##       # Paste everything back together
##       txt <- lapply(spl_txt_l, paste, collapse = " ")
##     }
##     # Main wrapper
##     txt_out <- lapply(txt,
##       strwrap,
##       width = max_width + 1
##     )
##     names(txt_out) <- NULL
##     txt_out <- unlist(txt_out)

##     txt_out
##   } else {
##     unlist(txt)
##   }
## }

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
#' @return a logical vector the same length as `spans` indicating
#' whether the contents of a string vector with those spans
spans_to_viscell <- function(spans) {
  if (!is.vector(spans)) {
    spans <- as.vector(spans)
  }
  myrle <- rle(spans)
  unlist(
    mapply(
      function(vl, ln) {
        rep(c(TRUE, rep(FALSE, vl - 1L)), times = ln / vl)
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
#' @examples
#' mf <- basic_matrix_form(mtcars)
#' propose_column_widths(mf)
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
propose_column_widths <- function(x, indent_size = 2) {
  ## stopifnot(is(x, "VTableTree"))
  if (!is(x, "MatrixPrintForm")) {
    x <- matrix_form(x, indent_rownames = TRUE, indent_size = indent_size)
  }
  body <- x$strings
  spans <- x$spans
  # aligns <- x$aligns
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
#' if (interactive()) {
#'   padstr("abc", 1)
#' }
#' @return `x`, padded to be a string of `n` characters
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
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}
