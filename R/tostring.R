## state completely sucks and I hate it, but
## we need a pdf device open to calculate the
## print width of strings, and we can't be opening
## a new one every time we want to
font_dev_state <- new.env()
font_dev_state$open <- FALSE
font_dev_state$fontspec <- list()
font_dev_state$spacewidth <- NA_real_
font_dev_state$ismonospace <- NA
font_dev_state$max_ratio <- NA_real_
font_dev_state$dev_num <- NA_integer_


cwidth_inches_unsafe <- function(x) {
  convertWidth(unit(1, "strwidth", x), "inches", valueOnly = TRUE)
}

## returns whether it opened a new device or not
#' Activate font state
#'
#' @param fontspec (`font_spec`)\cr a font_spec object specifying the font information to use for
#'   calculating string widths and heights, as returned by [font_spec()].
#' @param silent (`logical(1)`)\cr If `FALSE`, the default, a warning will be
#'   emitted if this function switches away from an active graphics device.
#'
#' @details The font device state is an environment with
#' four variables guaranteed to be set:
#'
#' \describe{
#'   \item{`open`}{(`logical(1)`)\cr whether a device is already open with font info}
#'   \item{`fontspec`}{(`font_spec`)\cr the font specification, if any, that is currently active (`list()` if none is).}
#'   \item{`spacewidth`}{(`numeric(1)`)\cr the width of the space character in the currently active font.}
#'   \item{`ismonospace`}{(`logical(1)`)\cr whether the specified font is monospaced.}
#' }
#'
#' `open_font_dev` opens a pdf device with the specified font
#' only if there is not one currently open with the same font.
#' If a new device is opened, it caches `spacewidth` and
#' `ismonospace` for use in `nchar_ttype`).
#'
#' `close_font_dev` closes any open font state device
#' and clears the cached values.
#'
#' @return
#' - `open_font_dev` returns a logical value indicating whether a *new* pdf device was opened.
#' - `close_font_dev` returns `NULL`.
#'
#' In both cases the value is returned invisibly.
#'
#' @examples
#' open_font_dev(font_spec("Times"))
#' nchar_ttype("Hiya there", font_spec("Times"))
#' close_font_dev()
#'
#' @export
open_font_dev <- function(fontspec, silent = FALSE) {
  ## XXX to remove for debugging only
  ## if(identical(fontspec, font_spec()))
  ##     stop()
  if (is.null(fontspec)) {
    return(invisible(FALSE))
  } else if (font_dev_state$open) {
    if (identical(font_dev_state$fontspec, fontspec)) {
      if (!silent && dev.cur() != font_dev_state$dev_num) {
        warning(
          "formatters is switching to the font state graphics device ",
          "to perform string width calculations. You may need to switch ",
          "to your currently open graphics device, depending on whether ",
          "the font device is closed and what other devices you have open."
        )
        dev.set(font_dev_state$dev_num)
      }
      return(invisible(FALSE))
    } else {
      close_font_dev()
    }
  } else if (FALSE && !font_dev_state$open) { ## remove 'FALSE &&' to get debugging info to helplocate places which aren't receiving/using the state properly # nolint
    # start nocov
    ## dump the call stack any time we have cache misses
    ## and have to open a completely new font state device
    scalls <- sys.calls()
    msg <- sapply(
      scalls[2:length(scalls)],
      function(sci) {
        toret <- deparse(sci[[1]], nlines = 3)
        if (substr(toret[1], 1, 8) == "function") {
          toret <- "anon function"
        }
        toret
      }
    )
    cat("\n***** START font dev debugging dump *****\n")
    cat(paste(msg, collapse = " -> "), "\n")
    print(fontspec)
  } #end nocov
  tmppdf <- tempfile(fileext = ".pdf")
  pdf(tmppdf)
  grid.newpage()
  gp <- gpar_from_fspec(fontspec)
  pushViewport(plotViewport(gp = gp))
  spcwidth <- cwidth_inches_unsafe(" ")
  ## XXX this assumes M or W is the widest "reasonable char"
  ## should be true for any reasonable font but not guaranteed!!!
  mwidth <- cwidth_inches_unsafe("M")
  wwidth <- cwidth_inches_unsafe("W")
  assign("open", TRUE, envir = font_dev_state)
  assign("fontspec", fontspec, envir = font_dev_state)
  assign("spacewidth", spcwidth, envir = font_dev_state)
  assign("ismonospace", spcwidth == cwidth_inches_unsafe("W"),
    envir = font_dev_state
  )
  assign("maxratio", max(mwidth, wwidth) / spcwidth,
    envir = font_dev_state
  )
  assign("dev_num", dev.cur(),
    envir = font_dev_state
  )
  invisible(TRUE)
}

#' @rdname open_font_dev
#' @export
close_font_dev <- function() {
  if (font_dev_state$open) {
    dev.off(font_dev_state$dev_num)
    assign("open", FALSE, envir = font_dev_state)
    assign("fontspec", list(), envir = font_dev_state)
    assign("spacewidth", NA_real_, envir = font_dev_state)
    assign("ismonospace", NA, envir = font_dev_state)
    assign("maxratio", NA_real_, envir = font_dev_state)
    assign("dev_num", NA_integer_, envir = font_dev_state)
  }
  invisible(NULL)
}

## can only be called when font_dev_state$open is TRUE
get_space_width <- function() {
  if (!font_dev_state$open) {
    stop(
      "get_space_width called when font dev state is not open. ",
      "This shouldn't happen, please contact the maintainers."
    )
  }
  font_dev_state$spacewidth
}

.open_fdev_is_monospace <- function() {
  if (!font_dev_state$open) {
    stop(
      ".open_fdev_is_monospace called when font dev state is not open. ",
      "This shouldn't happen, please contact the maintainers."
    )
  }
  font_dev_state$ismonospace
}

## safe wrapper around .open_fdev_is_monospace
is_monospace <- function(fontspec = font_spec(font_family, font_size, lineheight),
                         font_family = "Courier",
                         font_size = 8,
                         lineheight = 1) {
  if (is.null(fontspec)) {
    return(TRUE)
  }
  new_dev <- open_font_dev(fontspec)
  if (new_dev) {
    on.exit(close_font_dev())
  }
  .open_fdev_is_monospace()
}

get_max_wratio <- function() {
  if (!font_dev_state$open) {
    stop(
      "get_space_width called when font dev state is not open. ",
      "This shouldn't happen, please contact the maintainers."
    )
  }
  if (.open_fdev_is_monospace()) {
    1
  } else {
    font_dev_state$maxratio
  }
}

gpar_from_fspec <- function(fontspec) {
  gpar(
    fontfamily = fontspec$family,
    fontsize = fontspec$size,
    lineheight = fontspec$lineheight
  )
}

font_dev_is_open <- function() font_dev_state$open

#' Default horizontal separator
#'
#' The default horizontal separator character which can be displayed in the current
#' charset for use in rendering table-like objects.
#'
#' @param hsep_char (`string`)\cr character that will be set in the R environment
#'   options as the default horizontal separator. Must be a single character. Use
#'   `getOption("formatters_default_hsep")` to get its current value (`NULL` if not set).
#'
#' @return unicode 2014 (long dash for generating solid horizontal line) if in a
#'   locale that uses a UTF character set, otherwise an ASCII hyphen with a
#'   once-per-session warning.
#'
#' @examples
#' default_hsep()
#' set_default_hsep("o")
#' default_hsep()
#'
#' @name default_horizontal_sep
#' @export
default_hsep <- function() {
  system_default_hsep <- getOption("formatters_default_hsep")

  if (is.null(system_default_hsep)) {
    if (any(grepl("^UTF", utils::localeToCharset()))) {
      hsep <- "\u2014"
    } else {
      if (interactive()) {
        warning(
          "Detected non-UTF charset. Falling back to '-' ",
          "as default header/body separator. This warning ",
          "will only be shown once per R session."
        ) # nocov
      } # nocov
      hsep <- "-" # nocov
    }
  } else {
    hsep <- system_default_hsep
  }
  hsep
}

#' @name default_horizontal_sep
#' @export
set_default_hsep <- function(hsep_char) {
  checkmate::assert_character(hsep_char, n.chars = 1, len = 1, null.ok = TRUE)
  options("formatters_default_hsep" = hsep_char)
}

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

# Main function that does the wrapping
do_cell_fnotes_wrap <- function(mat, widths, max_width, tf_wrap, fontspec, expand_newlines = TRUE) {
  col_gap <- mf_colgap(mat)
  ncchar <- sum(widths) + (length(widths) - as.integer(mf_has_rlabels(mat))) * col_gap
  inset <- table_inset(mat)

  ## Text wrapping checks
  if (tf_wrap) {
    if (is.null(max_width)) {
      max_width <- getOption("width", 80L)
    } else if (is.character(max_width) && identical(max_width, "auto")) {
      max_width <- ncchar + inset
    }
    assert_number(max_width, lower = 0)
  }

  ## Check for having the right number of widths
  stopifnot(length(widths) == ncol(mat$strings))

  ## format the to ASCII
  cell_widths_mat <- .calc_cell_widths(mat, widths, col_gap)

  # Check that indentation is correct (it works only for body)
  .check_indentation(mat, row_col_width = cell_widths_mat[, 1, drop = TRUE])
  mod_ind_list <- .modify_indentation(mat, cell_widths_mat, do_what = "remove")
  mfs <- mod_ind_list[["mfs"]]
  cell_widths_mat <- mod_ind_list[["cell_widths_mat"]]

  # Main wrapper
  mf_strings(mat) <- matrix(
    unlist(mapply(wrap_string,
      str = mfs,
      width = cell_widths_mat,
      collapse = "\n",
      MoreArgs = list(fontspec = fontspec)
    )),
    ncol = ncol(mfs)
  )

  if (expand_newlines) {
    ## XXXXX this is wrong and will break for listings cause we don't know when
    ## we need has_topleft to be FALSE!!!!!!!!!!
    mat <- mform_handle_newlines(mat)

    ## this updates extents in rinfo AND nlines in ref_fnotes_df
    ## mat already has fontspec on it so no need to pass that down
    mat <- update_mf_nlines(mat, max_width = max_width)

    # Re-indenting
    mf_strings(mat) <- .modify_indentation(mat, cell_widths_mat, do_what = "add")[["mfs"]]
    .check_indentation(mat) # all went well
  }
  mat
}

# Helper function to see if body indentation matches (minimum)
# It sees if there is AT LEAST the indentation contained in rinfo
.check_indentation <- function(mat, row_col_width = NULL) {
  # mf_nrheader(mat) # not useful
  mf_nlh <- mf_nlheader(mat)
  mf_lgrp <- mf_lgrouping(mat)
  mf_str <- mf_strings(mat)
  # we base everything on the groupings -> unique indentation identifiers
  if (!is.null(mf_rinfo(mat))) { # this happens in rare cases with rtables::rtable()
    mf_ind <- c(rep(0, mf_nrheader(mat)), mf_rinfo(mat)$indent) # XXX to fix with topleft
  } else {
    mf_ind <- rep(0, mf_nrheader(mat))
  }
  ind_std <- paste0(rep(" ", mat$indent_size), collapse = "")

  # Expected indent (-x negative numbers should not appear at this stage)
  stopifnot(all(mf_ind >= 0))
  real_indent <- vapply(mf_ind, function(ii) {
    paste0(rep(ind_std, ii), collapse = "")
  }, character(1))

  if (!is.null(row_col_width) && any(row_col_width > 0) && !is.null(mf_rinfo(mat))) { # third is rare case
    # Self consistency test for row_col_width (same groups should have same width)
    # This should not be necessary (nocov)
    consistency_check <- vapply(unique(mf_lgrp), function(ii) {
      width_per_grp <- row_col_width[which(mf_lgrp == ii)]
      all(width_per_grp == width_per_grp[1])
    }, logical(1))
    stopifnot(all(consistency_check))

    # Taking only one width for each indentation grouping
    unique_row_col_width <- row_col_width[match(unique(mf_lgrp), mf_lgrp)]

    # Exception for check: case with summarize_row_groups and (hence) content_rows
    nchar_real_indent <- nchar(real_indent)
    body_rows <- seq(mf_nrheader(mat) + 1, length(nchar_real_indent))
    nchar_real_indent[body_rows] <- nchar_real_indent[body_rows] +
      as.numeric(mf_rinfo(mat)$node_class != "ContentRow")
    # xxx I think all of the above is a bit buggy honestly (check ContentRows!!!)

    if (any(nchar_real_indent > unique_row_col_width)) {
      stop(
        "Inserted width for row label column is not wide enough. ",
        "We found the following rows that do not have at least indentation * ind_size + 1",
        " characters to allow text to be shown after indentation: ",
        paste0(which(nchar(real_indent) + 1 > unique_row_col_width), collapse = " ")
      )
    }
  }

  # Main detector
  correct_indentation <- vapply(seq_along(mf_lgrp), function(xx) {
    grouping <- mf_lgrp[xx]
    if (nzchar(real_indent[grouping])) {
      has_correct_indentation <- stringi::stri_detect(
        mf_str[xx, 1],
        regex = paste0("^", real_indent[grouping])
      )
      return(has_correct_indentation || !nzchar(mf_str[xx, 1])) # "" is still an ok indentation
    }
    # Cases where no indent are true by definition
    return(TRUE)
  }, logical(1))

  if (any(!correct_indentation)) {
    stop(
      "We discovered indentation mismatches between the matrix_form and the indentation",
      " predefined in mf_rinfo. This should not happen. Contact the maintainer."
    ) # nocov
  }
}

# Helper function that takes out or adds the proper indentation
.modify_indentation <- function(mat, cell_widths_mat, do_what = c("remove", "add")) {
  # Extract info
  mfs <- mf_strings(mat) # we work on mfs
  mf_nlh <- mf_nlheader(mat)
  mf_l <- mf_lgrouping(mat)
  if (!is.null(mf_rinfo(mat))) { # this happens in rare cases with rtables::rtable()
    mf_ind <- c(rep(0, mf_nrheader(mat)), mf_rinfo(mat)$indent) # XXX to fix with topleft
  } else {
    mf_ind <- rep(0, mf_nrheader(mat))
  }
  stopifnot(length(mf_ind) == length(unique(mf_l))) # Check for indentation and grouping
  ind_std <- paste0(rep(" ", mat$indent_size), collapse = "") # standard size of indent 1

  # Create real indentation
  real_indent <- sapply(mf_ind, function(ii) paste0(rep(ind_std, ii), collapse = ""))

  # Use groupings to add or remove proper indentation
  lbl_row <- mfs[, 1, drop = TRUE]
  for (ii in seq_along(lbl_row)) {
    grp <- mf_l[ii]
    if (nzchar(real_indent[grp])) {
      # Update also the widths!!
      if (do_what[1] == "remove") {
        cell_widths_mat[ii, 1] <- cell_widths_mat[ii, 1] - nchar(real_indent[grp])
        mfs[ii, 1] <- stringi::stri_replace(lbl_row[ii], "", regex = paste0("^", real_indent[grp]))
      } else if (do_what[1] == "add") {
        mfs[ii, 1] <- paste0(real_indent[grp], lbl_row[ii])
      } else {
        stop("do_what needs to be remove or add.") # nocov
      }
    } else {
      mfs[ii, 1] <- lbl_row[ii]
    }
  }
  # Final return
  return(list("mfs" = mfs, "cell_widths_mat" = cell_widths_mat))
}

## take a character vector and return whether the value is
## a string version of a number or not
is_number_str <- function(vec) {
  is.na(as.numeric(vec))
}

is_dec_align <- function(vec) {
  # "c" is not an alignment method we define in `formatters`,
  # but the reverse dependency package `tables` will need
  sdiff <- setdiff(vec, c(list_valid_aligns(), "c"))
  if (length(sdiff) > 0) {
    stop("Invalid text-alignment(s): ", paste(sdiff, collapse = ", "))
  }
  grepl("dec", vec)
}

any_dec_align <- function(vec) any(is_dec_align(vec))

#' Decimal alignment
#'
#' Aligning decimal values of string matrix. Allowed alignments are: `dec_left`, `dec_right`,
#' and `decimal`.
#'
#' @param string_mat (`character matrix`)\cr "string" matrix component of `MatrixPrintForm` object.
#' @param align_mat (`character matrix`)\cr "aligns" matrix component of `MatrixPrintForm` object.
#'   Should contain either `dec_left`, `dec_right`, or `decimal` for values to be decimal aligned.
#'
#' @details Left and right decimal alignment (`dec_left` and `dec_right`) differ from center decimal
#'   alignment (`decimal`) only when there is padding present. This may occur if column widths are
#'   set wider via parameters `widths` in `toString` or `colwidths` in `paginate_*`. More commonly,
#'   it also occurs when column names are wider. Cell wrapping is not supported when decimal
#'   alignment is used.
#'
#' @return A processed string matrix of class `MatrixPrintForm` with decimal-aligned values.
#'
#' @seealso [toString()], [MatrixPrintForm()]
#'
#' @examples
#' dfmf <- basic_matrix_form(mtcars[1:5, ])
#' aligns <- mf_aligns(dfmf)
#' aligns[, -c(1)] <- "dec_left"
#' decimal_align(mf_strings(dfmf), aligns)
#'
#' @export
decimal_align <- function(string_mat, align_mat) {
  ## Evaluate if any values are to be decimal aligned
  if (!any_dec_align(align_mat)) {
    return(string_mat)
  }
  for (i in seq(1, ncol(string_mat))) {
    ## Take a column and its decimal alignments
    col_i <- as.character(string_mat[, i])
    align_col_i <- is_dec_align(align_mat[, i])

    ## !( A || B) -> !A && !B  DeMorgan's Law
    ## Are there any values to be decimal aligned? safe if
    if (any(align_col_i) && any(!grepl("^[0-9]\\.", col_i))) {
      ## Extract values not to be aligned (NAs, non-numbers,
      ##  doesn't say "decimal" in alignment matrix)
      ## XXX FIXME because this happens after formatting, we can't tell the difference between
      ## non-number strings which come from na_str+ NA  value and strings which just aren't numbers.
      ## this is a problem that should eventually be fixed.
      nas <- vapply(col_i, is.na, FUN.VALUE = logical(1))
      nonnum <- !grepl("[0-9]", col_i)
      ## No grepl("[a-zA-Z]", col_i) because this excludes N=xx, e.g.
      nonalign <- nas | nonnum | !align_col_i
      col_ia <- col_i[!nonalign]

      ## Do decimal alignment
      if (length(col_ia) > 0) {
        # Special case: scientific notation
        has_sc_not <- grepl("\\d+[e|E][\\+|\\-]\\d+", col_ia)
        if (any(has_sc_not)) {
          stop(
            "Found values using scientific notation between the ones that",
            " needs to be decimal aligned (aligns is decimal, dec_left or dec_right).",
            " Please consider using format functions to get a complete decimal ",
            "(e.g. formatC)."
          )
        }

        ## Count the number of numbers in the string
        matches <- gregexpr("\\d+\\.\\d+|\\d+", col_ia)
        more_than_one <- vapply(matches, function(x) {
          sum(attr(x, "match.length") > 0) > 1
        }, logical(1))
        ## Throw error in case any have more than 1 numbers
        if (any(more_than_one)) {
          stop(
            "Decimal alignment is not supported for multiple values. ",
            "Found the following string with multiple numbers ",
            "(first 3 selected from column ", col_i[1], "): '",
            paste0(col_ia[more_than_one][seq(1, 3)], collapse = "', '"),
            "'"
          )
        }
        ## General split (only one match -> the first)
        main_regexp <- regexpr("\\d+", col_ia)
        left <- regmatches(col_ia, main_regexp, invert = FALSE)
        right <- regmatches(col_ia, main_regexp, invert = TRUE)
        right <- sapply(right, "[[", 2)
        something_left <- sapply(strsplit(col_ia, "\\d+"), "[[", 1)
        left <- paste0(something_left, left)
        if (!checkmate::test_set_equal(paste0(left, right), col_ia)) {
          stop(
            "Split string list lost some piece along the way. This ",
            "should not have happened. Please contact the maintainer."
          )
        } # nocov
        separator <- sapply(right, function(x) {
          if (nzchar(x)) {
            substr(x, 1, 1)
          } else {
            c(" ")
          }
        }, USE.NAMES = FALSE)
        right <- sapply(right, function(x) {
          if (nchar(x) > 1) {
            substr(x, 2, nchar(x))
          } else {
            c("")
          }
        }, USE.NAMES = FALSE)
        ## figure out whether we need space separators (at least one had a "." or not)
        if (!any(grepl("[^[:space:]]", separator))) {
          separator <- gsub("[[:space:]]*", "", separator)
        }
        ## modify the piece with spaces
        left_mod <- paste0(spaces(max(nchar(left), na.rm = TRUE) - nchar(left)), left)
        right_mod <- paste0(right, spaces(max(nchar(right), na.rm = TRUE) - nchar(right)))
        # Put everything together
        aligned <- paste(left_mod, separator, right_mod, sep = "")
        string_mat[!nonalign, i] <- aligned
      }
    }
  }
  string_mat
}

## this gives the conversion from number of spaces to number of characters
## for use in, e.g., repping out divider lines.
calc_str_adj <- function(str, fontspec) {
  nchar(str) / nchar_ttype(str, fontspec, raw = TRUE)
}

# toString ---------------------------------------------------------------------
# main printing code for MatrixPrintForm

#' @description
#' All objects that are printed to console pass via `toString`. This function allows
#' fundamental formatting specifications to be applied to final output, like column widths
#' and relative wrapping (`width`), title and footer wrapping (`tf_wrap = TRUE` and
#' `max_width`), and horizontal separator character (e.g. `hsep = "+"`).
#'
#' @inheritParams MatrixPrintForm
#' @inheritParams open_font_dev
#' @param widths (`numeric` or  `NULL`)\cr Proposed widths for the columns of `x`. The expected
#'   length of this numeric vector can be retrieved with `ncol(x) + 1` as the column of row names
#'   must also be considered.
#' @param hsep (`string`)\cr character to repeat to create header/body separator line. If
#'   `NULL`, the object value will be used. If `" "`, an empty separator will be printed. See
#'   [default_hsep()] for more information.
#' @param tf_wrap (`flag`)\cr whether the text for title, subtitles, and footnotes should be wrapped.
#' @param max_width (`integer(1)`, `string` or `NULL`)\cr width that title and footer (including
#'   footnotes) materials should be word-wrapped to. If `NULL`, it is set to the current print width of the
#'   session (`getOption("width")`). If set to `"auto"`, the width of the table (plus any table inset) is
#'   used. Parameter is ignored if `tf_wrap = FALSE`.
#' @param ttype_ok logical(1). Should truetype (non-monospace) fonts be
#' allowed via `fontspec`. Defaults to `FALSE`. This parameter is primarily
#' for internal testing and generally should not be set by end users.
#'
#' @details
#' Manual insertion of newlines is not supported when `tf_wrap = TRUE` and will result in a warning and
#' undefined wrapping behavior. Passing vectors of already split strings remains supported, however in this
#' case each string is word-wrapped separately with the behavior described above.
#'
#' @return A character string containing the ASCII rendering of the table-like object represented by `x`.
#'
#' @seealso [wrap_string()]
#'
#' @examples
#' mform <- basic_matrix_form(mtcars)
#' cat(toString(mform))
#'
#' @rdname tostring
#' @exportMethod toString
setMethod("toString", "MatrixPrintForm", function(x,
                                                  widths = NULL,
                                                  tf_wrap = FALSE,
                                                  max_width = NULL,
                                                  col_gap = mf_colgap(x),
                                                  hsep = NULL,
                                                  fontspec = font_spec(),
                                                  ttype_ok = FALSE) {
  checkmate::assert_flag(tf_wrap)

  ## we are going to use the pdf device and grid to understand the actual
  ## print width of things given our font family and font size
  new_dev <- open_font_dev(fontspec)
  if (new_dev) {
    on.exit(close_font_dev())
  }

  if (!is_monospace(fontspec = fontspec) && !ttype_ok) {
    stop(
      "non-monospace font specified in toString call; this would result in cells contents not lining up exactly. ",
      "If you truly want this behavior please set ttype_ok = TRUE in the call to toString/export_as_txt/export_as_pdf"
    )
  }
  mat <- matrix_form(x, indent_rownames = TRUE, fontspec = fontspec)

  # Check for \n in mat strings -> if there are any, matrix_form did not work
  if (any(grepl("\n", mf_strings(mat)))) {
    stop(
      "Found newline characters (\\n) in string matrix produced by matrix_form. ",
      "This is not supported and implies missbehavior on the first parsing (in matrix_form). ",
      "Please contact the maintainer or file an issue."
    ) # nocov
  }
  if (any(grepl("\r", mf_strings(mat)))) {
    stop(
      "Found recursive special characters (\\r) in string matrix produced by matrix_form. ",
      "This special character is not supported and should be removed."
    ) # nocov
  }

  # Check that expansion worked for header -> should not happen
  if (!is.null(mf_rinfo(mat)) && # rare case of rtables::rtable()
    (length(mf_lgrouping(mat)) != nrow(mf_strings(mat)) || # non-unique grouping test # nolint
      mf_nrheader(mat) + nrow(mf_rinfo(mat)) != length(unique(mf_lgrouping(mat))))) { # nolint
    stop(
      "The sum of the expected nrows header and nrows of content table does ",
      "not match the number of rows in the string matrix. To our knowledge, ",
      "this is usually of a problem in solving newline characters (\\n) in the header. ",
      "Please contact the maintaner or file an issue."
    ) # nocov
  }

  inset <- table_inset(mat)

  # if cells are decimal aligned, run propose column widths
  # if the provided widths is less than proposed width, return an error
  if (any_dec_align(mf_aligns(mat))) {
    aligned <- propose_column_widths(x, fontspec = fontspec)

    # catch any columns that require widths more than what is provided
    if (!is.null(widths)) {
      how_wide <- sapply(seq_along(widths), function(i) c(widths[i] - aligned[i]))
      too_wide <- how_wide < 0
      if (any(too_wide)) {
        desc_width <- paste(paste(
          names(which(too_wide)),
          paste0("(", how_wide[too_wide], ")")
        ), collapse = ", ")
        stop(
          "Inserted width(s) for column(s) ", desc_width,
          " is(are) not wide enough for the desired alignment."
        )
      }
    }
  }

  # Column widths are fixed here
  if (is.null(widths)) {
    # if mf does not have widths -> propose them
    widths <- mf_col_widths(x) %||% propose_column_widths(x, fontspec = fontspec)
  } else {
    mf_col_widths(x) <- widths
  }

  ## Total number of characters for the table
  ## col_gap (and table inset) are in number of spaces
  ## so we're ok here even in the truetype case
  ncchar <- sum(widths) + (length(widths) - 1) * col_gap

  ## max_width for wrapping titles and footers (not related to ncchar if not indirectly)
  max_width <- .handle_max_width(
    tf_wrap = tf_wrap,
    max_width = max_width,
    colwidths = widths,
    col_gap = col_gap,
    inset = inset
  )

  # Main wrapper function for table core
  mat <- do_cell_fnotes_wrap(mat, widths, max_width = max_width, tf_wrap = tf_wrap, fontspec = fontspec)

  body <- mf_strings(mat)
  aligns <- mf_aligns(mat)
  keep_mat <- mf_display(mat)
  ## spans <- mat$spans
  mf_ri <- mf_rinfo(mat)
  ref_fnotes <- mf_rfnotes(mat)
  nl_header <- mf_nlheader(mat)

  cell_widths_mat <- .calc_cell_widths(mat, widths, col_gap)

  # decimal alignment
  if (any_dec_align(aligns)) {
    body <- decimal_align(body, aligns)
  }

  # Content is a matrix of cells with the right amount of spaces
  content <- matrix(
    mapply(padstr, body, cell_widths_mat, aligns, MoreArgs = list(fontspec = fontspec)),
    ncol = ncol(body)
  )
  content[!keep_mat] <- NA

  # Define gap string and divisor string
  gap_str <- strrep(" ", col_gap)
  if (is.null(hsep)) {
    hsep <- horizontal_sep(mat)
  }
  adj_hsep <- calc_str_adj(hsep, fontspec)
  div <- substr(strrep(hsep, ceiling(ncchar * adj_hsep)), 1, ceiling(ncchar * adj_hsep))
  hsd <- header_section_div(mat)
  if (!is.na(hsd)) {
    adj_hsd <- calc_str_adj(hsd, fontspec)
    hsd <- substr(strrep(hsd, ceiling(ncchar * adj_hsd)), 1, ceiling(ncchar * adj_hsd))
  } else {
    hsd <- NULL # no divisor
  }

  # text head (paste w/o NA content header and gap string)
  txt_head <- apply(head(content, nl_header), 1, .paste_no_na, collapse = gap_str)

  # txt body
  sec_seps_df <- mf_ri[, c("abs_rownumber", "trailing_sep"), drop = FALSE]
  if (!is.null(sec_seps_df) && any(!is.na(sec_seps_df$trailing_sep))) {
    bdy_cont <- tail(content, -nl_header)
    ## unfortunately we count "header rows" wrt line grouping so it
    ## doesn't match the real (i.e. body) rows as is
    row_grouping <- tail(mf_lgrouping(mat), -nl_header) - mf_nrheader(mat)
    nrbody <- NROW(bdy_cont)
    stopifnot(length(row_grouping) == nrbody)
    ## all rows with non-NA section divs and the final row (regardless of NA status)
    ## fixes #77
    sec_seps_df <- sec_seps_df[unique(c(
      which(!is.na(sec_seps_df$trailing_sep)),
      NROW(sec_seps_df)
    )), ]
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
          adj_i <- calc_str_adj(sec_seps_df$trailing_sep[i], fontspec)
          substr(
            strrep(sec_seps_df$trailing_sep[i], ceiling(ncchar * adj_i)), 1,
            ceiling((ncchar - inset) * adj_i)
          )
        }
      )
      sec_strt <- sec_end + 1
    }
  } else {
    # This is the usual default pasting
    txt_body <- apply(tail(content, -nl_header), 1, .paste_no_na, collapse = gap_str)
  }

  # retrieving titles and footers
  allts <- all_titles(mat)

  ref_fnotes <- reorder_ref_fnotes(ref_fnotes)
  # Fix for ref_fnotes with \n characters XXX this does not count in the pagination
  if (any(grepl("\\n", ref_fnotes))) {
    ref_fnotes <- unlist(strsplit(ref_fnotes, "\n", fixed = TRUE))
  }

  allfoots <- list(
    "main_footer" = main_footer(mat),
    "prov_footer" = prov_footer(mat),
    "ref_footnotes" = ref_fnotes
  )
  allfoots <- allfoots[!sapply(allfoots, is.null)]

  ## Wrapping titles if they go beyond the horizontally allowed space
  if (tf_wrap) {
    new_line_warning(allts)
    allts <- wrap_txt(allts, max_width, fontspec = fontspec)
  }
  titles_txt <- if (any(nzchar(allts))) c(allts, "", .do_inset(div, inset)) else NULL

  # Wrapping footers if they go beyond the horizontally allowed space
  if (tf_wrap) {
    new_line_warning(allfoots)
    allfoots$main_footer <- wrap_txt(allfoots$main_footer, max_width - inset, fontspec = fontspec)
    allfoots$ref_footnotes <- wrap_txt(allfoots$ref_footnotes, max_width - inset, fontspec = fontspec)
    ## no - inset here because the prov_footer is not inset
    allfoots$prov_footer <- wrap_txt(allfoots$prov_footer, max_width, fontspec = fontspec)
  }

  # Final return
  paste0(
    paste(c(
      titles_txt, # .do_inset(div, inset) happens if there are any titles
      .do_inset(txt_head, inset),
      .do_inset(div, inset),
      .do_inset(hsd, inset), # header_section_div if present
      .do_inset(txt_body, inset),
      .footer_inset_helper(allfoots, div, inset)
    ), collapse = "\n"),
    "\n"
  )
})

# Switcher for the 3 options for max_width (NULL, numeric, "auto"))
.handle_max_width <- function(tf_wrap, max_width,
                              cpp = NULL, # Defaults to getOption("width", 80L)
                              # Things for auto
                              inset = NULL, colwidths = NULL, col_gap = NULL) {
  max_width <- if (!tf_wrap) {
    if (!is.null(max_width)) {
      warning("tf_wrap is FALSE - ignoring non-null max_width value.")
    }
    NULL
  } else if (tf_wrap) {
    if (is.null(max_width)) {
      if (is.null(cpp) || is.na(cpp)) {
        getOption("width", 80L)
      } else {
        cpp
      }
    } else if (is.numeric(max_width)) {
      max_width
    } else if (is.character(max_width) && identical(max_width, "auto")) {
      # This should not happen, but just in case
      if (any(sapply(list(inset, colwidths, col_gap), is.null))) {
        stop("inset, colwidths, and col_gap must all be non-null when max_width is \"auto\".")
      }
      inset + sum(colwidths) + (length(colwidths) - 1) * col_gap
    } else {
      stop("max_width must be NULL, a numeric value, or \"auto\".")
    }
  }
  return(max_width)
}

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

reorder_ref_fnotes <- function(fns) {
  ind <- gsub("\\{(.*)\\}.*", "\\1", fns)
  ind_num <- suppressWarnings(as.numeric(ind))
  is_num <- !is.na(ind_num)
  is_asis <- ind == fns

  if (all(is_num)) {
    ord_num <- order(ind_num)
    ord_char <- NULL
    ord_other <- NULL
  } else {
    ord_num <- order(ind_num[is_num])
    ord_char <- order(ind[!is_num & !is_asis])
    ord_other <- order(ind[is_asis])
  }
  c(fns[is_num][ord_num], fns[!is_num & !is_asis][ord_char], ind[is_asis][ord_other])
}

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

#' Wrap a string to a precise width
#'
#' Core wrapping functionality that preserves whitespace. Newline character `"\n"` is not supported
#' by core functionality [stringi::stri_wrap()]. This is usually solved beforehand by [matrix_form()].
#' If the width is smaller than any large word, these will be truncated after `width` characters. If
#' the split leaves trailing groups of empty spaces, they will be dropped.
#'
#' @inheritParams open_font_dev
#' @param str (`string`, `character`, or `list`)\cr string to be wrapped. If it is a `vector` or
#'   a `list`, it will be looped as a `list` and returned with `unlist(use.names = FALSE)`.
#' @param width (`numeric(1)`)\cr width, in characters, that the text should be wrapped to.
#' @param collapse (`string` or `NULL`)\cr collapse character used to separate segments of words that
#'   have been split and should be pasted together. This is usually done internally with `"\n"` to update
#'   the wrapping along with other internal values.
#'
#' @details Word wrapping happens similarly to [stringi::stri_wrap()] with the following difference: individual
#'   words which are longer than `max_width` are broken up in a way that fits with other word wrapping.
#'
#' @return A string if `str` is one element and if `collapse = NULL`. Otherwise, a list of elements
#'   (if `length(str) > 1`) that can contain strings or vectors of characters (if `collapse = NULL`).
#'
#' @examples
#' str <- list(
#'   "  , something really  \\tnot  very good", # \t needs to be escaped
#'   "  but I keep it12   "
#' )
#' wrap_string(str, 5, collapse = "\n")
#'
#' @export
wrap_string <- function(str, width, collapse = NULL, fontspec = font_spec()) {
  if (length(str) > 1) {
    return(
      unlist(
        lapply(str, wrap_string, width = width, collapse = collapse, fontspec = fontspec),
        use.names = FALSE
      )
    )
  }
  str <- unlist(str, use.names = FALSE) # it happens is one list element
  if (!length(str) || !nzchar(str) || is.na(str)) {
    return(str)
  }
  checkmate::assert_character(str)
  checkmate::assert_int(width, lower = 1)

  if (any(grepl("\\n", str))) {
    stop(
      "Found \\n in a string that was meant to be wrapped. This should not happen ",
      "because matrix_form should take care of them before this step (toString, ",
      "i.e. the printing machinery). Please contact the maintaner or file an issue."
    )
  }

  if (!is_monospace(fontspec)) {
    return(wrap_string_ttype(str, width, fontspec, collapse = collapse))
  }

  # str can be also a vector or list. In this case simplify manages the output
  ret <- .go_stri_wrap(str, width)

  # Check if it went fine
  if (any(nchar_ttype(ret, fontspec) > width)) {
    which_exceeded <- which(nchar_ttype(ret, fontspec) > width)

    # Recursive for loop to take word interval
    while (length(which_exceeded) > 0) {
      we_i <- which_exceeded[1]
      # Is there space for some part of the next word?
      char_threshold <- width * (2 / 3) + 0.01 # if too little space -> no previous word
      smart_condition <- nchar_ttype(ret[we_i - 1], fontspec) + 1 < char_threshold # +1 is for spaces
      if (we_i - 1 > 0 && smart_condition) {
        we_interval <- unique(c(we_i - 1, we_i))
        we_interval <- we_interval[
          (we_interval < (length(ret) + 1)) &
            (we_interval > 0)
        ]
      } else {
        we_interval <- we_i
      }
      # Split words and collapse (needs unique afterwards)
      ret[we_interval] <- split_words_by(
        paste0(ret[we_interval], collapse = " "),
        width
      )
      # Taking out repetitions if there are more than one
      if (length(we_interval) > 1) {
        ret <- ret[-we_interval[-1]]
        we_interval <- we_interval[1]
      }
      # Paste together and rerun if it is not the same as original ret
      ret_collapse <- paste0(ret, collapse = " ")

      # Checking if we are stuck in a loop
      ori_wrapped_txt_v <- .go_stri_wrap(str, width)
      cur_wrapped_txt_v <- .go_stri_wrap(ret_collapse, width)
      broken_char_ori <- sum(nchar_ttype(ori_wrapped_txt_v, fontspec) > width) # how many issues there were
      broken_char_cur <- sum(nchar_ttype(cur_wrapped_txt_v, fontspec) > width) # how many issues there are

      # if still broken, we did not solve the current issue!
      if (setequal(ori_wrapped_txt_v, cur_wrapped_txt_v) || broken_char_cur >= broken_char_ori) {
        # help function: Very rare case where the recursion is stuck in a loop
        ret_tmp <- force_split_words_by(ret[we_interval], width) # here we_interval is only one ind
        ret <- append(ret, ret_tmp, we_interval)[-we_interval]
        which_exceeded <- which(nchar_ttype(ret, fontspec) > width)
      } else {
        return(wrap_string(str = ret_collapse, width = width, collapse = collapse))
      }
    }
  }

  if (!is.null(collapse)) {
    return(paste0(ret, collapse = collapse))
  }

  return(ret)
}

.go_stri_wrap <- function(str, w) {
  if (is.na(w) || is.nan(w)) {
    browser()
  }
  if (w < 1) {
    return(str)
  }
  stringi::stri_wrap(str,
    width = w,
    normalize = FALSE, # keeps spaces
    simplify = TRUE, # If FALSE makes it a list with str elements
    indent = 0,
    use_length = FALSE # incase the defaul changes, use actual char widths
  )
}

#' @rdname wrap_string_ttype
#' @export
split_word_ttype <- function(str, width, fontspec, min_ok_chars) {
  chrs <- strsplit(str, "")[[1]]
  nctt_chars <- nchar_ttype(chrs, fontspec, raw = TRUE)
  ok <- which(cumsum(nctt_chars) <= width)
  if (length(ok) < min_ok_chars || length(chrs) - length(ok) < min_ok_chars) {
    list(
      ok = character(),
      remainder = str
    )
  } else {
    list(
      ok = substr(str, 1, length(ok)),
      remainder = substr(str, length(ok) + 1, nchar(str))
    )
  }
}

## need a separate path here because **the number of characters**
## in each part is no longer going to be constant the way it
## was for monospace
## this is much slower but still shouldn't be a bottleneck, if it is we'll
## have to do something else
#' wrap string given a Truetype font
#'
#' @inheritParams wrap_string
#' @param min_ok_chars numeric(1). Number of minimum characters which much remain
#' on either side when a word is split
#' @param wordbreak_ok logical(1). Should breaking within a word be allowed. If, `FALSE`,
#' attempts to wrap a string to a width narrower than its widest word will result
#' in an error.
#' @return `str`, broken up into a word-wrapped vector
#' @export
wrap_string_ttype <- function(str,
                              width,
                              fontspec,
                              collapse = NULL,
                              min_ok_chars = min(floor(nchar(str) / 2), 4, floor(width / 2)),
                              wordbreak_ok = TRUE) {
  newdev <- open_font_dev(fontspec)
  if (newdev) {
    on.exit(close_font_dev())
  }

  rawspls <- strsplit(str, "[[:space:]](?=[^[:space:]])", perl = TRUE)[[1]] # preserve all but one space
  nctt <- nchar_ttype(rawspls, fontspec, raw = TRUE)
  pts <- which(cumsum(nctt) <= width)
  if (length(pts) == length(rawspls)) { ## no splitting needed
    return(str)
  } else if (length(pts) == 0) { ## no spaces, all one word, split it and keep going
    if (wordbreak_ok) {
      inner_res <- list()
      min_ok_inner <- min_ok_chars
      while (length(inner_res$ok) == 0) {
        inner_res <- split_word_ttype(rawspls[1], width, fontspec, min_ok_inner) # min_ok_chars)
        min_ok_inner <- floor(min_ok_inner / 2)
      }
      done <- inner_res$ok
      remainder <- paste(c(inner_res$remainder, rawspls[-1]), collapse = " ")
    } else {
      stop(
        "Unable to find word wrapping solution without breaking word: ",
        rawspls[[1]], " [requires  ", nchar_ttype(rawspls[[1]], fontspec), " spaces of width, out of ",
        width, " available]."
      )
    }
  } else { ## some words fit, and some words don't
    done_tmp <- paste(rawspls[pts], collapse = " ")
    tospl_tmp <- rawspls[length(pts) + 1]
    width_tmp <- width - sum(nctt[pts])
    if (wordbreak_ok && width_tmp / width > .33) {
      inner_res <- split_word_ttype(tospl_tmp, width_tmp, fontspec,
        min_ok_chars = min_ok_chars
      )
    } else {
      inner_res <- list(done = "", remainder = tospl_tmp)
    }
    done <- paste(c(rawspls[pts], inner_res$ok),
      collapse = " "
    )
    remainder <- paste(
      c(
        inner_res$remainder,
        if (length(rawspls) > length(pts) + 1) tail(rawspls, -(length(pts) + 1))
      ),
      collapse = " "
    )
  }
  ret <- c(
    done,
    wrap_string_ttype(remainder, width, fontspec)
  )
  if (!is.null(collapse)) {
    ret <- paste(ret, collapse = collapse)
  }
  ret
}

## w comes in in terms of number of spaces, but
## we need the more generic "number of characters"
## to pass to stringi::stri_wrap
#' @importFrom stats quantile
.ttype_adjust_width <- function(str, w, fontspec, adj_qntl) {
  ## we are going to be conservative in the truetype
  ## case, but monospace behavior shouldn't change
  ## so just immediately return w.
  new_dev <- open_font_dev(fontspec)
  if (new_dev) {
    on.exit(close_font_dev())
  }

  if (is_monospace(fontspec = fontspec) || nchar(str) == 0) {
    return(w)
  }
  nchars <- nchar(str)
  ind_chars <- strsplit(str, "")[[1]]
  nspaces <- nchar_ttype(ind_chars, fontspec, raw = TRUE)
  if (length(nspaces) == 1) {
    spc_per_char <- nspaces
  } else {
    ## use 75th percentile as conservative
    ## "character width"...
    ## if 75% of chars in str are that wide
    ## or narrower, having enough of them
    ## in a row to make wrapping do the wrong
    ## thing  seems very unlikely
    spc_per_char <- quantile(nspaces, adj_qntl)
  }

  ## convert from w spaces to adj_w characters
  ## where a "character" takes up the average
  ## width of characters in str
  ## then be a bit conservative with floor
  adj_w <- max(1L, floor(w / spc_per_char))
  adj_w
}

# help function: Very rare case where the recursion is stuck in a loop
force_split_words_by <- function(ret, width) {
  which_exceeded <- which(nchar(ret) > width)
  ret_tmp <- NULL
  for (ii in seq_along(ret)) {
    if (ii %in% which_exceeded) {
      wrd_i <- ret[ii]
      init_v <- seq(1, nchar(wrd_i), by = width)
      end_v <- c(init_v[-1] - 1, nchar(wrd_i))
      str_v_tmp <- stringi::stri_sub(wrd_i, from = init_v, to = end_v)
      ret_tmp <- c(ret_tmp, str_v_tmp[!grepl("^\\s+$", str_v_tmp) & nzchar(str_v_tmp)])
    } else {
      ret_tmp <- c(ret_tmp, ret[ii])
    }
  }
  ret_tmp
}

# Helper fnc to split the words and collapse them with space
split_words_by <- function(wrd, width) {
  vapply(wrd, function(wrd_i) {
    init_v <- seq(1, nchar(wrd_i), by = width)
    end_v <- c(init_v[-1] - 1, nchar(wrd_i))
    fin_str_v <- substring(wrd_i, init_v, end_v)
    is_only_spaces <- grepl("^\\s+$", fin_str_v)
    # We pop only spaces at this point
    if (all(is_only_spaces)) {
      fin_str_v <- fin_str_v[1] # keep only one width-sized empty
    } else {
      fin_str_v <- fin_str_v[!is_only_spaces] # hybrid text + \s
    }

    # Collapse the string
    paste0(fin_str_v, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

#' @describeIn wrap_string Deprecated function. Please use [wrap_string()] instead.
#'
#' @examples
#' wrap_txt(str, 5, collapse = NULL)
#'
#' @export
wrap_txt <- function(str, width, collapse = NULL, fontspec = font_spec()) {
  new_dev <- open_font_dev(fontspec)
  if (new_dev) {
    on.exit(close_font_dev())
  }

  unlist(wrap_string(str, width, collapse, fontspec = fontspec), use.names = FALSE)
}

pad_vert_top <- function(x, len, default = "") {
  c(x, rep(default, len - length(x)))
}

pad_vert_bottom <- function(x, len, default = "") {
  c(rep(default, len - length(x)), x)
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

#' Transform a vector of spans (with duplication) into a visibility vector
#'
#' @param spans (`numeric`)\cr a vector of spans, with each span value repeated
#'   for the cells it covers.
#'
#' @details
#' The values of `spans` are assumed to be repeated such that each individual position covered by the
#' span has the repeated value.
#'
#' This means that each block of values in `spans` must be of a length at least equal to its value
#' (i.e. two 2s, three 3s, etc).
#'
#' This function correctly handles cases where two spans of the same size are next to each other;
#' i.e., a block of four 2s represents two large cells each of which spans two individual cells.
#'
#' @return A logical vector the same length as `spans` indicating whether the contents of a string vector
#'   with those spans is valid.
#'
#' @note
#' Currently no checking or enforcement is done to verify that the vector of spans is valid according to
#' the specifications described in the Details section above.
#'
#' @examples
#' spans_to_viscell(c(2, 2, 2, 2, 1, 3, 3, 3))
#'
#' @export
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

#' Propose column widths based on the `MatrixPrintForm` of an object
#'
#' Row names are also considered a column for the output.
#'
#' @inheritParams open_font_dev
#' @param x (`ANY`)\cr a `MatrixPrintForm` object, or an object with a `matrix_form` method.
#' @param indent_size (`numeric(1)`)\cr indent size, in characters. Ignored when `x` is already
#'   a `MatrixPrintForm` object in favor of information there.
#'
#' @return A vector of column widths based on the content of `x` for use in printing and pagination.
#'
#' @examples
#' mf <- basic_matrix_form(mtcars)
#' propose_column_widths(mf)
#'
#' @export
propose_column_widths <- function(x,
                                  indent_size = 2,
                                  fontspec = font_spec()) {
  new_dev <- open_font_dev(fontspec)
  if (new_dev) {
    on.exit(close_font_dev())
  }

  if (!is(x, "MatrixPrintForm")) {
    x <- matrix_form(x, indent_rownames = TRUE, indent_size = indent_size, fontspec = fontspec)
  }
  body <- mf_strings(x)
  spans <- mf_spans(x)
  aligns <- mf_aligns(x)
  display <- mf_display(x)

  # compute decimal alignment if asked in alignment matrix
  if (any_dec_align(aligns)) {
    body <- decimal_align(body, aligns)
  }

  ## chars <- nchar(body) #old monospace assumption
  ## we now use widths in terms of the printwidth of the space (" ")
  ## character. This collapses to the same thing in the monospace
  ## case but allows us to reasonably support truetype fonts
  chars <- nchar_ttype(body, fontspec)

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
            ## widths[i_cols] <- cw_i + spread_width(nchar_i - available_width, length(cw_i))
          }
        }
      }
    }
  }
  widths
}

## "number of characters" width in terms of
## width of " " for the chosen font family

## pdf device with font specification MUST already be open

#' Calculate font-specific string width
#'
#' This function returns the width of each element `x`
#' *as a multiple of the width of the space character
#' for in declared font*, rounded up to the nearest
#' integer. This is used extensively in the text rendering
#' ([toString()]) and pagination machinery for
#' calculating word wrapping, default column widths,
#' lines per page, etc.
#'
#' @param x (`character`)\cr the string(s) to calculate width(s) for.
#' @param fontspec (`font_spec` or `NULL`)\cr if non-NULL, the font to use for
#'   the calculations (as returned by [font_spec()]). Defaults to "Courier",
#'   which is a monospace font. If NULL, the width will be returned
#'   in number of characters by calling `nchar` directly.
#' @param tol (`numeric(1)`)\cr the tolerance to use when determining
#'   if a multiple needs to be rounded up to the next integer. See
#'   Details.
#' @param raw (`logical(1)`)\cr whether unrounded widths should be returned. Defaults to `FALSE`.
#'
#' @details String width is defined in terms of spaces within
#' the specified font. For monospace fonts, this definition
#' collapses to the number of characters in the string
#' ([nchar()]), but for truetype fonts it does not.
#'
#' For `raw = FALSE`, non-integer values (the norm in a truetype
#' setting) for the number of spaces a string takes up is rounded
#' up, *unless the multiple is less than `tol` above the last integer
#' before it*. E.g., if `k - num_spaces < tol` for an integer
#' `k`, `k` is returned instead of `k+1`.
#'
#' @seealso [font_spec()]
#'
#' @examples
#' nchar_ttype("hi there!")
#'
#' nchar_ttype("hi there!", font_spec("Times"))
#'
#' @export
nchar_ttype <- function(x, fontspec = font_spec(), tol = sqrt(.Machine$double.eps), raw = FALSE) {
  ## escape hatch because sometimes we need to call, e.g. make_row_df
  ## but we dont' care about getting the word wrapping right and the
  ## performance penalty was KILLING us. Looking at you
  ## rtables::update_ref_indexing @.@
  if (is.null(fontspec)) {
    return(nchar(x))
  }
  new_dev <- open_font_dev(fontspec)
  if (new_dev) {
    on.exit(close_font_dev())
  }
  if (font_dev_state$ismonospace) { ## WAY faster if we can do it
    return(nchar(x))
  }
  space_width <- get_space_width()
  ## cwidth_inches_unsafe is ok here because if we don't
  ## have a successfully opened state (somehow), get_space_width
  ## above will error.
  num_inches_raw <- vapply(x, cwidth_inches_unsafe, 1.0)
  num_spaces_raw <- num_inches_raw / space_width
  if (!raw) {
    num_spaces_ceil <- ceiling(num_spaces_raw)
    ## we don't want to add one when the answer is e.g, 3.0000000000000953
    within_tol <- which(num_spaces_raw + 1 - num_spaces_ceil <= tol)
    ret <- num_spaces_ceil
    if (length(within_tol) == 0L) {
      ret[within_tol] <- floor(num_spaces_raw[within_tol])
    }
  } else {
    ret <- num_spaces_raw
  }
  if (!is.null(dim(x))) {
    dim(ret) <- dim(x)
  } else {
    names(ret) <- NULL
  }
  ret
}

#' Pad a string and align within string
#'
#' @inheritParams open_font_dev
#' @param x (`string`)\cr a string.
#' @param n (`integer(1)`)\cr number of characters in the output string. If `n < nchar(x)`, an error is thrown.
#' @param just (`string`)\cr text alignment justification to use. Defaults to `"center"`. Must be one of
#'   `"center"`, `"right"`, `"left"`, `"dec_right"`, `"dec_left"`, or `"decimal"`.
#'
#' @return `x`, padded to be a string of length `n`.
#'
#' @examples
#' padstr("abc", 3)
#' padstr("abc", 4)
#' padstr("abc", 5)
#' padstr("abc", 5, "left")
#' padstr("abc", 5, "right")
#'
#' \dontrun{
#' # Expect error: "abc" has more than 1 characters
#' padstr("abc", 1)
#' }
#'
#' @export
padstr <- function(x, n, just = list_valid_aligns(), fontspec = font_spec()) {
  just <- match.arg(just)

  if (length(x) != 1) stop("length of x needs to be 1 and not", length(x))
  if (is.na(n) || !is.numeric(n) || n < 0) stop("n needs to be numeric and > 0")

  if (is.na(x)) x <- "<NA>"

  ## nc <- nchar(x)
  nc <- nchar_ttype(x, fontspec)
  if (n < nc) stop("\"", x, "\" has more than ", n, " characters")

  switch(just,
    center = {
      pad <- (n - nc) / 2
      paste0(spaces(floor(pad)), x, spaces(ceiling(pad)))
    },
    left = paste0(x, spaces(n - nc)),
    right = paste0(spaces(n - nc), x),
    decimal = {
      pad <- (n - nc) / 2
      paste0(spaces(floor(pad)), x, spaces(ceiling(pad)))
    },
    dec_left = paste0(x, spaces(n - nc)),
    dec_right = paste0(spaces(n - nc), x)
  )
}

spaces <- function(n) {
  strrep(" ", n)
}

.paste_no_na <- function(x, ...) {
  paste(na.omit(x), ...)
}

#' Spread an integer to a given length
#'
#' @param x (`integer(1)`)\cr number to spread.
#' @param len (`integer(1)`)\cr number of times to repeat `x`.
#'
#' @return If `x` is a scalar whole number value (see [is.wholenumber()]), the value `x` is repeated `len` times.
#'   Otherwise, an error is thrown.
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
#' @export
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

spread_width <- function(x, len) {
  rep(x / len, len)
}

#' Check if a value is a whole number
#'
#' @param x (`numeric(1)`)\cr a numeric value.
#' @param tol (`numeric(1)`)\cr a precision tolerance.
#'
#' @return `TRUE` if `x` is within `tol` of zero, `FALSE` otherwise.
#'
#' @examples
#' is.wholenumber(5)
#' is.wholenumber(5.00000000000000001)
#' is.wholenumber(.5)
#'
#' @export
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}
