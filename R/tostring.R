## this can't be tested from within R
# nocov start
#' @importFrom stats na.omit
#' @importFrom utils head tail localeToCharset
#' @import checkmate

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
#' displayed in the current `charset` for use in rendering table-likes.
#'
#' @return `unicode` 2014 (long dash for generating solid horizontal line)
#' if in a locale that uses a UTF character set, otherwise an ASCII hyphen
#' with a once-per-session warning.
#'
#' @export
#' @examples
#' default_hsep()
default_hsep <- d_hsep_factory()

# nocov end

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
do_cell_fnotes_wrap <- function(mat, widths, max_width, tf_wrap) {

    col_gap <- mf_colgap(mat)
    ncchar <- sum(widths) + (length(widths) - 1) * col_gap
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
    .check_indentation(mat)
    mod_ind_list <- .modify_indentation(mat, cell_widths_mat, do_what = "remove")
    mfs <- mod_ind_list[["mfs"]]
    cell_widths_mat <- mod_ind_list[["cell_widths_mat"]]

    # Main wrapper
    mf_strings(mat) <- matrix(
        unlist(mapply(wrap_string,
                      str = mfs,
                      width = cell_widths_mat,
                      collapse = "\n"
                      )),
        ncol = ncol(mfs)
    )

    ## XXXXX this is wrong and will break for listings cause we don't know when
    ## we need has_topleft to be FALSE!!!!!!!!!!
    mat <- mform_handle_newlines(mat)

    ## this updates extents in rinfo AND nlines in ref_fnotes_df
    mat <- update_mf_nlines(mat, max_width = max_width)

    # Re-indenting
    mf_strings(mat) <- .modify_indentation(mat, cell_widths_mat, do_what = "add")[["mfs"]]
    .check_indentation(mat) # all went well

    mat
}

# Helper function to see if body indentation matches (minimum)
# It sees if there is AT LEAST the indentation contained in rinfo
.check_indentation <- function(mat) {
  # mf_nrheader(mat) # not useful
  mf_nlh <- mf_nlheader(mat)
  mf_lbody <- mf_lgrouping(mat)
  mf_str <- mf_strings(mat)
  mf_ind <- c(rep(0, mf_nrheader(mat)), mf_rinfo(mat)$indent) # XXX to fix with topleft
  ind_std <- paste0(rep(" ", mat$indent_size), collapse = "")

  # Expected indent (-x negative numbers should not appear at this stage)
  stopifnot(all(mf_ind >= 0))
  real_indent <- sapply(mf_ind, function(ii) paste0(rep(ind_std, ii), collapse = ""))

  # Main detector
  correct_indentation <- vapply(seq_along(mf_lbody), function(xx) {
    grouping <- mf_lbody[xx]
    if (nzchar(real_indent[grouping])) {
      return(stringi::stri_detect(mf_str[xx, 1], regex = paste0("^", real_indent[grouping])))
    }
    # Cases where no indent are true by definition
    return(TRUE)
  }, logical(1))

  if (any(!correct_indentation)) {
    stop("We discovered indentation mismatches between the matrix_form and the indentation",
         " predefined in mf_rinfo. This should not happen. Contact the maintainer.") # nocov
  }
}
# Helper function that takes out or adds the proper indentation
.modify_indentation <- function(mat, cell_widths_mat, do_what = c("remove", "add")) {
  # Extract info
  mfs <- mf_strings(mat) # we work on mfs
  mf_nlh <- mf_nlheader(mat)
  mf_l <- mf_lgrouping(mat)
  mf_ind <- c(rep(0, mf_nrheader(mat)), mf_rinfo(mat)$indent) # XXX TO FIX in matrix form
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
    if(length(sdiff) > 0)
        stop("Invalid text-alignment(s): ",
             paste(sdiff, collapse = ", "))
    grepl("dec", vec)
}

any_dec_align <- function(vec) any(is_dec_align(vec))

#' Decimal Alignment
#'
#' @description Aligning decimal values of string matrix. Allowed alignments are: `dec_left`,
#'  `dec_right` and `decimal`.
#'
#' @param string_mat character matrix. String matrix component of matrix print form object.
#' @param align_mat character matrix. Aligns matrix component of matrix print form object.
#'  Should contain either `dec_left`, `dec_right` or `decimal` for values to be decimal aligned.
#'
#' @details Decimal alignment left and right (`dec_left` and `dec_right`) are different to
#'  center decimal alignment `decimal` only in the case some padding is present. This may
#'  happen if column widths are wider by setting parameters `widths` in `toString` or
#'  `colwidths` in `paginate_*` accordingly. It will be also the case (more common) of
#'  wider column names. Decimal alignment is not supported along with cell wrapping.
#'
#' @examples
#' dfmf <- basic_matrix_form(mtcars[1:5,])
#' aligns <- mf_aligns(dfmf)
#' aligns[, -c(1)] <- "dec_left"
#' decimal_align(mf_strings(dfmf), aligns)
#'
#' @return Processed string matrix of matrix print form with decimal aligned values.
#'
#' @seealso [toString] and [MatrixPrintForm]
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
                if(any(has_sc_not)) {
                  stop("Found values using scientific notation between the ones that",
                       " needs to be decimal aligned (aligns is decimal, dec_left or dec_right).",
                       " Please consider using format functions to get a complete decimal ",
                       "(e.g. formatC).")
                }

                ## Count the number of numbers in the string
                matches <- gregexpr("\\d+\\.\\d+|\\d+", col_ia)
                more_than_one <- vapply(matches, function(x) {
                    sum(attr(x, "match.length") > 0) > 1
                }, logical(1))
                ## Throw error in case any have more than 1 numbers
                if (any(more_than_one)) {
                    stop("Decimal alignment is not supported for multiple values. ",
                         "Found the following string with multiple numbers ",
                         "(first 3 selected from column ", col_i[1],"): '",
                         paste0(col_ia[more_than_one][seq(1, 3)],
                                collapse = "', '"), "'")
                }
                ## General split (only one match -> the first)
                main_regexp <- regexpr("\\d+", col_ia)
                left <- regmatches(col_ia, main_regexp, invert = FALSE)
                right <- regmatches(col_ia, main_regexp, invert = TRUE)
                right <- sapply(right, "[[", 2)
                something_left <- sapply(strsplit(col_ia, "\\d+"), "[[", 1)
                left <- paste0(something_left, left)
                if (!checkmate::test_set_equal(paste0(left, right), col_ia))
                    stop("Split string list lost some piece along the way. This ",
                         "should not have happened. Please contact the maintainer.") # nocov
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
                if(!any(grepl("[^[:space:]]", separator)))
                    separator <- gsub("[[:space:]]*", "", separator)
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

# toString ---------------------------------------------------------------------
# main printing code for MatrixPrintForm
#

#' @title Main printing system: `toString`
#'
#' @description
#' All objects that are printed to console pass by `toString`. This function allows
#' fundamental formatting specifications for the final output, like column widths and
#' relative wrapping (`width`), title and footer wrapping (`tf_wrap = TRUE` and
#' `max_width`), or horizontal separator character (e.g. `hsep = "+"`).
#'
#' @inheritParams MatrixPrintForm
#' @param widths numeric (or  NULL). (proposed) widths for the columns
#'     of \code{x}. The expected length  of this numeric vector can be
#'     retrieved with  `ncol() + 1`  as the  column of row  names must
#'     also be considered.
#' @param hsep character(1). Characters to repeat to create
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
#' @seealso [wrap_string()]
#'
#' @examples
#' mform <- basic_matrix_form(mtcars)
#' cat(toString(mform))
#'
#' @return A character string containing the ASCII rendering
#' of the table-like object represented by `x`
#'
#' @rdname tostring
#' @exportMethod toString
setMethod("toString", "MatrixPrintForm", function(x,
                                                  widths = NULL,
                                                  tf_wrap = FALSE,
                                                  max_width = NULL,
                                                  col_gap = mf_colgap(x),
                                                  hsep = default_hsep()) {
    checkmate::assert_flag(tf_wrap)

    mat <- matrix_form(x, indent_rownames = TRUE)

    # Check for \n in mat strings -> if there are any, matrix_form did not work
    if (any(grepl("\n", mf_strings(mat)))) {
      stop("Found newline characters (\n) in string matrix produced by matrix_form.",
           "This is not supported and implies missbehavior on the first parsing (in matrix_form).",
           "Please contact the maintainer or file an issue.") # nocov
    }

    inset <- table_inset(mat)

    # if cells are decimal aligned, run propose column widths
    # if the provided widths is less than proposed width, return an error
    if (any_dec_align(mf_aligns(mat))) {
      aligned <- propose_column_widths(x)

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
        widths <- mf_col_widths(x) %||% propose_column_widths(x)
    } else {
        mf_col_widths(x) <- widths
    }

    # Total number of characters for the table
    ncchar <- sum(widths) + (length(widths) - 1) * col_gap

    ## Text wrapping checks (widths)
    if (tf_wrap) {
        if (is.null(max_width)) {
            max_width <- getOption("width", 80L)
        } else if (is.character(max_width) && identical(max_width, "auto")) {
            max_width <- ncchar + inset
        }
        assert_number(max_width, lower = 0)
    }

    # Main wrapper function for table core
    mat <- do_cell_fnotes_wrap(mat, widths, max_width = max_width, tf_wrap = tf_wrap)

    body <- mf_strings(mat)
    aligns <- mf_aligns(mat)
    keep_mat <- mf_display(mat)
    ## spans <- mat$spans
    ##    ri <- mat$row_info
    ref_fnotes <- mf_rfnotes(mat)
    nl_header <- mf_nlheader(mat)

    cell_widths_mat <- .calc_cell_widths(mat, widths, col_gap)

    # decimal alignment
    if (any_dec_align(aligns)) {
      body <- decimal_align(body, aligns)
    }

    # Content is a matrix of cells with the right amount of spaces
    content <- matrix(mapply(padstr, body, cell_widths_mat, aligns), ncol = ncol(body))
    content[!keep_mat] <- NA

    # Define gap string and divisor string
    gap_str <- strrep(" ", col_gap)
    div <- substr(strrep(hsep, ncchar), 1, ncchar)

    # text head (paste w/o NA content header and gap string)
    txt_head <- apply(head(content, nl_header), 1, .paste_no_na, collapse = gap_str)

    # txt body
    sec_seps_df <- x$row_info[, c("abs_rownumber", "trailing_sep"), drop = FALSE]
    if (!is.null(sec_seps_df) && any(!is.na(sec_seps_df$trailing_sep))) {
        bdy_cont <- tail(content, -nl_header)
        ## unfortunately we count "header rows" wrt line grouping so it
        ## doesn't match the real (i.e. body) rows as is
        row_grouping <- tail(x$line_grouping, -nl_header) - mf_nrheader(x)
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
                    substr(
                        strrep(sec_seps_df$trailing_sep[i], ncchar), 1,
                        ncchar - inset
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
        allts <- wrap_txt(allts, max_width)
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

    # Final return
    paste0(
      paste(
        c(
            titles_txt, # .do_inset(div, inset) happens if there are any titles
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


#' Wrap a string to within a precise width
#'
#' @description
#' Core wrapping functionality that preserve white spaces. Only `"\n"` is not supported
#' by core functionality [stringi::stri_wrap()]. This is usually solved before hand by
#' [matrix_form()]. If the width is smaller than any large word, these will be truncated
#' after `width` characters. If the split leaves trailing groups of empty spaces,
#' they will be dropped.
#'
#' @param str character. String to be wrapped. If it is a character vector or
#'   a list, it will be looped as a list and returned with `unlist(use.names = FALSE)`.
#' @param width numeric(1). Width, in characters, that the
#'   text should be wrapped at.
#' @param collapse character(1) or `NULL`. If the words that have been split should
#'   be pasted together with the collapse character. This is usually done internally
#'   with `"\n"` to have the wrapping updated along with other internal values.
#' @param smart logical(1). Defaults to `TRUE`. It attempts to calculate the optimal
#'   word split if there are some words that exceed inserted `width`. It does so by
#'   considering the preceding word (if present) and adding a piece of the word to split
#'   if there is space for it. This option uses a recurrent for loop, hence it may be
#'   expansive for large texts with a relatively small width.
#'
#' @details Word wrapping happens as with [stringi::stri_wrap()]
#'   with the following exception: individual words which are longer
#'   than `max_width` are broken up in a way that fits with the rest of the
#'   word wrapping.
#'
#' @return A string if `str` is one element and if `collapse = NULL`. Otherwise, is
#'   a list of elements (if `length(str) > 1`) that can contain strings or vector of
#'   characters (if `collapse = NULL`).
#'
#' @examples
#' str <- list("  , something really  \\tnot  very good", # \t needs to be escaped
#'             "  but I keep it12   ")
#' wrap_string(str, 5, collapse = "\n")
#'
#' @name wrap_string
#' @export
wrap_string <- function(str, width, collapse = NULL, smart = TRUE) {
  if (length(str) > 1) {
    return(
      unlist(
        lapply(str, wrap_string, width = width, collapse = collapse, smart = smart),
        use.names = FALSE
      )
    )
  }
  str <- unlist(str, use.names = FALSE) # it happens is one list element
  if (length(str) == 0 || is.null(str) || !nzchar(str) || is.na(str)) {
    return(str)
  }
  checkmate::assert_character(str)
  checkmate::assert_int(width, lower = 1)
  checkmate::assert_flag(smart)

  if (any(grepl("\\n", str))) {
    stop("Found \\n in a string that was meant to be wrapped. This should not happen ",
         "because matrix_form should take care of them before this step (toString, ",
         "i.e. the printing machinery). Please contact the maintaner or file an issue.")
  }

  # str can be also a vector or list. In this case simplify manages the output
  ret <- stringi::stri_wrap(str,
    width = width,
    normalize = FALSE, # keeps spaces
    simplify = TRUE, # If FALSE makes it a list with str elements
    indent = 0
  )
  # Check if it went fine
  if (any(nchar(ret) > width)) {
    which_exceeded <- which(nchar(ret) > width)

    if (smart) {
      # Recursive for loop to take word interval
      for (we_i in which_exceeded) {
        # Is there space for some part of the next word?
        char_threshold <- width *(2 / 3) + 0.01 # if too little space -> no previous word
        smart_condition <- nchar(ret[we_i - 1]) + 1 < char_threshold # +1 is for spaces
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
        # Paste together and rerun
        ret <- paste0(unique(ret), collapse = " ")
        return(wrap_string(str = ret, width = width, collapse = collapse, smart = smart))
      }
    } else {
      values_that_exceeded <- ret[which_exceeded]
      # Split the words, paste, and rerun
      ret[which_exceeded] <- split_words_by(values_that_exceeded, width)
      ret <- paste0(ret, collapse = " ")
      return(wrap_string(str = ret, width = width, collapse = collapse, smart = smart))
    }
  }

  if (!is.null(collapse)) {
    return(paste0(ret, collapse = collapse))
  }

  return(ret)
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

#' @describeIn wrap_string function that flattens the list of wrapped strings with
#'   `unist(str, use.names = FALSE)`. This is deprecated, use [wrap_string()] instead.
#' @examples
#' wrap_txt(str, 5, collapse = NULL)
#'
#' @export
wrap_txt <- function(str, width, collapse = NULL) {
  unlist(wrap_string(str, width, collapse), use.names = FALSE)
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


#' Transform vectors of spans (with duplication) to Visibility vector
#'
#' @param spans numeric. A vector of spans, with each span value repeated
#' for the cells it covers.
#'
#' @details
#'
#' The values of \code{spans} are assumed to be repeated to such that
#' each individual position covered by the span has the repeated value.
#'
#' This means that each block of values in \code{span} must be of a length
#' at least equal to its value (i.e. two 2s, three 3s, etc).
#'
#' This function correctly handles cases where two spans of the same size
#' are next to each other; i.e., a block of four 2s represents two large
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
#' @param x `MatrixPrintForm` object, or an object with a `matrix_form`
#' method.
#' @param indent_size numeric(1). Indent size in characters. Ignored
#' when `x` is already a `MatrixPrintForm` object in favor of information
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
  body <- mf_strings(x)
  spans <- mf_spans(x)
  aligns <- mf_aligns(x)
  display <- mf_display(x)

  # compute decimal alignment if asked in alignment matrix
  if (any_dec_align(aligns)) {
    body <- decimal_align(body, aligns)
  }

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
#'     use. Defaults to `center`. Must be `center`, `right`, `left`,
#'     `dec_right`, `dec_left` or `decimal`.
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
padstr <- function(x, n, just = list_valid_aligns()) {
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


#' spread `x` into `len` elements
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



#' `is.wholenumber`
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
