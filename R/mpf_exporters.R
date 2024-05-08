.need_pag <- function(page_type, pg_width, pg_height, cpp, lpp) {
  !(is.null(page_type) && is.null(pg_width) && is.null(pg_height) && is.null(cpp) && is.null(lpp))
}

#' Export a table-like object to plain (ASCII) text with page breaks
#'
#' This function converts `x` to a `MatrixPrintForm` object via [matrix_form()], paginates it
#' via [paginate_to_mpfs()], converts each page to ASCII text via [toString()], and outputs
#' the strings, separated by `page_break`, to `file`.
#'
#' @inheritParams paginate_indices
#' @inheritParams toString
#' @inheritParams propose_column_widths
#' @param x (`ANY`)\cr a table-like object to export. Must have an applicable `matrix_form` method.
#' @param file (`string` or `NULL`)\cr if non-`NULL`, the path to write a text file to
#'   containing `x` rendered as ASCII text.
#' @param page_break (`string`)\cr page break symbol (defaults to `"\\n\\s"`).
#' @param paginate (`flag`)\cr whether pagination should be performed. Defaults to `TRUE`
#'   if page size is specified (including the default).
#' @param ... additional parameters passed to [paginate_to_mpfs()].
#'
#' @details
#' If `x` has a `num_rep_cols` method, the value returned by it will be used for `rep_cols` by
#' default. Otherwise, 0 will be used.
#'
#' If `x` has an applicable `do_forced_paginate` method, it will be invoked during the
#' pagination process.
#'
#' @return If `file` is `NULL`, the full paginated and concatenated string value is returned,
#'   otherwise the output is written to `file` and no value (invisible `NULL`) is returned.
#'
#' @examples
#' export_as_txt(basic_matrix_form(mtcars), pg_height = 5, pg_width = 4)
#'
#' @export
export_as_txt <- function(x,
                          file = NULL,
                          page_type = NULL,
                          landscape = FALSE,
                          pg_width = page_dim(page_type)[if (landscape) 2 else 1],
                          pg_height = page_dim(page_type)[if (landscape) 1 else 2],
                          font_family = "Courier",
                          font_size = 8, # grid parameters
                          lineheight = 1L,
                          margins = c(top = .5, bottom = .5, left = .75, right = .75),
                          paginate = TRUE,
                          cpp = NA_integer_,
                          lpp = NA_integer_,
                          ...,
                          hsep = NULL,
                          indent_size = 2,
                          tf_wrap = paginate,
                          max_width = NULL,
                          colwidths = NULL,
                          min_siblings = 2,
                          nosplitin = character(),
                          rep_cols = NULL,
                          verbose = FALSE,
                          page_break = "\\s\\n",
                          page_num = default_page_number()) {
  # Processing lists of tables or listings
  if (.is_list_of_tables_or_listings(x)) {
    if (isFALSE(paginate)) {
      warning(
        "paginate is FALSE, but x is a list of tables or listings, ",
        "so paginate will automatically be updated to TRUE"
      )
    }
    paginate <- TRUE
  }

  if (paginate) {
    pages <- paginate_to_mpfs(
      x,
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
      min_siblings = min_siblings,
      nosplitin = nosplitin,
      colwidths = colwidths,
      tf_wrap = tf_wrap,
      max_width = max_width,
      indent_size = indent_size,
      verbose = verbose,
      rep_cols = rep_cols,
      page_num = page_num
    )
  } else {
    mf <- matrix_form(x, TRUE, TRUE, indent_size = indent_size)
    mf_col_widths(mf) <- colwidths %||% propose_column_widths(mf)
    pages <- list(mf)
  }

  # Needs to be here because of adding cpp if it is not "auto"
  if (!is.character(max_width)) {
    max_width <- .handle_max_width(
      tf_wrap = tf_wrap,
      max_width = max_width,
      cpp = cpp
    )
  }

  ## we don't set widths here because we already put that info in mpf
  ## so its on each of the pages.
  strings <- vapply(
    pages, toString, "",
    widths = NULL,
    hsep = hsep, tf_wrap = tf_wrap, max_width = max_width
  )

  res <- paste(strings, collapse = page_break)

  if (is.null(file)) {
    res
  } else {
    cat(res, file = file)
  }
}

.is_list_of_tables_or_listings <- function(a_list) {
  if (is(a_list, "list")) {
    all_matrix_forms <- FALSE
    obj_are_tables_or_listings <- FALSE

    if (is(a_list[[1]], "MatrixPrintForm")) {
      all_matrix_forms <- all(sapply(a_list, is, class2 = "MatrixPrintForm"))
    } else {
      obj_are_tables_or_listings <- all(
        sapply(a_list, function(list_i) {
          is(list_i, "listing_df") || is(list_i, "VTableTree")
        })
      )
    }
    out <- obj_are_tables_or_listings || all_matrix_forms
  } else {
    out <- FALSE
  }

  out
}

# RTF support ------------------------------------------------------------------

## In use, must be tested
prep_header_line <- function(mf, i) {
  ret <- mf$strings[i, mf$display[i, , drop = TRUE], drop = TRUE]
  ret
}

## margin_lines_to_in <- function(margins, font_size, font_family) {
##   tmpfile <- tempfile(fileext = ".pdf")
##   gp_plot <- gpar(fontsize = font_size, fontfamily = font_family)
##   pdf(file = tmpfile, width = 20, height = 20)
##   on.exit({
##     dev.off()
##     file.remove(tmpfile)
##   })
##   grid.newpage()
##   pushViewport(plotViewport(margins = margins, gp = gp_plot))
##   c(
##     bottom = convertHeight(unit(margins["bottom"], "lines"), "inches", valueOnly = TRUE),
##     left = convertWidth(unit(1, "strwidth", strrep("m", margins["left"])), "inches", valueOnly = TRUE),
##     top = convertHeight(unit(margins["top"], "lines"), "inches", valueOnly = TRUE),
##     right = convertWidth(unit(1, "strwidth", strrep("m", margins["right"])), "inches", valueOnly = TRUE)
##   )
## }

mpf_to_dfbody <- function(mpf, colwidths) {
  mf <- matrix_form(mpf, indent_rownames = TRUE)
  nlr <- mf_nlheader(mf)
  if (is.null(colwidths)) {
    colwidths <- propose_column_widths(mf)
  }
  mf$strings[1:nlr, 1] <- ifelse(nzchar(mf$strings[1:nlr, 1, drop = TRUE]),
    mf$strings[1:nlr, 1, drop = TRUE],
    strrep(" ", colwidths)
  )


  myfakedf <- as.data.frame(tail(mf$strings, -nlr))
  myfakedf
}

#' Transform `MatrixPrintForm` to RTF
#'
#' Experimental export to rich text format (RTF) via the `r2rtf` package.
#'
#' @inheritParams page_lcpp
#' @inheritParams toString
#' @inheritParams grid::plotViewport
#' @param mpf (`MatrixPrintForm`)\cr a `MatrixPrintForm` object.
#' @param colwidths (`numeric`)\cr column widths.
#'
#' @details
#' This function provides a low-level coercion of a `MatrixPrintForm` object into
#' text containing the corresponding table in RTF. Currently, no pagination is done
#' at this level, and should be done prior to calling this function, though that
#' may change in the future.
#'
#' @return An RTF object.
#'
#' @export
mpf_to_rtf <- function(mpf,
                       colwidths = NULL,
                       page_type = "letter",
                       pg_width = page_dim(page_type)[if (landscape) 2 else 1],
                       pg_height = page_dim(page_type)[if (landscape) 1 else 2],
                       landscape = FALSE,
                       margins = c(4, 4, 4, 4),
                       font_size = 8,
                       ...) {
  if (!requireNamespace("r2rtf")) {
    stop("RTF export requires the 'r2rtf' package, please install it.")
  }
  mpf <- matrix_form(mpf, indent_rownames = TRUE)
  nlr <- mf_nlheader(mpf)
  if (is.null(colwidths)) {
    colwidths <- propose_column_widths(mpf)
  }
  mpf$strings[1:nlr, 1] <- ifelse(nzchar(mpf$strings[1:nlr, 1, drop = TRUE]),
    mpf$strings[1:nlr, 1, drop = TRUE],
    strrep(" ", colwidths)
  )

  myfakedf <- mpf_to_dfbody(mpf, colwidths)

  rtfpg <- r2rtf::rtf_page(myfakedf,
    width = pg_width,
    height = pg_height,
    orientation = if (landscape) "landscape" else "portrait",
    margin = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
    nrow = 10000L
  ) ## dont allow r2rtf to restrict lines per page beyond actual real eastate
  rtfpg <- r2rtf::rtf_title(rtfpg, main_title(mpf), subtitles(mpf), text_font = 1)
  for (i in seq_len(nlr)) {
    hdrlndat <- prep_header_line(mpf, i)
    rtfpg <- r2rtf::rtf_colheader(rtfpg,
      paste(hdrlndat, collapse = " | "),
      col_rel_width = unlist(tapply(colwidths,
        cumsum(mpf$display[i, , drop = TRUE]),
        sum,
        simplify = FALSE
      )),
      border_top = c("", rep(if (i > 1) "single" else "", length(hdrlndat) - 1)),
      text_font = 9, ## this means Courier New for some insane reason
      text_font_size = font_size
    )
  }

  rtfpg <- r2rtf::rtf_body(rtfpg,
    col_rel_width = colwidths,
    text_justification = c("l", rep("c", ncol(myfakedf) - 1)),
    text_format = "",
    text_font = 9,
    text_font_size = font_size
  )

  for (i in seq_along(mpf$ref_footnotes)) {
    rtfpg <- r2rtf::rtf_footnote(rtfpg,
      mpf$ref_footnotes[i],
      border_top = if (i == 1) "single" else "",
      border_bottom = if (i == length(mpf$ref_footnotes)) "single" else "",
      text_font = 9
    )
  }

  if (length(main_footer(mpf)) > 0) {
    rtfpg <- r2rtf::rtf_footnote(rtfpg, main_footer(mpf), text_font = 9)
  }
  if (length(prov_footer(mpf)) > 0) {
    rtfpg <- r2rtf::rtf_source(rtfpg, prov_footer(mpf), text_font = 9)
  }

  rtfpg
}

## Not currently in use, previous alternate ways to get to RTF

## ## XXX Experimental. Not to be exported without approval
## mpf_to_huxtable <- function(obj) {
##   if (!requireNamespace("huxtable")) {
##     stop("mpf_to_huxtable requires the huxtable package")
##   }
##   mf <- matrix_form(obj, indent_rownames = TRUE)
##   nlr <- mf_nlheader(mf)
##   myfakedf <- as.data.frame(tail(mf$strings, -nlr))
##   ret <- huxtable::as_hux(myfakedf, add_colnames = FALSE)
##   mf$strings[!mf$display] <- ""
##   for (i in seq_len(nlr)) {
##     arglist <- c(
##       list(ht = ret, after = i - 1),
##       as.list(mf$strings[i, ])
##     )
##     ret <- do.call(huxtable::insert_row, arglist)

##     spanspl <- split(
##       seq_len(ncol(mf$strings)),
##       cumsum(mf$display[i, ])
##     )

##     for (j in seq_along(spanspl)) {
##       if (length(spanspl[[j]]) > 1) {
##         ret <- huxtable::merge_cells(ret, row = i, col = spanspl[[j]])
##       }
##     }
##   }
##   ret <- huxtable::set_header_rows(ret, seq_len(nlr), TRUE)
##   huxtable::font(ret) <- "courier"
##   huxtable::font_size(ret) <- 6
##   huxtable::align(ret)[
##     seq_len(nrow(ret)),
##     seq_len(ncol(ret))
##   ] <- mf$aligns
##   ret
## }

## ## XXX Experimental. Not to be exported without approval
## mpf_to_rtf <- function(obj, ..., file) {
##   huxt <- mpf_to_huxtable(obj)
##   ## a bunch more stuff here
##   huxtable::quick_rtf(huxt, ..., file = file)
## }

## ## XXX Experimental. Not to be exported without approval
## mpf_to_gt <- function(obj) {
##   requireNamespace("gt")
##   mf <- matrix_form(obj, indent_rownames = TRUE)
##   nlh <- mf_nlheader(mf)
##   body_df <- as.data.frame(mf$strings[-1 * seq_len(nlh), ])
##   varnamerow <- mf_nrheader(mf)
##   ## detect if we have counts
##   if (any(nzchar(mf$formats[seq_len(nlh), ]))) {
##     varnamerow <- varnamerow - 1
##   }

##   rlbl_lst <- as.list(mf$strings[nlh, , drop = TRUE])
##   names(rlbl_lst) <- names(body_df)

##   ret <- gt::gt(body_df, rowname_col = "V1")
##   ret <- gt::cols_label(ret, .list = rlbl_lst)
##   if (nlh > 1) {
##     for (i in 1:(nlh - 1)) {
##       linedat <- mf$strings[i, , drop = TRUE]
##       splvec <- cumsum(mf$display[i, , drop = TRUE])
##       spl <- split(seq_along(linedat), splvec)
##       for (j in seq_along(spl)) {
##         vns <- names(body_df)[spl[[j]]]
##         labval <- linedat[spl[[j]][1]]
##         ret <- gt::tab_spanner(ret,
##           label = labval,
##           columns = {{ vns }},
##           level = nlh - i,
##           id = paste0(labval, j)
##         )
##       }
##     }
##   }

##   ret <- gt::opt_css(ret, css = "th.gt_left { white-space:pre;}")

##   ret
## }

#' Export as RTF
#'
#' Experimental export to the rich text format (RTF) format.
#'
#' @details RTF export occurs via the following steps:
#'   * The table is paginated to the specified page size (vertically and horizontally).
#'   * Each separate page is converted to a `MatrixPrintForm` object and then to
#'     RTF-encoded text.
#'   * Separate RTF text chunks are combined and written to a single RTF file.
#'
#'   Conversion of `MatrixPrintForm` objects to RTF is done via [mpf_to_rtf()].
#'
#' @inheritParams export_as_txt
#' @inheritParams toString
#' @inheritParams grid::plotViewport
#' @inheritParams paginate_to_mpfs
#'
#' @export
export_as_rtf <- function(x,
                          file = NULL,
                          colwidths = NULL,
                          page_type = "letter",
                          pg_width = page_dim(page_type)[if (landscape) 2 else 1],
                          pg_height = page_dim(page_type)[if (landscape) 1 else 2],
                          landscape = FALSE,
                          margins = c(bottom = .5, left = .75, top = .5, right = .75),
                          font_size = 8,
                          font_family = "Courier",
                          ...) {
  # Processing lists of tables or listings
  if (.is_list_of_tables_or_listings(x)) {
    if (isFALSE(paginate)) {
      warning(
        "paginate is FALSE, but x is a list of tables or listings, ",
        "so paginate will automatically be updated to TRUE"
      )
    }
    paginate <- TRUE
  }

  if (!requireNamespace("r2rtf")) {
    stop("RTF export requires the r2rtf package, please install it.")
  }
  if (is.null(names(margins))) {
    names(margins) <- marg_order
  }

  true_width <- pg_width - sum(margins[c("left", "right")])
  true_height <- pg_height - sum(margins[c("top", "bottom")])

  mpfs <- paginate_to_mpfs(
    x,
    font_family = font_family, font_size = font_size,
    pg_width = true_width,
    pg_height = true_height,
    margins = c(bottom = 0, left = 0, top = 0, right = 0),
    lineheight = 1.25,
    colwidths = colwidths,
    ...
  )

  rtftxts <- lapply(mpfs, function(mf) {
    r2rtf::rtf_encode(mpf_to_rtf(mf,
      colwidths = mf_col_widths(mf),
      page_type = page_type,
      pg_width = pg_width,
      pg_height = pg_height,
      font_size = font_size,
      margins = c(top = 0, left = 0, bottom = 0, right = 0)
    ))
  })
  restxt <- paste(
    rtftxts[[1]]$start,
    paste(
      sapply(rtftxts, function(x) x$body),
      collapse = "\n{\\pard\\fs2\\par}\\page{\\pard\\fs2\\par}\n"
    ),
    rtftxts[[1]]$end
  )
  if (!is.null(file)) {
    cat(restxt, file = file)
  } else {
    restxt
  }
}


# PDF support ------------------------------------------------------------------

#' Export as PDF
#'
#' The PDF output from this function is based on the ASCII output created with [toString()].
#'
#' @inheritParams export_as_txt
#' @param file (`string`)\cr file to write to, must have `.pdf` extension.
#' @param width `r lifecycle::badge("deprecated")` Please use the `pg_width` argument or specify
#'   `page_type` instead.
#' @param height `r lifecycle::badge("deprecated")` Please use the `pg_height` argument or
#'   specify `page_type` instead.
#' @param fontsize `r lifecycle::badge("deprecated")` Please use the `font_size` argument instead.
#' @param margins (`numeric(4)`)\cr the number of lines/characters of the margin on the bottom,
#'   left, top, and right sides of the page, respectively.
#'
#' @importFrom grDevices pdf
#' @importFrom grid textGrob grid.newpage gpar pushViewport plotViewport unit grid.draw
#'   convertWidth convertHeight grobHeight grobWidth
#' @importFrom grid textGrob get.gpar
#' @importFrom grDevices dev.off
#' @importFrom tools file_ext
#'
#' @details
#' By default, pagination is performed with default `cpp` and `lpp` defined by specified page
#' dimensions and margins. User-specified `lpp` and `cpp` values override this, and should
#' be used with caution.
#'
#' Title and footer materials are also word-wrapped by default (unlike when printed to the
#' terminal), with `cpp` (as defined above) as the default `max_width`.
#'
#' @seealso [export_as_txt()]
#'
#' @examples
#' \dontrun{
#' tf <- tempfile(fileext = ".pdf")
#' export_as_pdf(basic_matrix_form(mtcars), file = tf, pg_height = 4)
#'
#' tf <- tempfile(fileext = ".pdf")
#' export_as_pdf(basic_matrix_form(mtcars), file = tf, lpp = 8)
#' }
#'
#' @export
export_as_pdf <- function(x,
                          file,
                          page_type = "letter",
                          landscape = FALSE,
                          pg_width = page_dim(page_type)[if (landscape) 2 else 1],
                          pg_height = page_dim(page_type)[if (landscape) 1 else 2],
                          width = lifecycle::deprecated(),
                          height = lifecycle::deprecated(),
                          margins = c(4, 4, 4, 4),
                          min_siblings = 2,
                          font_family = "Courier",
                          font_size = 8,
                          fontsize = lifecycle::deprecated(),
                          paginate = TRUE,
                          page_num = default_page_number(),
                          lpp = NULL,
                          cpp = NULL,
                          hsep = "-",
                          indent_size = 2,
                          rep_cols = NULL,
                          tf_wrap = TRUE,
                          max_width = NULL,
                          colwidths = NULL) {
  stopifnot(tools::file_ext(file) != ".pdf")

  # Processing lists of tables or listings
  if (.is_list_of_tables_or_listings(x)) {
    if (isFALSE(paginate)) {
      warning(
        "paginate is FALSE, but x is a list of tables or listings, ",
        "so paginate will automatically be updated to TRUE"
      )
    }
    paginate <- TRUE
  }

  if (lifecycle::is_present(width)) {
    lifecycle::deprecate_warn("0.5.5", "export_as_pdf(width)", "export_as_pdf(pg_width)")
    pg_width <- width
  }
  if (lifecycle::is_present(height)) {
    lifecycle::deprecate_warn("0.5.5", "export_as_pdf(height)", "export_as_pdf(pg_height)")
    pg_height <- height
  }
  if (lifecycle::is_present(fontsize)) {
    lifecycle::deprecate_warn("0.5.5", "export_as_pdf(fontsize)", "export_as_pdf(font_size)")
    font_size <- fontsize
  }

  gp_plot <- grid::gpar(fontsize = font_size, fontfamily = font_family)

  pdf(file = file, width = pg_width, height = pg_height)
  on.exit(dev.off())
  grid::grid.newpage()
  grid::pushViewport(grid::plotViewport(margins = margins, gp = gp_plot))

  cur_gpar <- grid::get.gpar()
  if (is.null(lpp)) {
    lpp <- floor(grid::convertHeight(grid::unit(1, "npc"), "lines", valueOnly = TRUE) /
      (cur_gpar$cex * cur_gpar$lineheight)) - sum(margins[c(1, 3)]) # bottom, top # nolint
  }
  if (is.null(cpp)) {
    cpp <- floor(grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE) *
      font_lcpi(font_family, font_size, cur_gpar$lineheight)$cpi) - sum(margins[c(2, 4)]) # left, right # nolint
  }
  if (tf_wrap && is.null(max_width)) {
    max_width <- cpp
  }

  if (paginate) {
    tbls <- paginate_to_mpfs(
      x,
      page_type = page_type,
      font_family = font_family,
      font_size = font_size,
      lineheight = cur_gpar$lineheight,
      landscape = landscape,
      pg_width = pg_width,
      pg_height = pg_height,
      margins = margins,
      lpp = lpp,
      cpp = cpp,
      min_siblings = min_siblings,
      nosplitin = character(),
      colwidths = colwidths,
      tf_wrap = tf_wrap,
      max_width = max_width,
      indent_size = indent_size,
      verbose = FALSE,
      rep_cols = rep_cols,
      page_num = page_num
    )
  } else {
    mf <- matrix_form(x, TRUE, TRUE, indent_size = indent_size)
    mf_col_widths(mf) <- colwidths %||% propose_column_widths(mf)
    tbls <- list(mf)
  }

  # Needs to be here because of adding cpp if it is not "auto"
  if (!is.character(max_width)) {
    max_width <- .handle_max_width(
      tf_wrap = tf_wrap,
      max_width = max_width,
      cpp = cpp
    )
  }

  gtbls <- lapply(tbls, function(txt) {
    grid::textGrob(
      label = toString(txt,
        widths = txt$col_widths + 1, hsep = hsep,
        tf_wrap = tf_wrap, max_width = max_width
      ),
      x = grid::unit(0, "npc"), y = grid::unit(1, "npc"),
      just = c("left", "top")
    )
  })

  npages <- length(gtbls)
  exceeds_width <- rep(FALSE, npages)
  exceeds_height <- rep(FALSE, npages)

  for (i in seq_along(gtbls)) {
    g <- gtbls[[i]]

    if (i > 1) {
      grid::grid.newpage()
      grid::pushViewport(grid::plotViewport(margins = margins, gp = gp_plot))
    }

    if (grid::convertHeight(grid::grobHeight(g), "inches", valueOnly = TRUE) >
      grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)) { # nolint
      exceeds_height[i] <- TRUE
      warning("height of page ", i, " exceeds the available space")
    }
    if (grid::convertWidth(grid::grobWidth(g), "inches", valueOnly = TRUE) >
      grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)) { # nolint
      exceeds_width[i] <- TRUE
      warning("width of page ", i, " exceeds the available space")
    }

    grid::grid.draw(g)
  }
  list(
    file = file, npages = npages, exceeds_width = exceeds_width, exceeds_height = exceeds_height,
    lpp = lpp, cpp = cpp
  )
}
