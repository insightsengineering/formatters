.need_pag <- function(page_type, pg_width, pg_height, cpp, lpp) {
  !(is.null(page_type) && is.null(pg_width) && is.null(pg_height) && is.null(cpp) && is.null(lpp))
}

#' Export as plain text with page break symbol
#'
#' @inheritParams rtables::gen_args
#' @inheritParams rtables::tostring
#' @inheritParams rtables::paginate_table
#' @param file character(1). File to write.
#' @param paginate logical(1). Should \code{tt} be paginated before writing the file. Defaults to `TRUE` if any sort of page dimension is specified.
#' @param \dots Passed directly to \code{\link[rtables]{paginate_table}}
#' @param page_break character(1). Page break symbol (defaults to outputting \code{"\\s"}).
#' @return \code{file} (this function is called for the side effect of writing the file.
#'
#' @note When specified, `font_size` is used *only* to determine pagination based
#' on page dimensions. The written file is populated in raw ASCII text, which
#' does not have the concept of font size.
#'
#' @seealso [export_as_pdf()]
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(rtables)
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2", "COUNTRY"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#'
#' cat(export_as_txt(tbl, file = NULL, paginate = TRUE, lpp = 8))
#'
#' \dontrun{
#' tf <- tempfile(fileext = ".txt")
#' export_as_txt(tbl, file = tf)
#' system2("cat", tf)
#' }
export_as_txt <- function(tt, file = NULL,
                          page_type = NULL,
                          landscape = FALSE,
                          pg_width = page_dim(page_type)[if(landscape) 2 else 1],
                          pg_height = page_dim(page_type)[if(landscape) 1 else 2],
                          font_family = "Courier",
                          font_size = 8,  # grid parameters
                          paginate = .need_pag(page_type, pg_width, pg_height, lpp, cpp),
                          cpp = NULL,
                          lpp = NULL,
                          ..., page_break = "\\s\\n",
                          hsep = default_hsep(),
                          indent_size = 2,
                          tf_wrap = paginate,
                          max_width = cpp,
                          colwidths = propose_column_widths(matrix_form(tt, TRUE))) {
  if(!is.null(colwidths) && length(colwidths) != ncol(tt) + 1)
    stop("non-null colwidths argument must have length ncol(tt) + 1 [",
         ncol(tt) + 1, "], got length ", length(colwidths))
  if(paginate) {
    gp_plot <- gpar(fontsize = font_size, fontfamily = font_family)

    pdf(file = tempfile(), width = pg_width, height = pg_height)
    on.exit(dev.off())
    grid.newpage()
    pushViewport(plotViewport(margins = c(0, 0, 0, 0), gp = gp_plot))

    cur_gpar <-  get.gpar()
    if(is.null(page_type) && is.null(pg_width) && is.null(pg_height) &&
       (is.null(cpp) || is.null(lpp))) {
      page_type <- "letter"
      pg_width <- page_dim(page_type)[if(landscape) 2 else 1]
      pg_height <- page_dim(page_type)[if(landscape) 1 else 2]
    }

    if (is.null(lpp)) {
      lpp <- floor(convertHeight(unit(1, "npc"), "lines", valueOnly = TRUE) /
                     (cur_gpar$cex * cur_gpar$lineheight))
    }
    if(is.null(cpp)) {
      cpp <- floor(convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE) *
                     font_lcpi(font_family, font_size, cur_gpar$lineheight)$cpi)
    }
    if(tf_wrap && is.null(max_width))
      max_width <- cpp

    tbls <- rtables::paginate_table(tt, cpp = cpp, lpp = lpp, tf_wrap = tf_wrap, max_width = max_width,
                           colwidths = colwidths, ...)
  } else {
    tbls <- list(tt)
  }

  res <- paste(mapply(function(tb, cwidths, ...) {
    ## 1 and +1 are because cwidths includes rowlabel 'column'
    cinds <- c(1, .figure_out_colinds(tb, tt) + 1L)
    toString(tb, widths = cwidths[cinds], ...)
  },
  MoreArgs = list(hsep = hsep,
                  indent_size = indent_size,
                  tf_wrap = tf_wrap,
                  max_width = max_width,
                  cwidths = colwidths),
  SIMPLIFY = FALSE,
  tb = tbls),
  collapse = page_break)

  if(!is.null(file))
    cat(res, file = file)
  else
    res
}

pos_to_path <- function(pos) {
  spls <- rtables:::pos_splits(pos)
  vals <- rtables:::pos_splvals(pos)

  path <- character()
  for(i in seq_along(spls)) {
    path <- c(path,
              rtables::obj_name(spls[[i]]),
              ##rawvalues(vals[[i]]))
              rtables::value_names(vals[[i]]))
  }
  path
}

#' Create a FlexTable object representing an rtables TableTree
#'
#' @inheritParams export_as_txt
#' @param paginate logical(1). Should \code{tt} be paginated and exported as
#'   multiple flextables. Defaults to \code{FALSE}
#' @param total_width numeric(1). Total width in inches for the resulting
#'   flextable(s). Defaults to 5.
#' @return a flextable object
#'
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' library(dplyr)
#' library(rtables)
#'
#' analysisfun <- function(x, ...) {
#'     in_rows(row1 = 5,
#'             row2 = c(1, 2),
#'             .row_footnotes = list(row1 = "row 1 - row footnote"),
#'             .cell_footnotes = list(row2 = "row 2 - cell footnote"))
#' }
#'
#' lyt <- basic_table(title = "Title says Whaaaat", subtitles = "Oh, ok.",
#'                main_footer = "ha HA! Footer!") %>%
#' split_cols_by("ARM") %>%
#' analyze("AGE", afun = analysisfun)
#'
#' tbl <-  build_table(lyt, ex_adsl)
#' ft <- export_as_flextable(tbl)
#' ft
export_as_flextable <- function(tt, paginate = FALSE, lpp = NULL,
                                cpp = NULL,
                                ...,
                                colwidths = propose_column_widths(matrix_form(tt, indent_rownames = TRUE)),
                                tf_wrap = !is.null(cpp),
                                max_width = cpp,
                                total_width = 10) {
  if(!requireNamespace("flextable") || !requireNamespace("officer")) {
    stop("this function requires the flextable and officer packages. ",
         "Please install them if you wish to use it")
  }

  ## if we're paginating, just call
  if(paginate) {
    if(is.null(lpp))
      stop("lpp must be specified when calling export_as_flextable with paginate=TRUE")
    tabs <- rtables::paginate_table(tt, lpp = lpp, cpp = cpp, tf_wrap = tf_wrap, max_width = max_width, ...)
    return(lapply(tabs, export_as_flextable, paginate = FALSE, total_width = total_width, colwidths = colwidths))
  }

  final_cwidths <- total_width * colwidths / sum(colwidths)
  matform <- matrix_form(tt, indent_rownames = TRUE)

  ## this was nrow_header before but that seems wrong!
  hnum <- mf_nlheader(matform) ## attr(matform, "nrow_header")

  content <- as.data.frame(matform$strings[-(1:hnum), , drop = FALSE])

  rdf <- make_row_df(tt)

  hdr <- matform$strings[1:hnum, , drop = FALSE]

  flx <- flextable::qflextable(content)

  flx <- flextable::set_header_labels(flx, values = setNames(as.vector(hdr[hnum, , drop = TRUE]), names(content)))
  flx <- flextable::width(flx, width = final_cwidths)

  if(hnum > 1) {
    for(i in (hnum - 1):1) {
      sel <- spans_to_viscell(matform$spans[i, ])
      flx <- flextable::add_header_row(flx, top = TRUE,
                                       values = as.vector(hdr[i, sel]),
                                       colwidths = as.integer(matform$spans[i, sel]))
    }

  }
  flx <- flextable::align(flx, j = 2:(NCOL(tt) + 1), align = "center", part = "header")
  flx <- flextable::align(flx, j = 2:(NCOL(tt) + 1),  align = "center", part = "body")
  for(i in seq_len(NROW(tt))) {
    flx <- flextable::padding(flx, i = i, j = 1, padding.left = 10 * rdf$indent[[i]])
  }
  if(length(matform$ref_footnotes) > 0) {
    flx <- flextable::add_footer_lines(flx, values = matform$ref_footnotes)
  }

  if(length(all_titles(tt)) > 0 && any(nzchar(all_titles(tt)))) {
    real_titles <- all_titles(tt)
    real_titles <- real_titles[nzchar(real_titles)]
    flx <- flextable::hline(flx, i = 1L,
                            border = officer::fp_border(), part = "header")
    ## rev is because add_header_lines(top=TRUE) seems to put them in backwards!!! AAHHHH
    flx <- flextable::add_header_lines(flx, values = rev(real_titles),
                                       top = TRUE)
  }

  if(length(all_footers(tt)) > 0) {
    flx <- flextable::hline(flx, i = length(matform$ref_footnotes),
                            border = officer::fp_border(), part = "footer")
    flx <- flextable::add_footer_lines(flx, values = all_footers(tt))
  }

  flx <- flextable::font(flx, fontname = "courier")

  flextable::set_table_properties(flx, layout = "autofit")
}

.tab_to_colpath_set <- function(tt) {
  vapply(rtables::collect_leaves(rtables::coltree(tt)),
         function(y) paste(pos_to_path(rtables:::tree_pos(y)), collapse = " "),
         "")
}
.figure_out_colinds <- function(subtab, fulltab) {
  match(.tab_to_colpath_set(subtab),
        .tab_to_colpath_set(fulltab))
}

#' Export as PDF
#'
#' The PDF output is based on the ASCII output created with `toString`
#'
#' @inheritParams export_as_txt
#' @inheritParams grid::plotViewport
#' @param file file to write, must have `.pdf` extension
#' @param   width  Deprecated,  please  use   `pg_width`  or  specify
#'     `page_type`.  The width of  the graphics  region in inches
#' @param  height  Deprecated,  please  use  `pg_height`  or  specify
#'     `page_type`. The height of  the graphics  region in
#'     inches
#' @param  fontsize Deprecated,  please use  `font_size`. the  size of
#'     text (in points)
#' @param margins numeric(4). The number of lines/characters of margin on the
#'     bottom, left, top, and right sides of the page.
#' @param ... arguments passed on to `paginate_table`
#'
#' @importFrom grDevices pdf
#' @importFrom grid textGrob grid.newpage gpar pushViewport plotViewport unit grid.draw
#'   convertWidth convertHeight grobHeight grobWidth
#' @importFrom grid textGrob get.gpar
#' @importFrom grDevices dev.off
#' @importFrom tools file_ext
#'
#' @details By default, pagination is performed, with default
#' `cpp` and `lpp` defined by specified page dimensions and margins.
#' User-specified `lpp` and `cpp` values override this, and should
#' be used with caution.
#'
#' Title and footer materials are also word-wrapped by default
#' (unlike when printed to the terminal), with `cpp`, as
#' defined above, as the default `max_width`.
#'
#' @seealso [export_as_txt()]
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(rtables)
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2", "COUNTRY"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#'
#' \dontrun{
#' tf <- tempfile(fileext = ".pdf")
#' export_as_pdf(tbl, file = tf, pg_height = 4)
#' tf <- tempfile(fileext = ".pdf")
#' export_as_pdf(tbl, file = tf, lpp = 8)
#' }
export_as_pdf <- function(tt,
                          file,
                          page_type = "letter",
                          landscape = FALSE,
                          pg_width = page_dim(page_type)[if(landscape) 2 else 1],
                          pg_height = page_dim(page_type)[if(landscape) 1 else 2],
                          width = NULL,
                          height = NULL, # passed to pdf()
                          margins = c(4, 4, 4, 4),
                          font_family = "Courier",
                          fontsize = 8,  # grid parameters
                          font_size = fontsize,
                          paginate = TRUE,
                          lpp = NULL,
                          cpp = NULL,
                          hsep = "-",
                          indent_size = 2,
                          tf_wrap = TRUE,
                          max_width = NULL,
                          colwidths = propose_column_widths(matrix_form(tt, TRUE)),
                          ... # passed to paginate_table
) {
  stopifnot(file_ext(file) != ".pdf")
  if(!is.null(colwidths) && length(colwidths) != ncol(tt) + 1)
    stop("non-null colwidths argument must have length ncol(tt) + 1 [",
         ncol(tt) + 1, "], got length ", length(colwidths))

  gp_plot <- gpar(fontsize = font_size, fontfamily = font_family)

  ## soft deprecation. To become hard deprecation.
  if(!is.null(height))
    pg_height <- height

  if(!is.null(width))
    pg_width <- width

  if(missing(font_size) && !missing(fontsize))
    font_size <- fontsize

  pdf(file = file, width = pg_width, height = pg_height)
  on.exit(dev.off())
  grid.newpage()
  pushViewport(plotViewport(margins = margins, gp = gp_plot))

  cur_gpar <-  get.gpar()
  if (is.null(lpp)) {
    lpp <- floor(convertHeight(unit(1, "npc"), "lines", valueOnly = TRUE) /
                   (cur_gpar$cex * cur_gpar$lineheight)) - sum(margins[c(1, 3)]) # bottom, top
  }
  if(is.null(cpp)) {
    cpp <- floor(convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE) *
                   font_lcpi(font_family, font_size, cur_gpar$lineheight)$cpi) - sum(margins[c(2, 4)]) # left, right
  }
  if(tf_wrap && is.null(max_width))
    max_width <- cpp

  tbls <- if (paginate) {
    rtables::paginate_table(tt, lpp = lpp, cpp = cpp, tf_wrap = tf_wrap, max_width = max_width, colwidths = colwidths, ...)
  } else {
    list(tt)
  }
  stbls <- lapply(lapply(tbls,
                         function(tbl_i) {
                           cinds <- c(1, .figure_out_colinds(tbl_i, tt) + 1L)
                           toString(tbl_i, widths = colwidths[cinds], hsep = hsep,
                                    indent_size = indent_size, tf_wrap = tf_wrap,
                                    max_width = max_width)
                         }), function(xi) substr(xi, 1, nchar(xi) - nchar("\n")))
  gtbls <- lapply(stbls, function(txt) {
    textGrob(
      label = txt,
      x = unit(0, "npc"), y = unit(1, "npc"),
      just = c("left", "top")
    )
  })

  npages <- length(gtbls)
  exceeds_width <- rep(FALSE, npages)
  exceeds_height <- rep(FALSE, npages)

  for (i in seq_along(gtbls)) {
    g <- gtbls[[i]]

    if (i > 1) {
      grid.newpage()
      pushViewport(plotViewport(margins = margins, gp = gp_plot))
    }

    if (convertHeight(grobHeight(g), "inches", valueOnly = TRUE) >
        convertHeight(unit(1, "npc"), "inches", valueOnly = TRUE)) {
      exceeds_height[i] <- TRUE
      warning("height of page ", i, " exceeds the available space")
    }
    if (convertWidth(grobWidth(g), "inches", valueOnly = TRUE) >
        convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE)) {
      exceeds_width[i] <- TRUE
      warning("width of page ", i, " exceeds the available space")
    }

    grid.draw(g)
  }
  list(file = file, npages = npages, exceeds_width = exceeds_width, exceeds_height = exceeds_height, lpp = lpp, cpp = cpp)
}

.margin_lines_to_in <- function(margins, font_size, font_family) {
  tmpfile <- tempfile(fileext = ".pdf")
  gp_plot <- gpar(fontsize = font_size, fontfamily = font_family)
  pdf(file = tmpfile, width = 20, height = 20)
  on.exit({dev.off(); file.remove(tmpfile)})
  grid.newpage()
  pushViewport(plotViewport(margins = margins, gp = gp_plot))
  c(bottom = convertHeight(unit(margins["bottom"], "lines"), "inches", valueOnly = TRUE),
    left = convertWidth(unit(1, "strwidth", strrep("m", margins["left"])), "inches", valueOnly = TRUE),
    top = convertHeight(unit(margins["top"], "lines"), "inches", valueOnly = TRUE),
    right = convertWidth(unit(1, "strwidth", strrep("m", margins["right"])), "inches", valueOnly = TRUE))
}

#' Export table to RTF
#'
#' Experimental export to the RTF format.
#'
#' @details RTF export occurs by via the following steps
#'
#' \itemize{
#' \item{the table is paginated to the page size (Vertically and horizontally)}
#' \item{Each separate page is converted to a MatrixPrintForm and from there to RTF-encoded text}
#' \item{Separate rtfs text chunks are combined and written out as a single RTF file}
#' }
#'
#' Conversion of `MatrixPrintForm` objects to RTF is done via [mpf_to_rtf()].
#' @inheritParams export_as_txt
#' @inheritParams grid::plotViewport
#'
#' @export
export_as_rtf <- function(tt,
                          file = NULL,
                          colwidths = propose_column_widths(matrix_form(tt, TRUE)),
                          page_type = "letter",
                          pg_width = page_dim(page_type)[if(landscape) 2 else 1],
                          pg_height = page_dim(page_type)[if(landscape) 1 else 2],
                          landscape = FALSE,
                          margins = c(bottom = 4, left = 4, top=4, right = 4),
                          font_size = 8,
                          font_family = "Courier",
                          ...) {
  if(!requireNamespace("r2rtf"))
    stop("RTF export requires the r2rtf package, please install it.")
  if(is.null(names(margins)))
    names(margins) <- c("bottom", "left", "top", "right")
  if(!is.null(colwidths) && length(colwidths) != ncol(tt) + 1)
    stop("non-null colwidths argument must have length ncol(tt) + 1 [",
         ncol(tt) + 1, "], got length ", length(colwidths))

  margins_in <- .margin_lines_to_in(margins, font_size, font_family)
  true_width <- pg_width - sum(margins_in[c("left", "right")])
  true_height <- pg_height - sum(margins_in[c("top", "bottom")])

  tbls <- rtables::paginate_table(tt, font_family = font_family, font_size = font_size,
                         pg_width = true_width,
                         pg_height = true_height,
                         margins = c(bottom = 0, left = 0, top = 0, right = 0),
                         lineheight = 1.25,
                         colwidths = colwidths,
                         ...)

  rtftxts <- lapply(tbls, function(tbl) r2rtf::rtf_encode(mpf_to_rtf(tbl,
                                                                     colwidths = colwidths[c(1, .figure_out_colinds(tbl, tt))],
                                                                     page_type = page_type,
                                                                     pg_width = pg_width,
                                                                     pg_height = pg_height,
                                                                     font_size = font_size,
                                                                     margins = c(top = 0, left = 0, bottom = 0, right = 0))))
  restxt <- paste(rtftxts[[1]]$start,
                  paste(sapply(rtftxts, function(x) x$body), collapse = "\n{\\pard\\fs2\\par}\\page{\\pard\\fs2\\par}\n"),
                  rtftxts[[1]]$end)
  if(!is.null(file))
    cat(restxt, file = file)
  else
    restxt
}
