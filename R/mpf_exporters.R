## XXX Experimental. Not to be exported without approval
mpf_to_huxtable <- function(obj) {
    if(!requireNamespace("huxtable"))
        stop("mpf_to_huxtable requires the huxtable package")
    mf <- matrix_form(obj, indent_rownames = TRUE)
    nlr <- mf_nlheader(mf)
    myfakedf <- as.data.frame(tail(mf$strings, -nlr))
    ret <- huxtable::as_hux(myfakedf, add_colnames = FALSE)
    mf$strings[!mf$display] <- ""
    for(i in seq_len(nlr)) {
        arglist <- c(list(ht = ret, after = i - 1),
                     as.list(mf$strings[i,]))
        ret <- do.call(huxtable::insert_row, arglist)

        spanspl <- split(seq_len(ncol(mf$strings)),
                         cumsum(mf$display[i,]))


        for(j in seq_along(spanspl)) {
            if(length(spanspl[[j]]) > 1) {
                ret <- huxtable::merge_cells(ret, row = i, col = spanspl[[j]])
            }
        }
    }
    ret <- huxtable::set_header_rows(ret, seq_len(nlr), TRUE)
    huxtable::font(ret) <- "courier"
    huxtable::font_size(ret) <- 6
    huxtable::align(ret)[seq_len(nrow(ret)),
               seq_len(ncol(ret))] <- mf$aligns
    ret
}

## XXX Experimental. Not to be exported without approval
mpf_to_rtf <- function(obj, ..., file) {
    huxt <- mpf_to_huxtable(obj)
    ## a bunch more stuff here
    huxtable::quick_rtf(huxt, ..., file = file)
}




## XXX Experimental. Not to be exported without approval
mpf_to_gt <- function(obj) {
    requireNamespace("gt")
    mf <- matrix_form(obj, indent_rownames = TRUE)
    nlh <- mf_nlheader(mf)
    body_df <- as.data.frame(mf$strings[-1*seq_len(nlh),])
    varnamerow <- mf_nrheader(mf)
    ## detect if we have counts
    if(any(nzchar(mf$formats[seq_len(nlh),]))) {
        varnamerow <- varnamerow - 1
    }

    rlbl_lst <- as.list(mf$strings[nlh, , drop = TRUE])
    names(rlbl_lst) <- names(body_df)

    ret <- gt::gt(body_df, rowname_col = "V1")
    ret <- gt::cols_label(ret, .list = rlbl_lst)
    if(nlh > 1) {
        for(i in 1 : (nlh - 1)) {
            linedat <- mf$strings[i,, drop = TRUE]
            splvec <- cumsum(mf$display[i,, drop = TRUE])
            spl <- split(seq_along(linedat), splvec)
            for(j in seq_along(spl)) {
                vns <- names(body_df)[spl[[j]]]
                labval <- linedat[spl[[j]][1]]
                ret <- gt::tab_spanner(ret, label = labval,
                                   columns = {{vns}},
                                   level = nlh - i,
                                   id = paste0(labval, j))
            }

        }
    }

    ret <- gt::opt_css(ret, css = "th.gt_left { white-space:pre;}")

    ret
}


prep_header_line <- function(mf, i) {
    ret <- mf$strings[i, mf$display[i,, drop = TRUE], drop = TRUE]
    ret
}

margin_lines_to_in <- function(margins, font_size, font_family) {
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



mpf_to_dfbody <- function(mpf, colwidths) {
    mf <- matrix_form(mpf, indent_rownames = TRUE)
    nlr <- mf_nlheader(mf)
    if(is.null(colwidths))
        colwidths <- propose_column_widths(tbl)
    mf$strings[1:nlr, 1] <- ifelse(nzchar(mf$strings[1:nlr, 1, drop = TRUE]),
                                   mf$strings[1:nlr, 1, drop = TRUE],
                                   strrep(" ", colwidths))


    myfakedf <- as.data.frame(tail(mf$strings, -nlr))
    myfakedf
}


#' Transform MPF to RTF
#'
#' Experimental export to RTF via the `r2rtf` package
#'
#' @inheritParams page_lcpp
#' @inheritParams toString
#' @inheritParams grid::plotViewport
#' @details This function provides a low-level coercion of a
#' `MatrixPrintForm object into text containing the corresponding
#' table in `RTF`. Currently, no pagination is done at this level,
#' and should be done prior to calling this function, though that
#' may change in the future.
#'
#' @return An rtf object
#' @export
mpf_to_rtf <- function(mpf,
                       colwidths = NULL,
                       page_type = "letter",
                       pg_width = page_dim(page_type)[if(landscape) 2 else 1],
                       pg_height = page_dim(page_type)[if(landscape) 1 else 2],
                       landscape = FALSE,
                       margins = c(4, 4, 4, 4),
                       font_size = 8,
                       ...) {

    if(!requireNamespace("r2rtf"))
        stop("RTF export requires the 'r2rtf' function please install it")
    mpf <- matrix_form(mpf, indent_rownames = TRUE)
    nlr <- mf_nlheader(mpf)
    if(is.null(colwidths))
        colwidths <- propose_column_widths(mpf)
    mpf$strings[1:nlr, 1] <- ifelse(nzchar(mpf$strings[1:nlr, 1, drop = TRUE]),
                                   mpf$strings[1:nlr, 1, drop = TRUE],
                                   strrep(" ", colwidths))

    myfakedf <- mpf_to_dfbody(mpf, colwidths)

    rtfpg <- r2rtf::rtf_page(myfakedf, width = pg_width,
                      height = pg_height,
                      orientation = if(landscape) "landscape" else "portrait",
                      margin = c(0.1,0.1,0.1,0.1, 0.1, 0.1),
                      nrow = 10000L) ## dont allow r2rtf to restrict lines per page beyond actual real eastate
    rtfpg <- r2rtf::rtf_title(rtfpg, main_title(mpf), subtitles(mpf), text_font = 1)
    for(i in seq_len(nlr)) {
        hdrlndat <- prep_header_line(mpf, i)
        rtfpg <- r2rtf::rtf_colheader(rtfpg,
                               paste(hdrlndat, collapse = " | "),
                               col_rel_width = unlist(tapply(colwidths,
                                                             cumsum(mpf$display[i,,drop = TRUE]),
                                                             sum,
                                                             simplify = FALSE)),
                               border_top = c("", rep(if(i>1) "single" else "", length(hdrlndat) - 1)),
                               text_font = 9, ## this means Courier New for some insane reason
                               text_font_size = font_size)
    }

    rtfpg <- r2rtf::rtf_body(rtfpg,
                      col_rel_width = colwidths,
                      text_justification = c("l", rep("c", ncol(myfakedf) - 1)),
                      text_format = "",
                      text_font = 9,
                      text_font_size = font_size)

    for(i in seq_along(mpf$ref_footnotes)) {

        rtfpg <- r2rtf::rtf_footnote(rtfpg,
                              mpf$ref_footnotes[i],
                              border_top = if(i == 1) "single" else "",
                              border_bottom = if(i == length(mpf$ref_footnotes)) "single" else "",
                              text_font = 9)
    }

    if(length(main_footer(mpf)) > 0) {
        rtfpg <- r2rtf::rtf_footnote(rtfpg, main_footer(mpf), text_font = 9)
    }
    if(length(prov_footer(mpf)) > 0) {
        rtfpg <- r2rtf::rtf_source(rtfpg, prov_footer(mpf), text_font = 9)
    }

    rtfpg

}


