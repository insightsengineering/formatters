## XXX Experimental. Not to be exported without approval
mpf_to_huxtable <- function(obj) {
    requireNamespace("huxtable")
    mf <- matrix_form(obj, indent_rownames = TRUE)
    nlr <- mf_nlheader(mf)
    myfakedf <- as.data.frame(tail(mf$strings, -nlr))
    ret <- huxtable::as_hux(myfakedf, add_colnames = FALSE)
    mf$strings[!mf$display] <- ""
    for (i in seq_len(nlr)) {
        arglist <- c(
            list(ht = ret, after = i - 1),
            as.list(mf$strings[i, ])
        )
        ret <- do.call(huxtable::insert_row, arglist)

        spanspl <- split(
            seq_len(ncol(mf$strings)),
            cumsum(mf$display[i, ])
        )

        for (j in seq_along(spanspl)) {
            if (length(spanspl[[j]]) > 1) {
                ret <- huxtable::merge_cells(ret, row = i, col = spanspl[[j]])
            }
        }
    }
    ret <- huxtable::set_header_rows(ret, seq_len(nlr), TRUE)
    huxtable::font(ret) <- "courier"
    huxtable::font_size(ret) <- 6
    huxtable::align(ret)[
        seq_len(nrow(ret)),
        seq_len(ncol(ret))
    ] <- mf$aligns
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
    body_df <- as.data.frame(mf$strings[-1 * seq_len(nlh), ])
    varnamerow <- mf_nrheader(mf)
    ## detect if we have counts
    if (any(nzchar(mf$formats[seq_len(nlh), ]))) {
        varnamerow <- varnamerow - 1
    }

    rlbl_lst <- as.list(mf$strings[nlh, , drop = TRUE])
    names(rlbl_lst) <- names(body_df)

    ret <- gt::gt(body_df, rowname_col = "V1")
    ret <- gt::cols_label(ret, .list = rlbl_lst)
    if (nlh > 1) {
        for (i in 1:(nlh - 1)) {
            linedat <- mf$strings[i, , drop = TRUE]
            splvec <- cumsum(mf$display[i, , drop = TRUE])
            spl <- split(seq_along(linedat), splvec)
            for (j in seq_along(spl)) {
                vns <- names(body_df)[spl[[j]]]
                labval <- linedat[spl[[j]][1]]
                ret <- gt::tab_spanner(
                    ret,
                    label = labval,
                    columns = {{ vns }},
                    level = nlh - i,
                    id = paste0(labval, j)
                )
            }
        }
    }

    ret <- gt::opt_css(ret, css = "th.gt_left { white-space:pre;}")

    ret
}
