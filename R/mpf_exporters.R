## XXX Experimental. Not to be exported without approval
mpf_to_huxtable <- function(obj) {
    requireNamespace("huxtable")
    mf <- matrix_form(obj, indent_rownames = TRUE)
    nlr <- formatters:::mf_nlheader(mf)
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

mpf_to_rtf <- function(obj, ..., file) {
    huxt <- mpf_to_huxtable(obj)
    ## a bunch more stuff here
    huxtable::quick_rtf(huxt, ..., file = file)
}
