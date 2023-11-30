make_basemf_fnotes <- function() {
  dfmf <- basic_matrix_form(mtcars)
  main_title(dfmf) <- "main title"
  main_footer(dfmf) <- "main footer"
  prov_footer(dfmf) <- "prov footer"
  subtitles(dfmf) <- c("sub", "titles")

  dfmf$strings[2, 2] <- paste(dfmf$strings[2, 2, drop = TRUE], "{*}")
  dfmf$strings[3, 4] <- paste(dfmf$strings[3, 4, drop = TRUE], "{1}")
  dfmf$strings[6, 3] <- paste(dfmf$strings[6, 3, drop = TRUE], "{2}")
  dfmf$strings[10, 1:2] <- paste(dfmf$strings[10, 1:2], "{*}")
  dfmf$strings[11, 4:5] <- paste(dfmf$strings[11, 4:5], "{*}")

  mf_rfnotes(dfmf) <- c(
    "{1} - fnote 1 is the coolest",
    "{2} - no way, fnote 2 forever",
    "{*} - symbooollllssss"
  )

  dfmf <- mform_build_refdf(dfmf)
  mf_rfnotes(dfmf) <- reconstruct_basic_fnote_list(dfmf)
  mf_col_widths(dfmf) <- propose_column_widths(dfmf)
  dfmf
}
