dfmf <- basic_matrix_form(mtcars)
main_title(dfmf) <- "main title"
main_footer(dfmf) <- "main footer"
prov_footer(dfmf) <- "prov footer"
subtitles(dfmf) <- c("sub", "titles")

dfmf$strings[2, 2] <- paste(dfmf$strings[2, 2, drop = TRUE], "{*}")
dfmf$strings[3, 4] <- paste(dfmf$strings[3, 4, drop = TRUE], "{1}")
dfmf$strings[6, 3] <- paste(dfmf$strings[6, 3, drop = TRUE], "{2}")
dfmf$strings[10, 1:3] <- paste(dfmf$strings[10, 1:3], "{*}")
dfmf$strings[11, 4:6] <- paste(dfmf$strings[11, 4:6], "{*}")

mf_rfnotes(dfmf) <- c("{1} - fnote 1 is the coolest",
                      "{2} - no way, fnote 2 forever",
                      "{*} - symbooollllssss")

formatters:::mf_fnote_df(dfmf) <- formatters:::mform_build_refdf(dfmf)
mf_rfnotes(dfmf) <- formatters:::reconstruct_basic_fnote_list(dfmf)
formatters:::mf_col_widths(dfmf) <- propose_column_widths(dfmf)
fil <- tempfile(fileext = ".rtf")


myrtf <- mpf_to_rtf(dfmf)
r2rtf::write_rtf(myrtf, fil)
expect_true(file.exists(fil))


fil2 <- tempfile(fileext = ".txt")

export_as_txt(dfmf, file = fil2, page_break = "~~~~\n", verbose = TRUE)
exptlines <- readLines(fil2)

## 2 pages, 1 page break
pbreak_pos <- grep("~", exptlines, fixed = TRUE)
expect_identical(length(pbreak_pos),
                 1L)

## one per page
sym_msg_pos <- grep("{*} - symbooollllssss",  exptlines, fixed = TRUE)

expect_identical(length(grep("{*} - symbooollllssss",  exptlines, fixed = TRUE)),
                 2L)
expect_true(sym_msg_pos[1] < pbreak_pos &&
            sym_msg_pos[2] > pbreak_pos)

## 4 on first page, incl the message, 2 on second page incl message
expect_identical(length(grep("{*}", exptlines, fixed = TRUE)),
                 6L)



exp_h_pags <- 3L
exp_v_pags <- 2L
fil3 <- tempfile(fileext = ".txt")
export_as_txt(dfmf, pg_height = 4, pg_width = 4, file = fil3, page_break = "~~~~\n",
              margins = rep(0, 4))
exptlines <- readLines(fil3)

msg_1_pos <- grep("{1} - fnote 1 is the coolest",  exptlines, fixed = TRUE)
expect_identical(length(msg_1_pos),
                 1L)

msg_asterisk_pos <- grep("{*} - symbooollllssss",  exptlines, fixed = TRUE)
expect_identical(length(msg_asterisk_pos),
                 exp_h_pags)

expect_true(msg_1_pos > min(msg_asterisk_pos))


expect_identical(length(grep("~", exptlines, fixed = TRUE)),
                 exp_h_pags * exp_v_pags - 1L)
