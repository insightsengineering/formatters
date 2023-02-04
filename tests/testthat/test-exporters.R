dfmf <- basic_matrix_form(mtcars)
main_title(dfmf) <- "main title"
main_footer(dfmf) <- "main footer"
prov_footer(dfmf) <- "prov footer"
subtitles(dfmf) <- c("sub", "titles")

mf_rfnotes(dfmf) <- c("fnote 1 is the coolest",
                      "no way, fnote 2 forever")

fil <- tempfile(fileext = ".rtf")


myrtf <- mpf_to_rtf(dfmf)
r2rtf::write_rtf(myrtf, fil)
expect_true(file.exists(fil))
