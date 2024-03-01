#' Default page number
#'@export
default_page_number <- function() {
  getOption("formatter_default_page_number", default = NULL)
}

#' Set default page number
#' @export
set_default_page_number <- function(page_number) {
  options("formatter_default_page_number" = page_number)
}
