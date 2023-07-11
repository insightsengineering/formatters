## credit: rlang, Henry and Wickham.
## this one tiny utility function is NOT worth a dependency.
## modified it so any length 0 x grabs y

#' `%||%` If length-0 alternative operator
#' @name ifnotlen0
#'
#'
#'
#' @param a ANY. Element to select only if it is not length 0
#' @param b ANY. Element to select if \code{a} is length 0
#' @export
#' @examples
#' 6 %||% 10
#'
#' character() %||% "hi"
#'
#' NULL %||% "hi"
#' @return `a`, unless it  is length 0, in which case  `b` (even in the
#'     case `b` is also length 0)
`%||%` <- function(a, b) if (length(a) == 0) b else a

