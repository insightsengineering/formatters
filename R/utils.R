## credit: rlang, Henry and Wickham.
## this one tiny utility function is NOT worth a dependency.
## modified it so any length 0 x grabs y

#' `%||%` (if length-0) alternative operator
#'
#' @param a (`any`)\cr element to select *only* if it is not of length 0.
#' @param b (`any`)\cr element to select if `a` has length 0.
#'
#' @return `a` if it is not of length 0, otherwise `b`.
#'
#' @examples
#' 6 %||% 10
#'
#' character() %||% "hi"
#'
#' NULL %||% "hi"
#'
#' @export
#' @name ifnotlen0
`%||%` <- function(a, b) {
  if (length(a) == 0) {
    b
  } else {
    a
  }
}
