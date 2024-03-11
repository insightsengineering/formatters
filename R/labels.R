#' Return an object with a label attribute
#'
#' @param x (`any`)\cr an object.
#' @param label (`character(1)`)\cr label attribute to attach to `x`.
#'
#' @return `x` labeled by `label`. Note that the exact mechanism of labeling should be considered
#'   an internal implementation detail, but the label will always be retrieved via `obj_label`.
#'
#' @examples
#' x <- with_label(c(1, 2, 3), label = "Test")
#' obj_label(x)
#'
#' @export
with_label <- function(x, label) {
  obj_label(x) <- label
  x
}

#' Get label attributes of variables in a `data.frame`
#'
#' Variable labels can be stored as a `label` attribute for each variable.
#' This functions returns a named character vector with the variable labels
#' (or empty strings if not specified).
#'
#' @param x (`data.frame`)\cr a data frame object.
#' @param fill (`logical(1)`)\cr whether variable names should be returned for variables for
#'   which the `label` attribute does not exist. If `FALSE`, these variables are filled with
#'   `NA`s instead.
#'
#' @return a named character vector of variable labels from `x`, with names corresponding
#'   to variable names.
#'
#' @examples
#' x <- iris
#' var_labels(x)
#' var_labels(x) <- paste("label for", names(iris))
#' var_labels(x)
#'
#' @export
var_labels <- function(x, fill = FALSE) {
  stopifnot(is.data.frame(x))
  if (NCOL(x) == 0) {
    return(character())
  }

  y <- Map(function(col, colname) {
    label <- attr(col, "label")

    if (is.null(label)) {
      if (fill) {
        colname
      } else {
        NA_character_
      }
    } else {
      if (!is.character(label) && !(length(label) == 1)) {
        stop("label for variable ", colname, "is not a character string")
      }
      as.vector(label)
    }
  }, x, colnames(x))

  labels <- unlist(y, recursive = FALSE, use.names = TRUE)

  if (!is.character(labels)) {
    stop("label extraction failed")
  }

  labels
}

#' Set label attributes of all variables in a `data.frame`
#'
#' Variable labels can be stored as the `label` attribute for each variable.
#' This functions sets all non-missing (non-`NA`) variable labels in a `data.frame`.
#'
#' @inheritParams var_labels
#' @param value (`character`)\cr a vector of new variable labels. If any values are `NA`,
#'   the label for that variable is removed.
#'
#' @return `x` with modified variable labels.
#'
#' @examples
#' x <- iris
#' var_labels(x)
#' var_labels(x) <- paste("label for", names(iris))
#' var_labels(x)
#'
#' if (interactive()) {
#'   View(x) # in RStudio data viewer labels are displayed
#' }
#'
#' @export
`var_labels<-` <- function(x, value) {
  stopifnot(
    is.data.frame(x),
    is.character(value),
    ncol(x) == length(value)
  )

  theseq <- if (!is.null(names(value))) names(value) else seq_along(x)
  # across columns of x
  for (j in theseq) {
    attr(x[[j]], "label") <- if (!is.na(value[j])) {
      unname(value[j])
    } else {
      NULL
    }
  }

  x
}

#' Copy and change variable labels of a `data.frame`
#'
#' Relabel a subset of the variables.
#'
#' @inheritParams var_labels<-
#' @param ... name-value pairs, where each name corresponds to a variable name in
#'   `x` and the value to the new variable label.
#'
#' @return A copy of `x` with labels modified according to `...`
#'
#' @examples
#' x <- var_relabel(iris, Sepal.Length = "Sepal Length of iris flower")
#' var_labels(x)
#'
#' @export
var_relabel <- function(x, ...) {
  # todo: make this function more readable / code easier
  stopifnot(is.data.frame(x))
  if (missing(...)) {
    return(x)
  }
  dots <- list(...)
  varnames <- names(dots)
  stopifnot(!is.null(varnames))

  map_varnames <- match(varnames, colnames(x))

  if (any(is.na(map_varnames))) {
    stop("variables: ", paste(varnames[is.na(map_varnames)], collapse = ", "), " not found")
  }

  if (any(vapply(dots, Negate(is.character), logical(1)))) {
    stop("all variable labels must be of type character")
  }

  for (i in seq_along(map_varnames)) {
    attr(x[[map_varnames[[i]]]], "label") <- dots[[i]]
  }

  x
}

#' Remove variable labels of a `data.frame`
#'
#' Remove `label` attribute from all variables in a data frame.
#'
#' @param x (`data.frame`)\cr a `data.frame` object.
#'
#' @return `x` with its variable labels stripped.
#'
#' @examples
#' x <- var_labels_remove(iris)
#'
#' @export
var_labels_remove <- function(x) {
  stopifnot(is.data.frame(x))

  for (i in seq_len(ncol(x))) {
    attr(x[[i]], "label") <- NULL
  }

  x
}
