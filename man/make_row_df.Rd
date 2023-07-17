% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R
\name{make_row_df}
\alias{make_row_df}
\title{Make row and column layout summary data.frames for use during pagination}
\usage{
make_row_df(
  tt,
  colwidths = NULL,
  visible_only = TRUE,
  rownum = 0,
  indent = 0L,
  path = character(),
  incontent = FALSE,
  repr_ext = 0L,
  repr_inds = integer(),
  sibpos = NA_integer_,
  nsibs = NA_integer_,
  max_width = NULL
)
}
\arguments{
\item{tt}{ANY. Object representing the table-like object to be summarized.}

\item{colwidths}{numeric. Internal detail do not set manually.}

\item{visible_only}{logical(1). Should only visible aspects of the table structure be reflected in this summary.
Defaults to \code{TRUE}. May not be supported by all methods.}

\item{rownum}{numeric(1). Internal detail do not set manually.}

\item{indent}{integer(1). Internal detail do not set manually.}

\item{path}{character.  Path  to  the (sub)table  represented  by
\code{tt}. Defaults to \code{character()}}

\item{incontent}{logical(1). Internal detail do not set manually.}

\item{repr_ext}{integer(1). Internal detail do not set manually.}

\item{repr_inds}{integer. Internal detail do not set manually.}

\item{sibpos}{integer(1). Internal detail do not set manually.}

\item{nsibs}{integer(1). Internal detail do not set manually.}

\item{max_width}{numeric(1) or NULL. Maximum width for title/footer
materials.}
}
\value{
a data.frame of row/column-structure information used by the pagination machinery.
}
\description{
Make row and column layout summary data.frames for use during pagination
}
\details{
When  \code{visible_only} is  \code{TRUE} (the  default),
methods should  return a  data.frame with  exactly one  row per
visible  row in  the table-like  object.  This  is useful  when
reasoning about  how a table  will print, but does  not reflect
the full pathing space of the structure (though the paths which
are given will all work as is).

If  supported,  when  \code{visible_only}  is  \code{FALSE},  every
structural element of the table (in row-space) will be reflected in
the  returned data.frame,  meaning the  full pathing-space  will be
represented but some rows in  the layout summary will not represent
printed rows in the table as it is displayed.

Most arguments beyond \code{tt} and \code{visible_only} are present so that
\code{make_row_df} methods can call \code{make_row_df} recursively and retain information,
and should not be set during a top-level call
}
\note{
the technically present root tree node is excluded from the summary returned by
both \code{make_row_df} and \code{make_col_df}, as it is simply the
row/column structure of \code{tt} and thus not useful for pathing or pagination.
}