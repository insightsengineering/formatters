# Pagination

Pagination

## Pagination Algorithm

Pagination is performed independently in the vertical and horizontal
directions based solely on a *pagination data frame*, which includes the
following information for each row/column:

- Number of lines/characters rendering the row will take **after
  word-wrapping** (`self_extent`)

- The indices (`reprint_inds`) and number of lines (`par_extent`) of the
  rows which act as **context** for the row

- The row's number of siblings and position within its siblings

Given `lpp` (`cpp`) is already adjusted for rendered elements which are
not rows/columns and a data frame of pagination information, pagination
is performed via the following algorithm with `start = 1`.

Core Pagination Algorithm:

1.  Initial guess for pagination position is `start + lpp`
    (`start + cpp`)

2.  While the guess is not a valid pagination position, and
    `guess > start`, decrement guess and repeat.

    - An error is thrown if all possible pagination positions between
      `start` and `start + lpp` (`start + cpp`) would be `< start` after
      decrementing

3.  Retain pagination index

4.  If pagination point was less than `NROW(tt)` (`ncol(tt)`), set
    `start` to `pos + 1`, and repeat steps (1) - (4).

Validating Pagination Position:

Given an (already adjusted) `lpp` or `cpp` value, a pagination is
invalid if:

- The rows/columns on the page would take more than (adjusted) `lpp`
  lines/`cpp` characters to render **including**:

  - word-wrapping

  - (vertical only) context repetition

- (vertical only) footnote messages and/or section divider lines take up
  too many lines after rendering rows

- (vertical only) row is a label or content (row-group summary) row

- (vertical only) row at the pagination point has siblings, and it has
  less than `min_siblings` preceding or following siblings

- pagination would occur within a sub-table listed in `nosplitin`
