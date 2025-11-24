# Find pagination indices from pagination info data frame

Pagination methods should typically call the `make_row_df` method for
their object and then call this function on the resulting pagination
info `data.frame`.

## Usage

``` r
pag_indices_inner(
  pagdf,
  rlpp,
  lpp_or_cpp = NA_integer_,
  context_lpp_or_cpp = NA_integer_,
  min_siblings,
  nosplitin = character(),
  verbose = FALSE,
  row = TRUE,
  have_col_fnotes = FALSE,
  div_height = 1L,
  col_gap = 3L,
  has_rowlabels
)
```

## Arguments

- pagdf:

  (`data.frame`)  
  a pagination info `data.frame` as created by either `make_rows_df` or
  `make_cols_df`.

- rlpp:

  (`numeric`)  
  maximum number of *row* lines per page (not including header
  materials), including (re)printed header and context rows.

- lpp_or_cpp:

  (`numeric`)  
  total maximum number of *row* lines or content (column-wise
  characters) per page (including header materials and context rows).
  This is only for informative results with `verbose = TRUE`. It will
  print `NA` if not specified by the pagination machinery.

- context_lpp_or_cpp:

  (`numeric`)  
  total number of context *row* lines or content (column-wise
  characters) per page (including header materials). Uses `NA` if not
  specified by the pagination machinery and is only for informative
  results with `verbose = TRUE`.

- min_siblings:

  (`numeric`)  
  minimum sibling rows which must appear on either side of pagination
  row for a mid-subtable split to be valid. Defaults to 2 for tables. It
  is automatically turned off (set to 0) for listings.

- nosplitin:

  (`character`)  
  list of names of subtables where page breaks are not allowed,
  regardless of other considerations. Defaults to none.

- verbose:

  (`flag`)  
  whether additional informative messages about the search for
  pagination breaks should be shown. Defaults to `FALSE`.

- row:

  (`flag`)  
  whether pagination is happening in row space (`TRUE`, the default) or
  column space (`FALSE`).

- have_col_fnotes:

  (`flag`)  
  whether the table-like object being rendered has column-associated
  referential footnotes.

- div_height:

  (`numeric(1)`)  
  the height of the divider line when the associated object is rendered.
  Defaults to `1`.

- col_gap:

  (`numeric(1)`)  
  width of gap between columns, in same units as extent in `pagdf`
  (spaces under a particular font specification).

- has_rowlabels:

  (`logical(1)`)  
  whether the object being paginated has row labels.

## Value

A `list` containing a vector of row numbers, broken up by page.

## Details

`pab_indices_inner` implements the core pagination algorithm (see below)
for a single direction (vertical if `row = TRUE` (the default),
horizontal otherwise) based on the pagination data frame and (already
adjusted for non-body rows/columns) lines (or characters) per page.

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

## Examples

``` r
mypgdf <- basic_pagdf(row.names(mtcars))

paginds <- pag_indices_inner(mypgdf, rlpp = 15, min_siblings = 0)
lapply(paginds, function(x) mtcars[x, ])
#> [[1]]
#>                     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4          21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag      21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710         22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive     21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout  18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant            18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360         14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D          24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230           22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280           19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C          17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE         16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL         17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC        15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> 
#> [[2]]
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> 
#> [[3]]
#>                mpg cyl disp  hp drat   wt qsec vs am gear carb
#> Maserati Bora 15.0   8  301 335 3.54 3.57 14.6  0  1    5    8
#> Volvo 142E    21.4   4  121 109 4.11 2.78 18.6  1  1    4    2
#> 
```
