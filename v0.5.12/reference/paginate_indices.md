# Paginate a table-like object for rendering

These functions perform or diagnose bi-directional pagination on an
object.

## Usage

``` r
paginate_indices(
  obj,
  page_type = "letter",
  font_family = "Courier",
  font_size = 8,
  lineheight = 1,
  landscape = FALSE,
  pg_width = NULL,
  pg_height = NULL,
  margins = c(top = 0.5, bottom = 0.5, left = 0.75, right = 0.75),
  lpp = NA_integer_,
  cpp = NA_integer_,
  min_siblings = 2,
  nosplitin = list(rows = character(), cols = character()),
  colwidths = NULL,
  tf_wrap = FALSE,
  max_width = NULL,
  indent_size = 2,
  pg_size_spec = NULL,
  rep_cols = num_rep_cols(obj),
  col_gap = 3,
  fontspec = font_spec(font_family, font_size, lineheight),
  round_type = obj_round_type(obj),
  verbose = FALSE
)

paginate_to_mpfs(
  obj,
  page_type = "letter",
  font_family = "Courier",
  font_size = 8,
  lineheight = 1,
  landscape = FALSE,
  pg_width = NULL,
  pg_height = NULL,
  margins = c(top = 0.5, bottom = 0.5, left = 0.75, right = 0.75),
  lpp = NA_integer_,
  cpp = NA_integer_,
  min_siblings = 2,
  nosplitin = character(),
  colwidths = NULL,
  tf_wrap = FALSE,
  max_width = NULL,
  indent_size = 2,
  pg_size_spec = NULL,
  page_num = default_page_number(),
  rep_cols = NULL,
  col_gap = 3,
  fontspec = font_spec(font_family, font_size, lineheight),
  round_type = obj_round_type(obj),
  verbose = FALSE
)

diagnose_pagination(
  obj,
  page_type = "letter",
  font_family = "Courier",
  font_size = 8,
  lineheight = 1,
  landscape = FALSE,
  pg_width = NULL,
  pg_height = NULL,
  margins = c(top = 0.5, bottom = 0.5, left = 0.75, right = 0.75),
  lpp = NA_integer_,
  cpp = NA_integer_,
  min_siblings = 2,
  nosplitin = character(),
  colwidths = propose_column_widths(matrix_form(obj, TRUE, round_type = round_type),
    fontspec = fontspec, round_type = round_type),
  tf_wrap = FALSE,
  max_width = NULL,
  indent_size = 2,
  pg_size_spec = NULL,
  rep_cols = num_rep_cols(obj),
  col_gap = 3,
  verbose = FALSE,
  fontspec = font_spec(font_family, font_size, lineheight),
  round_type = obj_round_type(obj),
  ...
)
```

## Arguments

- obj:

  (`ANY`)  
  object to be paginated. Must have a
  [`matrix_form()`](https://insightsengineering.github.io/formatters/reference/matrix_form.md)
  method.

- page_type:

  (`string`)  
  name of a page type. See
  [`page_types`](https://insightsengineering.github.io/formatters/reference/page_types.md).
  Ignored when `pg_width` and `pg_height` are set directly.

- font_family:

  (`string`)  
  name of a font family. An error will be thrown if the family named is
  not monospaced. Defaults to `"Courier"`.

- font_size:

  (`numeric(1)`)  
  font size. Defaults to `12`.

- lineheight:

  (`numeric(1)`)  
  line height. Defaults to `1`.

- landscape:

  (`flag`)  
  whether the dimensions of `page_type` should be inverted for landscape
  orientation. Defaults to `FALSE`, ignored when `pg_width` and
  `pg_height` are set directly.

- pg_width:

  (`numeric(1)`)  
  page width in inches.

- pg_height:

  (`numeric(1)`)  
  page height in inches.

- margins:

  (`numeric(4)`)  
  named numeric vector containing `"bottom"`, `"left"`, `"top"`, and
  `"right"` margins in inches. Defaults to `.5` inches for both vertical
  margins and `.75` for both horizontal margins.

- lpp:

  (`numeric(1)` or `NULL`)  
  lines per page. If `NA` (the default), this is calculated
  automatically based on the specified page size). `NULL` indicates no
  vertical pagination should occur.

- cpp:

  (`numeric(1)` or `NULL`)  
  width (in characters) per page. If `NA` (the default), this is
  calculated automatically based on the specified page size). `NULL`
  indicates no horizontal pagination should occur.

- min_siblings:

  (`numeric`)  
  minimum sibling rows which must appear on either side of pagination
  row for a mid-subtable split to be valid. Defaults to 2 for tables. It
  is automatically turned off (set to 0) for listings.

- nosplitin:

  (`character`)  
  list of names of subtables where page breaks are not allowed,
  regardless of other considerations. Defaults to none.

- colwidths:

  (`numeric`)  
  vector of column widths (in characters) for use in vertical
  pagination.

- tf_wrap:

  (`flag`)  
  whether the text for title, subtitles, and footnotes should be
  wrapped.

- max_width:

  (`integer(1)`, `string` or `NULL`)  
  width that title and footer (including footnotes) materials should be
  word-wrapped to. If `NULL`, it is set to the current print width of
  the session (`getOption("width")`). If set to `"auto"`, the width of
  the table (plus any table inset) is used. Parameter is ignored if
  `tf_wrap = FALSE`.

- indent_size:

  (`numeric(1)`)  
  indent size, in characters. Ignored when `x` is already a
  `MatrixPrintForm` object in favor of information there.

- pg_size_spec:

  (`page_size_spec`)  
  . a pre-calculated page size specification. Typically this is not set
  by end users.

- rep_cols:

  (`numeric(1)`)  
  number of *columns* (not including row labels) to be repeated on every
  page. Defaults to 0.

- col_gap:

  (`numeric(1)`)  
  The number of spaces to be placed between columns in the rendered
  table (and assumed for horizontal pagination).

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

- round_type:

  (`string`)  
  .  
  The type of rounding to perform. Allowed values: (`"iec"`, `"iec_mod"`
  or `"sas"`)  
  iec, the default, and iec_mod performs rounding compliant with IEC
  60559 (see notes in
  [`round_fmt()`](https://insightsengineering.github.io/formatters/reference/round_fmt.md)),
  while sas performs nearest-value rounding consistent with rounding
  within SAS.  
  In addition, the rounding of a negative number that rounds to zero
  will be presented as 0 (with the appropriate number of trailing zeros)
  for both `sas` and `iec_mod`, while for `iec`, it will be presented as
  -0 (with the appropriate number of trailing zeros).

- verbose:

  (`flag`)  
  whether additional informative messages about the search for
  pagination breaks should be shown. Defaults to `FALSE`.

- page_num:

  (`string`)  
  placeholder string for page numbers. See
  [default_page_number](https://insightsengineering.github.io/formatters/reference/default_page_number.md)
  for more information. Defaults to `NULL`.

- ...:

  additional parameters passed to individual methods.

## Value

- `paginate_indices` returns a `list` with two elements of the same
  length: `pag_row_indices` and `pag_col_indices`.

- `paginate_to_mpfs` returns a `list` of `MatrixPrintForm` objects
  representing each individual page after pagination (including forced
  pagination if necessary).

&nbsp;

- `diagnose_pagination` returns a `list` containing:

  - `lpp_diagnostics`:

    Diagnostic information regarding lines per page.

  - `row_diagnostics`:

    Basic information about rows, whether pagination was attempted after
    each row, and the final result of such an attempt, if made.

  - `cpp_diagnostics`:

    Diagnostic information regarding columns per page.

  - `col_diagnostics`:

    Very basic information about leaf columns, whether pagination was
    attempted after each leaf column, ad the final result of such
    attempts, if made.

## Details

`paginate_indices` renders `obj` into a `MatrixPrintForm` (MPF), then
uses that representation to calculate the rows and columns of `obj`
corresponding to each page of the pagination of `obj`, but simply
returns these indices rather than paginating `obj` itself (see Details
for an important caveat).

`paginate_to_mpfs` renders `obj` into its MPF intermediate
representation, then paginates that MPF into component MPFs each
corresponding to an individual page and returns those in a `list`.

`diagnose_pagination` attempts pagination via `paginate_to_mpfs`, then
returns diagnostic information which explains why page breaks were
positioned where they were, or alternatively why no valid pagination
could be found.

All three of these functions generally support all classes which have a
corresponding
[`matrix_form()`](https://insightsengineering.github.io/formatters/reference/matrix_form.md)
method which returns a valid `MatrixPrintForm` object (including
`MatrixPrintForm` objects themselves).

`paginate_indices` is directly called by `paginate_to_mpfs` (and thus
`diagnose_pagination`). For most classes, and most tables represented by
supported classes, calling `paginate_to_mpfs` is equivalent to a manual
`paginate_indices -> subset obj into pages -> matrix_form` workflow.

The exception to this equivalence is objects which support "forced
pagination", or pagination logic which is built into the object itself
rather than being a function of space on a page. Forced pagination
generally involves the creation of, e.g., page-specific titles which
apply to these forced paginations. `paginate_to_mpfs` and
`diagnose_pagination` support forced pagination by automatically calling
the
[`do_forced_paginate()`](https://insightsengineering.github.io/formatters/reference/do_forced_paginate.md)
generic on the object and then paginating each object returned by that
generic separately. The assumption here, then, is that page-specific
titles and such are handled by the class'
[`do_forced_paginate()`](https://insightsengineering.github.io/formatters/reference/do_forced_paginate.md)
method.

`paginate_indices`, on the other hand, *does not support forced
pagination*, because it returns only a set of indices for row and column
subsetting for each page, and thus cannot retain any changes, e.g., to
titles, done within
[`do_forced_paginate()`](https://insightsengineering.github.io/formatters/reference/do_forced_paginate.md).
`paginate_indices` does call
[`do_forced_paginate()`](https://insightsengineering.github.io/formatters/reference/do_forced_paginate.md),
but instead of continuing it throws an error in the case that the result
is larger than a single "page".

`diagnose_pagination` attempts pagination and then, regardless of
success or failure, returns diagnostic information about pagination
attempts (if any) after each row and column.

The diagnostics data reflects the final time the pagination algorithm
evaluated a page break at the specified location, regardless of how many
times the position was assessed in total.

To get information about intermediate attempts, perform pagination with
`verbose = TRUE` and inspect the messages in order.

## Note

For `diagnose_pagination`, the column labels are not displayed in the
`col_diagnostics` element due to certain internal implementation
details; rather the diagnostics are reported in terms of absolute (leaf)
column position. This is a known limitation, and may eventually be
changed, but the information remains useful as it is currently reported.

`diagnose_pagination` is intended for interactive debugging use and
*should not be programmed against*, as the exact content and form of the
verbose messages it captures and returns is subject to change.

Because `diagnose_pagination` relies on
`capture.output(type = "message")`, it cannot be used within the
`testthat` (and likely other) testing frameworks, and likely cannot be
used within `knitr`/`rmarkdown` contexts either, as this clashes with
those systems' capture of messages.

## Examples

``` r
mpf <- basic_matrix_form(mtcars)

paginate_indices(mpf, pg_width = 5, pg_height = 3)
#> $pag_row_indices
#> $pag_row_indices[[1]]
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
#> 
#> $pag_row_indices[[2]]
#>  [1] 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
#> 
#> 
#> $pag_col_indices
#> $pag_col_indices[[1]]
#> [1] 1 2 3 4
#> 
#> $pag_col_indices[[2]]
#> [1] 5 6 7 8 9
#> 
#> $pag_col_indices[[3]]
#> [1] 10 11
#> 
#> 

paginate_to_mpfs(mpf, pg_width = 5, pg_height = 3)
#> [[1]]
#> $strings
#>       rnms                mpg    cyl   disp    hp   
#>  [1,] ""                  "mpg"  "cyl" "disp"  "hp" 
#>  [2,] "Mazda RX4"         "21"   "6"   "160"   "110"
#>  [3,] "Mazda RX4 Wag"     "21"   "6"   "160"   "110"
#>  [4,] "Datsun 710"        "22.8" "4"   "108"   "93" 
#>  [5,] "Hornet 4 Drive"    "21.4" "6"   "258"   "110"
#>  [6,] "Hornet Sportabout" "18.7" "8"   "360"   "175"
#>  [7,] "Valiant"           "18.1" "6"   "225"   "105"
#>  [8,] "Duster 360"        "14.3" "8"   "360"   "245"
#>  [9,] "Merc 240D"         "24.4" "4"   "146.7" "62" 
#> [10,] "Merc 230"          "22.8" "4"   "140.8" "95" 
#> [11,] "Merc 280"          "19.2" "6"   "167.6" "123"
#> [12,] "Merc 280C"         "17.8" "6"   "167.6" "123"
#> [13,] "Merc 450SE"        "16.4" "8"   "275.8" "180"
#> [14,] "Merc 450SL"        "17.3" "8"   "275.8" "180"
#> 
#> $spans
#>       [,1] [,2] [,3] [,4] [,5]
#>  [1,]    1    1    1    1    1
#>  [2,]    1    1    1    1    1
#>  [3,]    1    1    1    1    1
#>  [4,]    1    1    1    1    1
#>  [5,]    1    1    1    1    1
#>  [6,]    1    1    1    1    1
#>  [7,]    1    1    1    1    1
#>  [8,]    1    1    1    1    1
#>  [9,]    1    1    1    1    1
#> [10,]    1    1    1    1    1
#> [11,]    1    1    1    1    1
#> [12,]    1    1    1    1    1
#> [13,]    1    1    1    1    1
#> [14,]    1    1    1    1    1
#> 
#> $aligns
#>       [,1]   [,2]     [,3]     [,4]     [,5]    
#>  [1,] "left" "center" "center" "center" "center"
#>  [2,] "left" "center" "center" "center" "center"
#>  [3,] "left" "center" "center" "center" "center"
#>  [4,] "left" "center" "center" "center" "center"
#>  [5,] "left" "center" "center" "center" "center"
#>  [6,] "left" "center" "center" "center" "center"
#>  [7,] "left" "center" "center" "center" "center"
#>  [8,] "left" "center" "center" "center" "center"
#>  [9,] "left" "center" "center" "center" "center"
#> [10,] "left" "center" "center" "center" "center"
#> [11,] "left" "center" "center" "center" "center"
#> [12,] "left" "center" "center" "center" "center"
#> [13,] "left" "center" "center" "center" "center"
#> [14,] "left" "center" "center" "center" "center"
#> 
#> $display
#>       [,1] [,2] [,3] [,4] [,5]
#>  [1,] TRUE TRUE TRUE TRUE TRUE
#>  [2,] TRUE TRUE TRUE TRUE TRUE
#>  [3,] TRUE TRUE TRUE TRUE TRUE
#>  [4,] TRUE TRUE TRUE TRUE TRUE
#>  [5,] TRUE TRUE TRUE TRUE TRUE
#>  [6,] TRUE TRUE TRUE TRUE TRUE
#>  [7,] TRUE TRUE TRUE TRUE TRUE
#>  [8,] TRUE TRUE TRUE TRUE TRUE
#>  [9,] TRUE TRUE TRUE TRUE TRUE
#> [10,] TRUE TRUE TRUE TRUE TRUE
#> [11,] TRUE TRUE TRUE TRUE TRUE
#> [12,] TRUE TRUE TRUE TRUE TRUE
#> [13,] TRUE TRUE TRUE TRUE TRUE
#> [14,] TRUE TRUE TRUE TRUE TRUE
#> 
#> $formats
#>    rnms mpg cyl disp hp
#> 1                      
#> 2        xx  xx   xx xx
#> 3        xx  xx   xx xx
#> 4        xx  xx   xx xx
#> 5        xx  xx   xx xx
#> 6        xx  xx   xx xx
#> 7        xx  xx   xx xx
#> 8        xx  xx   xx xx
#> 9        xx  xx   xx xx
#> 10       xx  xx   xx xx
#> 11       xx  xx   xx xx
#> 12       xx  xx   xx xx
#> 13       xx  xx   xx xx
#> 14       xx  xx   xx xx
#> 
#> $row_info
#>                               label              name abs_rownumber
#> Mazda RX4                 Mazda RX4         Mazda RX4             1
#> Mazda RX4 Wag         Mazda RX4 Wag     Mazda RX4 Wag             2
#> Datsun 710               Datsun 710        Datsun 710             3
#> Hornet 4 Drive       Hornet 4 Drive    Hornet 4 Drive             4
#> Hornet Sportabout Hornet Sportabout Hornet Sportabout             5
#> Valiant                     Valiant           Valiant             6
#> Duster 360               Duster 360        Duster 360             7
#> Merc 240D                 Merc 240D         Merc 240D             8
#> Merc 230                   Merc 230          Merc 230             9
#> Merc 280                   Merc 280          Merc 280            10
#> Merc 280C                 Merc 280C         Merc 280C            11
#> Merc 450SE               Merc 450SE        Merc 450SE            12
#> Merc 450SL               Merc 450SL        Merc 450SL            13
#>                           path pos_in_siblings n_siblings self_extent
#> Mazda RX4            Mazda RX4               1         32           1
#> Mazda RX4 Wag     Mazda RX....               2         32           1
#> Datsun 710          Datsun 710               3         32           1
#> Hornet 4 Drive    Hornet 4....               4         32           1
#> Hornet Sportabout Hornet S....               5         32           1
#> Valiant                Valiant               6         32           1
#> Duster 360          Duster 360               7         32           1
#> Merc 240D            Merc 240D               8         32           1
#> Merc 230              Merc 230               9         32           1
#> Merc 280              Merc 280              10         32           1
#> Merc 280C            Merc 280C              11         32           1
#> Merc 450SE          Merc 450SE              12         32           1
#> Merc 450SL          Merc 450SL              13         32           1
#>                   par_extent reprint_inds node_class indent nrowrefs ncellrefs
#> Mazda RX4                  0                 DataRow      0        0         0
#> Mazda RX4 Wag              0                 DataRow      0        0         0
#> Datsun 710                 0                 DataRow      0        0         0
#> Hornet 4 Drive             0                 DataRow      0        0         0
#> Hornet Sportabout          0                 DataRow      0        0         0
#> Valiant                    0                 DataRow      0        0         0
#> Duster 360                 0                 DataRow      0        0         0
#> Merc 240D                  0                 DataRow      0        0         0
#> Merc 230                   0                 DataRow      0        0         0
#> Merc 280                   0                 DataRow      0        0         0
#> Merc 280C                  0                 DataRow      0        0         0
#> Merc 450SE                 0                 DataRow      0        0         0
#> Merc 450SL                 0                 DataRow      0        0         0
#>                   nreflines force_page page_title trailing_sep ref_info_df
#> Mazda RX4                 0      FALSE       <NA>         <NA>            
#> Mazda RX4 Wag             0      FALSE       <NA>         <NA>            
#> Datsun 710                0      FALSE       <NA>         <NA>            
#> Hornet 4 Drive            0      FALSE       <NA>         <NA>            
#> Hornet Sportabout         0      FALSE       <NA>         <NA>            
#> Valiant                   0      FALSE       <NA>         <NA>            
#> Duster 360                0      FALSE       <NA>         <NA>            
#> Merc 240D                 0      FALSE       <NA>         <NA>            
#> Merc 230                  0      FALSE       <NA>         <NA>            
#> Merc 280                  0      FALSE       <NA>         <NA>            
#> Merc 280C                 0      FALSE       <NA>         <NA>            
#> Merc 450SE                0      FALSE       <NA>         <NA>            
#> Merc 450SL                0      FALSE       <NA>         <NA>            
#> 
#> $line_grouping
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14
#> 
#> $main_title
#> [1] ""
#> 
#> $subtitles
#> character(0)
#> 
#> $page_titles
#> character(0)
#> 
#> $main_footer
#> [1] ""
#> 
#> $prov_footer
#> [1] "                                         page 1 of 9"
#> 
#> $header_section_div
#> [1] NA
#> 
#> $horizontal_sep
#> [1] "o"
#> 
#> $col_gap
#> [1] 3
#> 
#> $listing_keycols
#> NULL
#> 
#> $table_inset
#> [1] 0
#> 
#> $has_topleft
#> [1] FALSE
#> 
#> $indent_size
#> [1] 2
#> 
#> $col_widths
#> rnms  mpg  cyl disp   hp 
#>   19    4    3    5    3 
#> 
#> $fontspec
#> $family
#> [1] "Courier"
#> 
#> $size
#> [1] 8
#> 
#> $lineheight
#> [1] 1
#> 
#> attr(,"class")
#> [1] "font_spec" "list"     
#> 
#> $num_rep_cols
#> [1] 0
#> 
#> $round_type
#> [1] "iec"
#> 
#> $ref_fnote_df
#> [1] row_path  col_path  row       col       symbol    ref_index msg      
#> [8] nlines   
#> <0 rows> (or 0-length row.names)
#> 
#> $col_info
#>   label name abs_rownumber path pos_in_siblings n_siblings self_extent
#> 1     1    1             1   NA               0          0           4
#> 2     2    2             2   NA               0          0           3
#> 3     3    3             3   NA               0          0           5
#> 4     4    4             4   NA               0          0           3
#>   par_extent reprint_inds node_class indent nrowrefs ncellrefs nreflines
#> 1          0                   stuff      0        0         0         0
#> 2          0                   stuff      0        0         0         0
#> 3          0                   stuff      0        0         0         0
#> 4          0                   stuff      0        0         0         0
#>   force_page page_title trailing_sep ref_info_df
#> 1      FALSE       <NA>         <NA>            
#> 2      FALSE       <NA>         <NA>            
#> 3      FALSE       <NA>         <NA>            
#> 4      FALSE       <NA>         <NA>            
#> 
#> attr(,"nrow_header")
#> [1] 1
#> attr(,"ncols")
#> [1] 4
#> attr(,"class")
#> [1] "MatrixPrintForm" "list"           
#> 
#> [[2]]
#> $strings
#>       rnms                drat   wt      qsec    vs   am  
#>  [1,] ""                  "drat" "wt"    "qsec"  "vs" "am"
#>  [2,] "Mazda RX4"         "3.9"  "2.62"  "16.46" "0"  "1" 
#>  [3,] "Mazda RX4 Wag"     "3.9"  "2.875" "17.02" "0"  "1" 
#>  [4,] "Datsun 710"        "3.85" "2.32"  "18.61" "1"  "1" 
#>  [5,] "Hornet 4 Drive"    "3.08" "3.215" "19.44" "1"  "0" 
#>  [6,] "Hornet Sportabout" "3.15" "3.44"  "17.02" "0"  "0" 
#>  [7,] "Valiant"           "2.76" "3.46"  "20.22" "1"  "0" 
#>  [8,] "Duster 360"        "3.21" "3.57"  "15.84" "0"  "0" 
#>  [9,] "Merc 240D"         "3.69" "3.19"  "20"    "1"  "0" 
#> [10,] "Merc 230"          "3.92" "3.15"  "22.9"  "1"  "0" 
#> [11,] "Merc 280"          "3.92" "3.44"  "18.3"  "1"  "0" 
#> [12,] "Merc 280C"         "3.92" "3.44"  "18.9"  "1"  "0" 
#> [13,] "Merc 450SE"        "3.07" "4.07"  "17.4"  "0"  "0" 
#> [14,] "Merc 450SL"        "3.07" "3.73"  "17.6"  "0"  "0" 
#> 
#> $spans
#>       [,1] [,2] [,3] [,4] [,5] [,6]
#>  [1,]    1    1    1    1    1    1
#>  [2,]    1    1    1    1    1    1
#>  [3,]    1    1    1    1    1    1
#>  [4,]    1    1    1    1    1    1
#>  [5,]    1    1    1    1    1    1
#>  [6,]    1    1    1    1    1    1
#>  [7,]    1    1    1    1    1    1
#>  [8,]    1    1    1    1    1    1
#>  [9,]    1    1    1    1    1    1
#> [10,]    1    1    1    1    1    1
#> [11,]    1    1    1    1    1    1
#> [12,]    1    1    1    1    1    1
#> [13,]    1    1    1    1    1    1
#> [14,]    1    1    1    1    1    1
#> 
#> $aligns
#>       [,1]   [,2]     [,3]     [,4]     [,5]     [,6]    
#>  [1,] "left" "center" "center" "center" "center" "center"
#>  [2,] "left" "center" "center" "center" "center" "center"
#>  [3,] "left" "center" "center" "center" "center" "center"
#>  [4,] "left" "center" "center" "center" "center" "center"
#>  [5,] "left" "center" "center" "center" "center" "center"
#>  [6,] "left" "center" "center" "center" "center" "center"
#>  [7,] "left" "center" "center" "center" "center" "center"
#>  [8,] "left" "center" "center" "center" "center" "center"
#>  [9,] "left" "center" "center" "center" "center" "center"
#> [10,] "left" "center" "center" "center" "center" "center"
#> [11,] "left" "center" "center" "center" "center" "center"
#> [12,] "left" "center" "center" "center" "center" "center"
#> [13,] "left" "center" "center" "center" "center" "center"
#> [14,] "left" "center" "center" "center" "center" "center"
#> 
#> $display
#>       [,1] [,2] [,3] [,4] [,5] [,6]
#>  [1,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [2,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [3,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [4,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [5,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [6,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [7,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [8,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [9,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [10,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [11,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [12,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [13,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [14,] TRUE TRUE TRUE TRUE TRUE TRUE
#> 
#> $formats
#>    rnms drat wt qsec vs am
#> 1                         
#> 2         xx xx   xx xx xx
#> 3         xx xx   xx xx xx
#> 4         xx xx   xx xx xx
#> 5         xx xx   xx xx xx
#> 6         xx xx   xx xx xx
#> 7         xx xx   xx xx xx
#> 8         xx xx   xx xx xx
#> 9         xx xx   xx xx xx
#> 10        xx xx   xx xx xx
#> 11        xx xx   xx xx xx
#> 12        xx xx   xx xx xx
#> 13        xx xx   xx xx xx
#> 14        xx xx   xx xx xx
#> 
#> $row_info
#>                               label              name abs_rownumber
#> Mazda RX4                 Mazda RX4         Mazda RX4             1
#> Mazda RX4 Wag         Mazda RX4 Wag     Mazda RX4 Wag             2
#> Datsun 710               Datsun 710        Datsun 710             3
#> Hornet 4 Drive       Hornet 4 Drive    Hornet 4 Drive             4
#> Hornet Sportabout Hornet Sportabout Hornet Sportabout             5
#> Valiant                     Valiant           Valiant             6
#> Duster 360               Duster 360        Duster 360             7
#> Merc 240D                 Merc 240D         Merc 240D             8
#> Merc 230                   Merc 230          Merc 230             9
#> Merc 280                   Merc 280          Merc 280            10
#> Merc 280C                 Merc 280C         Merc 280C            11
#> Merc 450SE               Merc 450SE        Merc 450SE            12
#> Merc 450SL               Merc 450SL        Merc 450SL            13
#>                           path pos_in_siblings n_siblings self_extent
#> Mazda RX4            Mazda RX4               1         32           1
#> Mazda RX4 Wag     Mazda RX....               2         32           1
#> Datsun 710          Datsun 710               3         32           1
#> Hornet 4 Drive    Hornet 4....               4         32           1
#> Hornet Sportabout Hornet S....               5         32           1
#> Valiant                Valiant               6         32           1
#> Duster 360          Duster 360               7         32           1
#> Merc 240D            Merc 240D               8         32           1
#> Merc 230              Merc 230               9         32           1
#> Merc 280              Merc 280              10         32           1
#> Merc 280C            Merc 280C              11         32           1
#> Merc 450SE          Merc 450SE              12         32           1
#> Merc 450SL          Merc 450SL              13         32           1
#>                   par_extent reprint_inds node_class indent nrowrefs ncellrefs
#> Mazda RX4                  0                 DataRow      0        0         0
#> Mazda RX4 Wag              0                 DataRow      0        0         0
#> Datsun 710                 0                 DataRow      0        0         0
#> Hornet 4 Drive             0                 DataRow      0        0         0
#> Hornet Sportabout          0                 DataRow      0        0         0
#> Valiant                    0                 DataRow      0        0         0
#> Duster 360                 0                 DataRow      0        0         0
#> Merc 240D                  0                 DataRow      0        0         0
#> Merc 230                   0                 DataRow      0        0         0
#> Merc 280                   0                 DataRow      0        0         0
#> Merc 280C                  0                 DataRow      0        0         0
#> Merc 450SE                 0                 DataRow      0        0         0
#> Merc 450SL                 0                 DataRow      0        0         0
#>                   nreflines force_page page_title trailing_sep ref_info_df
#> Mazda RX4                 0      FALSE       <NA>         <NA>            
#> Mazda RX4 Wag             0      FALSE       <NA>         <NA>            
#> Datsun 710                0      FALSE       <NA>         <NA>            
#> Hornet 4 Drive            0      FALSE       <NA>         <NA>            
#> Hornet Sportabout         0      FALSE       <NA>         <NA>            
#> Valiant                   0      FALSE       <NA>         <NA>            
#> Duster 360                0      FALSE       <NA>         <NA>            
#> Merc 240D                 0      FALSE       <NA>         <NA>            
#> Merc 230                  0      FALSE       <NA>         <NA>            
#> Merc 280                  0      FALSE       <NA>         <NA>            
#> Merc 280C                 0      FALSE       <NA>         <NA>            
#> Merc 450SE                0      FALSE       <NA>         <NA>            
#> Merc 450SL                0      FALSE       <NA>         <NA>            
#> 
#> $line_grouping
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14
#> 
#> $main_title
#> [1] ""
#> 
#> $subtitles
#> character(0)
#> 
#> $page_titles
#> character(0)
#> 
#> $main_footer
#> [1] ""
#> 
#> $prov_footer
#> [1] "                                         page 2 of 9"
#> 
#> $header_section_div
#> [1] NA
#> 
#> $horizontal_sep
#> [1] "o"
#> 
#> $col_gap
#> [1] 3
#> 
#> $listing_keycols
#> NULL
#> 
#> $table_inset
#> [1] 0
#> 
#> $has_topleft
#> [1] FALSE
#> 
#> $indent_size
#> [1] 2
#> 
#> $col_widths
#> rnms drat   wt qsec   vs   am 
#>   19    4    5    5    2    2 
#> 
#> $fontspec
#> $family
#> [1] "Courier"
#> 
#> $size
#> [1] 8
#> 
#> $lineheight
#> [1] 1
#> 
#> attr(,"class")
#> [1] "font_spec" "list"     
#> 
#> $num_rep_cols
#> [1] 0
#> 
#> $round_type
#> [1] "iec"
#> 
#> $ref_fnote_df
#> [1] row_path  col_path  row       col       symbol    ref_index msg      
#> [8] nlines   
#> <0 rows> (or 0-length row.names)
#> 
#> $col_info
#>   label name abs_rownumber path pos_in_siblings n_siblings self_extent
#> 5     5    5             5   NA               0          0           4
#> 6     6    6             6   NA               0          0           5
#> 7     7    7             7   NA               0          0           5
#> 8     8    8             8   NA               0          0           2
#> 9     9    9             9   NA               0          0           2
#>   par_extent reprint_inds node_class indent nrowrefs ncellrefs nreflines
#> 5          0                   stuff      0        0         0         0
#> 6          0                   stuff      0        0         0         0
#> 7          0                   stuff      0        0         0         0
#> 8          0                   stuff      0        0         0         0
#> 9          0                   stuff      0        0         0         0
#>   force_page page_title trailing_sep ref_info_df
#> 5      FALSE       <NA>         <NA>            
#> 6      FALSE       <NA>         <NA>            
#> 7      FALSE       <NA>         <NA>            
#> 8      FALSE       <NA>         <NA>            
#> 9      FALSE       <NA>         <NA>            
#> 
#> attr(,"nrow_header")
#> [1] 1
#> attr(,"ncols")
#> [1] 5
#> attr(,"class")
#> [1] "MatrixPrintForm" "list"           
#> 
#> [[3]]
#> $strings
#>       rnms                gear   carb  
#>  [1,] ""                  "gear" "carb"
#>  [2,] "Mazda RX4"         "4"    "4"   
#>  [3,] "Mazda RX4 Wag"     "4"    "4"   
#>  [4,] "Datsun 710"        "4"    "1"   
#>  [5,] "Hornet 4 Drive"    "3"    "1"   
#>  [6,] "Hornet Sportabout" "3"    "2"   
#>  [7,] "Valiant"           "3"    "1"   
#>  [8,] "Duster 360"        "3"    "4"   
#>  [9,] "Merc 240D"         "4"    "2"   
#> [10,] "Merc 230"          "4"    "2"   
#> [11,] "Merc 280"          "4"    "4"   
#> [12,] "Merc 280C"         "4"    "4"   
#> [13,] "Merc 450SE"        "3"    "3"   
#> [14,] "Merc 450SL"        "3"    "3"   
#> 
#> $spans
#>       [,1] [,2] [,3]
#>  [1,]    1    1    1
#>  [2,]    1    1    1
#>  [3,]    1    1    1
#>  [4,]    1    1    1
#>  [5,]    1    1    1
#>  [6,]    1    1    1
#>  [7,]    1    1    1
#>  [8,]    1    1    1
#>  [9,]    1    1    1
#> [10,]    1    1    1
#> [11,]    1    1    1
#> [12,]    1    1    1
#> [13,]    1    1    1
#> [14,]    1    1    1
#> 
#> $aligns
#>       [,1]   [,2]     [,3]    
#>  [1,] "left" "center" "center"
#>  [2,] "left" "center" "center"
#>  [3,] "left" "center" "center"
#>  [4,] "left" "center" "center"
#>  [5,] "left" "center" "center"
#>  [6,] "left" "center" "center"
#>  [7,] "left" "center" "center"
#>  [8,] "left" "center" "center"
#>  [9,] "left" "center" "center"
#> [10,] "left" "center" "center"
#> [11,] "left" "center" "center"
#> [12,] "left" "center" "center"
#> [13,] "left" "center" "center"
#> [14,] "left" "center" "center"
#> 
#> $display
#>       [,1] [,2] [,3]
#>  [1,] TRUE TRUE TRUE
#>  [2,] TRUE TRUE TRUE
#>  [3,] TRUE TRUE TRUE
#>  [4,] TRUE TRUE TRUE
#>  [5,] TRUE TRUE TRUE
#>  [6,] TRUE TRUE TRUE
#>  [7,] TRUE TRUE TRUE
#>  [8,] TRUE TRUE TRUE
#>  [9,] TRUE TRUE TRUE
#> [10,] TRUE TRUE TRUE
#> [11,] TRUE TRUE TRUE
#> [12,] TRUE TRUE TRUE
#> [13,] TRUE TRUE TRUE
#> [14,] TRUE TRUE TRUE
#> 
#> $formats
#>    rnms gear carb
#> 1                
#> 2         xx   xx
#> 3         xx   xx
#> 4         xx   xx
#> 5         xx   xx
#> 6         xx   xx
#> 7         xx   xx
#> 8         xx   xx
#> 9         xx   xx
#> 10        xx   xx
#> 11        xx   xx
#> 12        xx   xx
#> 13        xx   xx
#> 14        xx   xx
#> 
#> $row_info
#>                               label              name abs_rownumber
#> Mazda RX4                 Mazda RX4         Mazda RX4             1
#> Mazda RX4 Wag         Mazda RX4 Wag     Mazda RX4 Wag             2
#> Datsun 710               Datsun 710        Datsun 710             3
#> Hornet 4 Drive       Hornet 4 Drive    Hornet 4 Drive             4
#> Hornet Sportabout Hornet Sportabout Hornet Sportabout             5
#> Valiant                     Valiant           Valiant             6
#> Duster 360               Duster 360        Duster 360             7
#> Merc 240D                 Merc 240D         Merc 240D             8
#> Merc 230                   Merc 230          Merc 230             9
#> Merc 280                   Merc 280          Merc 280            10
#> Merc 280C                 Merc 280C         Merc 280C            11
#> Merc 450SE               Merc 450SE        Merc 450SE            12
#> Merc 450SL               Merc 450SL        Merc 450SL            13
#>                           path pos_in_siblings n_siblings self_extent
#> Mazda RX4            Mazda RX4               1         32           1
#> Mazda RX4 Wag     Mazda RX....               2         32           1
#> Datsun 710          Datsun 710               3         32           1
#> Hornet 4 Drive    Hornet 4....               4         32           1
#> Hornet Sportabout Hornet S....               5         32           1
#> Valiant                Valiant               6         32           1
#> Duster 360          Duster 360               7         32           1
#> Merc 240D            Merc 240D               8         32           1
#> Merc 230              Merc 230               9         32           1
#> Merc 280              Merc 280              10         32           1
#> Merc 280C            Merc 280C              11         32           1
#> Merc 450SE          Merc 450SE              12         32           1
#> Merc 450SL          Merc 450SL              13         32           1
#>                   par_extent reprint_inds node_class indent nrowrefs ncellrefs
#> Mazda RX4                  0                 DataRow      0        0         0
#> Mazda RX4 Wag              0                 DataRow      0        0         0
#> Datsun 710                 0                 DataRow      0        0         0
#> Hornet 4 Drive             0                 DataRow      0        0         0
#> Hornet Sportabout          0                 DataRow      0        0         0
#> Valiant                    0                 DataRow      0        0         0
#> Duster 360                 0                 DataRow      0        0         0
#> Merc 240D                  0                 DataRow      0        0         0
#> Merc 230                   0                 DataRow      0        0         0
#> Merc 280                   0                 DataRow      0        0         0
#> Merc 280C                  0                 DataRow      0        0         0
#> Merc 450SE                 0                 DataRow      0        0         0
#> Merc 450SL                 0                 DataRow      0        0         0
#>                   nreflines force_page page_title trailing_sep ref_info_df
#> Mazda RX4                 0      FALSE       <NA>         <NA>            
#> Mazda RX4 Wag             0      FALSE       <NA>         <NA>            
#> Datsun 710                0      FALSE       <NA>         <NA>            
#> Hornet 4 Drive            0      FALSE       <NA>         <NA>            
#> Hornet Sportabout         0      FALSE       <NA>         <NA>            
#> Valiant                   0      FALSE       <NA>         <NA>            
#> Duster 360                0      FALSE       <NA>         <NA>            
#> Merc 240D                 0      FALSE       <NA>         <NA>            
#> Merc 230                  0      FALSE       <NA>         <NA>            
#> Merc 280                  0      FALSE       <NA>         <NA>            
#> Merc 280C                 0      FALSE       <NA>         <NA>            
#> Merc 450SE                0      FALSE       <NA>         <NA>            
#> Merc 450SL                0      FALSE       <NA>         <NA>            
#> 
#> $line_grouping
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14
#> 
#> $main_title
#> [1] ""
#> 
#> $subtitles
#> character(0)
#> 
#> $page_titles
#> character(0)
#> 
#> $main_footer
#> [1] ""
#> 
#> $prov_footer
#> [1] "                                         page 3 of 9"
#> 
#> $header_section_div
#> [1] NA
#> 
#> $horizontal_sep
#> [1] "o"
#> 
#> $col_gap
#> [1] 3
#> 
#> $listing_keycols
#> NULL
#> 
#> $table_inset
#> [1] 0
#> 
#> $has_topleft
#> [1] FALSE
#> 
#> $indent_size
#> [1] 2
#> 
#> $col_widths
#> rnms gear carb 
#>   19    4    4 
#> 
#> $fontspec
#> $family
#> [1] "Courier"
#> 
#> $size
#> [1] 8
#> 
#> $lineheight
#> [1] 1
#> 
#> attr(,"class")
#> [1] "font_spec" "list"     
#> 
#> $num_rep_cols
#> [1] 0
#> 
#> $round_type
#> [1] "iec"
#> 
#> $ref_fnote_df
#> [1] row_path  col_path  row       col       symbol    ref_index msg      
#> [8] nlines   
#> <0 rows> (or 0-length row.names)
#> 
#> $col_info
#>    label name abs_rownumber path pos_in_siblings n_siblings self_extent
#> 10    10   10            10   NA               0          0           4
#> 11    11   11            11   NA               0          0           4
#>    par_extent reprint_inds node_class indent nrowrefs ncellrefs nreflines
#> 10          0                   stuff      0        0         0         0
#> 11          0                   stuff      0        0         0         0
#>    force_page page_title trailing_sep ref_info_df
#> 10      FALSE       <NA>         <NA>            
#> 11      FALSE       <NA>         <NA>            
#> 
#> attr(,"nrow_header")
#> [1] 1
#> attr(,"ncols")
#> [1] 2
#> attr(,"class")
#> [1] "MatrixPrintForm" "list"           
#> 
#> [[4]]
#> $strings
#>       rnms                  mpg    cyl   disp    hp   
#>  [1,] ""                    "mpg"  "cyl" "disp"  "hp" 
#>  [2,] "Merc 450SLC"         "15.2" "8"   "275.8" "180"
#>  [3,] "Cadillac Fleetwood"  "10.4" "8"   "472"   "205"
#>  [4,] "Lincoln Continental" "10.4" "8"   "460"   "215"
#>  [5,] "Chrysler Imperial"   "14.7" "8"   "440"   "230"
#>  [6,] "Fiat 128"            "32.4" "4"   "78.7"  "66" 
#>  [7,] "Honda Civic"         "30.4" "4"   "75.7"  "52" 
#>  [8,] "Toyota Corolla"      "33.9" "4"   "71.1"  "65" 
#>  [9,] "Toyota Corona"       "21.5" "4"   "120.1" "97" 
#> [10,] "Dodge Challenger"    "15.5" "8"   "318"   "150"
#> [11,] "AMC Javelin"         "15.2" "8"   "304"   "150"
#> [12,] "Camaro Z28"          "13.3" "8"   "350"   "245"
#> [13,] "Pontiac Firebird"    "19.2" "8"   "400"   "175"
#> [14,] "Fiat X1-9"           "27.3" "4"   "79"    "66" 
#> 
#> $spans
#>       [,1] [,2] [,3] [,4] [,5]
#>  [1,]    1    1    1    1    1
#>  [2,]    1    1    1    1    1
#>  [3,]    1    1    1    1    1
#>  [4,]    1    1    1    1    1
#>  [5,]    1    1    1    1    1
#>  [6,]    1    1    1    1    1
#>  [7,]    1    1    1    1    1
#>  [8,]    1    1    1    1    1
#>  [9,]    1    1    1    1    1
#> [10,]    1    1    1    1    1
#> [11,]    1    1    1    1    1
#> [12,]    1    1    1    1    1
#> [13,]    1    1    1    1    1
#> [14,]    1    1    1    1    1
#> 
#> $aligns
#>       [,1]   [,2]     [,3]     [,4]     [,5]    
#>  [1,] "left" "center" "center" "center" "center"
#>  [2,] "left" "center" "center" "center" "center"
#>  [3,] "left" "center" "center" "center" "center"
#>  [4,] "left" "center" "center" "center" "center"
#>  [5,] "left" "center" "center" "center" "center"
#>  [6,] "left" "center" "center" "center" "center"
#>  [7,] "left" "center" "center" "center" "center"
#>  [8,] "left" "center" "center" "center" "center"
#>  [9,] "left" "center" "center" "center" "center"
#> [10,] "left" "center" "center" "center" "center"
#> [11,] "left" "center" "center" "center" "center"
#> [12,] "left" "center" "center" "center" "center"
#> [13,] "left" "center" "center" "center" "center"
#> [14,] "left" "center" "center" "center" "center"
#> 
#> $display
#>       [,1] [,2] [,3] [,4] [,5]
#>  [1,] TRUE TRUE TRUE TRUE TRUE
#>  [2,] TRUE TRUE TRUE TRUE TRUE
#>  [3,] TRUE TRUE TRUE TRUE TRUE
#>  [4,] TRUE TRUE TRUE TRUE TRUE
#>  [5,] TRUE TRUE TRUE TRUE TRUE
#>  [6,] TRUE TRUE TRUE TRUE TRUE
#>  [7,] TRUE TRUE TRUE TRUE TRUE
#>  [8,] TRUE TRUE TRUE TRUE TRUE
#>  [9,] TRUE TRUE TRUE TRUE TRUE
#> [10,] TRUE TRUE TRUE TRUE TRUE
#> [11,] TRUE TRUE TRUE TRUE TRUE
#> [12,] TRUE TRUE TRUE TRUE TRUE
#> [13,] TRUE TRUE TRUE TRUE TRUE
#> [14,] TRUE TRUE TRUE TRUE TRUE
#> 
#> $formats
#>    rnms mpg cyl disp hp
#> 1                      
#> 15       xx  xx   xx xx
#> 16       xx  xx   xx xx
#> 17       xx  xx   xx xx
#> 18       xx  xx   xx xx
#> 19       xx  xx   xx xx
#> 20       xx  xx   xx xx
#> 21       xx  xx   xx xx
#> 22       xx  xx   xx xx
#> 23       xx  xx   xx xx
#> 24       xx  xx   xx xx
#> 25       xx  xx   xx xx
#> 26       xx  xx   xx xx
#> 27       xx  xx   xx xx
#> 
#> $row_info
#>                                   label                name abs_rownumber
#> Merc 450SLC                 Merc 450SLC         Merc 450SLC             1
#> Cadillac Fleetwood   Cadillac Fleetwood  Cadillac Fleetwood             2
#> Lincoln Continental Lincoln Continental Lincoln Continental             3
#> Chrysler Imperial     Chrysler Imperial   Chrysler Imperial             4
#> Fiat 128                       Fiat 128            Fiat 128             5
#> Honda Civic                 Honda Civic         Honda Civic             6
#> Toyota Corolla           Toyota Corolla      Toyota Corolla             7
#> Toyota Corona             Toyota Corona       Toyota Corona             8
#> Dodge Challenger       Dodge Challenger    Dodge Challenger             9
#> AMC Javelin                 AMC Javelin         AMC Javelin            10
#> Camaro Z28                   Camaro Z28          Camaro Z28            11
#> Pontiac Firebird       Pontiac Firebird    Pontiac Firebird            12
#> Fiat X1-9                     Fiat X1-9           Fiat X1-9            13
#>                             path pos_in_siblings n_siblings self_extent
#> Merc 450SLC          Merc 450SLC              14         32           1
#> Cadillac Fleetwood  Cadillac....              15         32           1
#> Lincoln Continental Lincoln ....              16         32           1
#> Chrysler Imperial   Chrysler....              17         32           1
#> Fiat 128                Fiat 128              18         32           1
#> Honda Civic          Honda Civic              19         32           1
#> Toyota Corolla      Toyota C....              20         32           1
#> Toyota Corona       Toyota C....              21         32           1
#> Dodge Challenger    Dodge Ch....              22         32           1
#> AMC Javelin          AMC Javelin              23         32           1
#> Camaro Z28            Camaro Z28              24         32           1
#> Pontiac Firebird    Pontiac ....              25         32           1
#> Fiat X1-9              Fiat X1-9              26         32           1
#>                     par_extent reprint_inds node_class indent nrowrefs
#> Merc 450SLC                  0                 DataRow      0        0
#> Cadillac Fleetwood           0                 DataRow      0        0
#> Lincoln Continental          0                 DataRow      0        0
#> Chrysler Imperial            0                 DataRow      0        0
#> Fiat 128                     0                 DataRow      0        0
#> Honda Civic                  0                 DataRow      0        0
#> Toyota Corolla               0                 DataRow      0        0
#> Toyota Corona                0                 DataRow      0        0
#> Dodge Challenger             0                 DataRow      0        0
#> AMC Javelin                  0                 DataRow      0        0
#> Camaro Z28                   0                 DataRow      0        0
#> Pontiac Firebird             0                 DataRow      0        0
#> Fiat X1-9                    0                 DataRow      0        0
#>                     ncellrefs nreflines force_page page_title trailing_sep
#> Merc 450SLC                 0         0      FALSE       <NA>         <NA>
#> Cadillac Fleetwood          0         0      FALSE       <NA>         <NA>
#> Lincoln Continental         0         0      FALSE       <NA>         <NA>
#> Chrysler Imperial           0         0      FALSE       <NA>         <NA>
#> Fiat 128                    0         0      FALSE       <NA>         <NA>
#> Honda Civic                 0         0      FALSE       <NA>         <NA>
#> Toyota Corolla              0         0      FALSE       <NA>         <NA>
#> Toyota Corona               0         0      FALSE       <NA>         <NA>
#> Dodge Challenger            0         0      FALSE       <NA>         <NA>
#> AMC Javelin                 0         0      FALSE       <NA>         <NA>
#> Camaro Z28                  0         0      FALSE       <NA>         <NA>
#> Pontiac Firebird            0         0      FALSE       <NA>         <NA>
#> Fiat X1-9                   0         0      FALSE       <NA>         <NA>
#>                     ref_info_df
#> Merc 450SLC                    
#> Cadillac Fleetwood             
#> Lincoln Continental            
#> Chrysler Imperial              
#> Fiat 128                       
#> Honda Civic                    
#> Toyota Corolla                 
#> Toyota Corona                  
#> Dodge Challenger               
#> AMC Javelin                    
#> Camaro Z28                     
#> Pontiac Firebird               
#> Fiat X1-9                      
#> 
#> $line_grouping
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14
#> 
#> $main_title
#> [1] ""
#> 
#> $subtitles
#> character(0)
#> 
#> $page_titles
#> character(0)
#> 
#> $main_footer
#> [1] ""
#> 
#> $prov_footer
#> [1] "                                         page 4 of 9"
#> 
#> $header_section_div
#> [1] NA
#> 
#> $horizontal_sep
#> [1] "o"
#> 
#> $col_gap
#> [1] 3
#> 
#> $listing_keycols
#> NULL
#> 
#> $table_inset
#> [1] 0
#> 
#> $has_topleft
#> [1] FALSE
#> 
#> $indent_size
#> [1] 2
#> 
#> $col_widths
#> rnms  mpg  cyl disp   hp 
#>   19    4    3    5    3 
#> 
#> $fontspec
#> $family
#> [1] "Courier"
#> 
#> $size
#> [1] 8
#> 
#> $lineheight
#> [1] 1
#> 
#> attr(,"class")
#> [1] "font_spec" "list"     
#> 
#> $num_rep_cols
#> [1] 0
#> 
#> $round_type
#> [1] "iec"
#> 
#> $ref_fnote_df
#> [1] row_path  col_path  row       col       symbol    ref_index msg      
#> [8] nlines   
#> <0 rows> (or 0-length row.names)
#> 
#> $col_info
#>   label name abs_rownumber path pos_in_siblings n_siblings self_extent
#> 1     1    1             1   NA               0          0           4
#> 2     2    2             2   NA               0          0           3
#> 3     3    3             3   NA               0          0           5
#> 4     4    4             4   NA               0          0           3
#>   par_extent reprint_inds node_class indent nrowrefs ncellrefs nreflines
#> 1          0                   stuff      0        0         0         0
#> 2          0                   stuff      0        0         0         0
#> 3          0                   stuff      0        0         0         0
#> 4          0                   stuff      0        0         0         0
#>   force_page page_title trailing_sep ref_info_df
#> 1      FALSE       <NA>         <NA>            
#> 2      FALSE       <NA>         <NA>            
#> 3      FALSE       <NA>         <NA>            
#> 4      FALSE       <NA>         <NA>            
#> 
#> attr(,"nrow_header")
#> [1] 1
#> attr(,"ncols")
#> [1] 4
#> attr(,"class")
#> [1] "MatrixPrintForm" "list"           
#> 
#> [[5]]
#> $strings
#>       rnms                  drat   wt      qsec    vs   am  
#>  [1,] ""                    "drat" "wt"    "qsec"  "vs" "am"
#>  [2,] "Merc 450SLC"         "3.07" "3.78"  "18"    "0"  "0" 
#>  [3,] "Cadillac Fleetwood"  "2.93" "5.25"  "17.98" "0"  "0" 
#>  [4,] "Lincoln Continental" "3"    "5.424" "17.82" "0"  "0" 
#>  [5,] "Chrysler Imperial"   "3.23" "5.345" "17.42" "0"  "0" 
#>  [6,] "Fiat 128"            "4.08" "2.2"   "19.47" "1"  "1" 
#>  [7,] "Honda Civic"         "4.93" "1.615" "18.52" "1"  "1" 
#>  [8,] "Toyota Corolla"      "4.22" "1.835" "19.9"  "1"  "1" 
#>  [9,] "Toyota Corona"       "3.7"  "2.465" "20.01" "1"  "0" 
#> [10,] "Dodge Challenger"    "2.76" "3.52"  "16.87" "0"  "0" 
#> [11,] "AMC Javelin"         "3.15" "3.435" "17.3"  "0"  "0" 
#> [12,] "Camaro Z28"          "3.73" "3.84"  "15.41" "0"  "0" 
#> [13,] "Pontiac Firebird"    "3.08" "3.845" "17.05" "0"  "0" 
#> [14,] "Fiat X1-9"           "4.08" "1.935" "18.9"  "1"  "1" 
#> 
#> $spans
#>       [,1] [,2] [,3] [,4] [,5] [,6]
#>  [1,]    1    1    1    1    1    1
#>  [2,]    1    1    1    1    1    1
#>  [3,]    1    1    1    1    1    1
#>  [4,]    1    1    1    1    1    1
#>  [5,]    1    1    1    1    1    1
#>  [6,]    1    1    1    1    1    1
#>  [7,]    1    1    1    1    1    1
#>  [8,]    1    1    1    1    1    1
#>  [9,]    1    1    1    1    1    1
#> [10,]    1    1    1    1    1    1
#> [11,]    1    1    1    1    1    1
#> [12,]    1    1    1    1    1    1
#> [13,]    1    1    1    1    1    1
#> [14,]    1    1    1    1    1    1
#> 
#> $aligns
#>       [,1]   [,2]     [,3]     [,4]     [,5]     [,6]    
#>  [1,] "left" "center" "center" "center" "center" "center"
#>  [2,] "left" "center" "center" "center" "center" "center"
#>  [3,] "left" "center" "center" "center" "center" "center"
#>  [4,] "left" "center" "center" "center" "center" "center"
#>  [5,] "left" "center" "center" "center" "center" "center"
#>  [6,] "left" "center" "center" "center" "center" "center"
#>  [7,] "left" "center" "center" "center" "center" "center"
#>  [8,] "left" "center" "center" "center" "center" "center"
#>  [9,] "left" "center" "center" "center" "center" "center"
#> [10,] "left" "center" "center" "center" "center" "center"
#> [11,] "left" "center" "center" "center" "center" "center"
#> [12,] "left" "center" "center" "center" "center" "center"
#> [13,] "left" "center" "center" "center" "center" "center"
#> [14,] "left" "center" "center" "center" "center" "center"
#> 
#> $display
#>       [,1] [,2] [,3] [,4] [,5] [,6]
#>  [1,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [2,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [3,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [4,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [5,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [6,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [7,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [8,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [9,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [10,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [11,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [12,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [13,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [14,] TRUE TRUE TRUE TRUE TRUE TRUE
#> 
#> $formats
#>    rnms drat wt qsec vs am
#> 1                         
#> 15        xx xx   xx xx xx
#> 16        xx xx   xx xx xx
#> 17        xx xx   xx xx xx
#> 18        xx xx   xx xx xx
#> 19        xx xx   xx xx xx
#> 20        xx xx   xx xx xx
#> 21        xx xx   xx xx xx
#> 22        xx xx   xx xx xx
#> 23        xx xx   xx xx xx
#> 24        xx xx   xx xx xx
#> 25        xx xx   xx xx xx
#> 26        xx xx   xx xx xx
#> 27        xx xx   xx xx xx
#> 
#> $row_info
#>                                   label                name abs_rownumber
#> Merc 450SLC                 Merc 450SLC         Merc 450SLC             1
#> Cadillac Fleetwood   Cadillac Fleetwood  Cadillac Fleetwood             2
#> Lincoln Continental Lincoln Continental Lincoln Continental             3
#> Chrysler Imperial     Chrysler Imperial   Chrysler Imperial             4
#> Fiat 128                       Fiat 128            Fiat 128             5
#> Honda Civic                 Honda Civic         Honda Civic             6
#> Toyota Corolla           Toyota Corolla      Toyota Corolla             7
#> Toyota Corona             Toyota Corona       Toyota Corona             8
#> Dodge Challenger       Dodge Challenger    Dodge Challenger             9
#> AMC Javelin                 AMC Javelin         AMC Javelin            10
#> Camaro Z28                   Camaro Z28          Camaro Z28            11
#> Pontiac Firebird       Pontiac Firebird    Pontiac Firebird            12
#> Fiat X1-9                     Fiat X1-9           Fiat X1-9            13
#>                             path pos_in_siblings n_siblings self_extent
#> Merc 450SLC          Merc 450SLC              14         32           1
#> Cadillac Fleetwood  Cadillac....              15         32           1
#> Lincoln Continental Lincoln ....              16         32           1
#> Chrysler Imperial   Chrysler....              17         32           1
#> Fiat 128                Fiat 128              18         32           1
#> Honda Civic          Honda Civic              19         32           1
#> Toyota Corolla      Toyota C....              20         32           1
#> Toyota Corona       Toyota C....              21         32           1
#> Dodge Challenger    Dodge Ch....              22         32           1
#> AMC Javelin          AMC Javelin              23         32           1
#> Camaro Z28            Camaro Z28              24         32           1
#> Pontiac Firebird    Pontiac ....              25         32           1
#> Fiat X1-9              Fiat X1-9              26         32           1
#>                     par_extent reprint_inds node_class indent nrowrefs
#> Merc 450SLC                  0                 DataRow      0        0
#> Cadillac Fleetwood           0                 DataRow      0        0
#> Lincoln Continental          0                 DataRow      0        0
#> Chrysler Imperial            0                 DataRow      0        0
#> Fiat 128                     0                 DataRow      0        0
#> Honda Civic                  0                 DataRow      0        0
#> Toyota Corolla               0                 DataRow      0        0
#> Toyota Corona                0                 DataRow      0        0
#> Dodge Challenger             0                 DataRow      0        0
#> AMC Javelin                  0                 DataRow      0        0
#> Camaro Z28                   0                 DataRow      0        0
#> Pontiac Firebird             0                 DataRow      0        0
#> Fiat X1-9                    0                 DataRow      0        0
#>                     ncellrefs nreflines force_page page_title trailing_sep
#> Merc 450SLC                 0         0      FALSE       <NA>         <NA>
#> Cadillac Fleetwood          0         0      FALSE       <NA>         <NA>
#> Lincoln Continental         0         0      FALSE       <NA>         <NA>
#> Chrysler Imperial           0         0      FALSE       <NA>         <NA>
#> Fiat 128                    0         0      FALSE       <NA>         <NA>
#> Honda Civic                 0         0      FALSE       <NA>         <NA>
#> Toyota Corolla              0         0      FALSE       <NA>         <NA>
#> Toyota Corona               0         0      FALSE       <NA>         <NA>
#> Dodge Challenger            0         0      FALSE       <NA>         <NA>
#> AMC Javelin                 0         0      FALSE       <NA>         <NA>
#> Camaro Z28                  0         0      FALSE       <NA>         <NA>
#> Pontiac Firebird            0         0      FALSE       <NA>         <NA>
#> Fiat X1-9                   0         0      FALSE       <NA>         <NA>
#>                     ref_info_df
#> Merc 450SLC                    
#> Cadillac Fleetwood             
#> Lincoln Continental            
#> Chrysler Imperial              
#> Fiat 128                       
#> Honda Civic                    
#> Toyota Corolla                 
#> Toyota Corona                  
#> Dodge Challenger               
#> AMC Javelin                    
#> Camaro Z28                     
#> Pontiac Firebird               
#> Fiat X1-9                      
#> 
#> $line_grouping
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14
#> 
#> $main_title
#> [1] ""
#> 
#> $subtitles
#> character(0)
#> 
#> $page_titles
#> character(0)
#> 
#> $main_footer
#> [1] ""
#> 
#> $prov_footer
#> [1] "                                         page 5 of 9"
#> 
#> $header_section_div
#> [1] NA
#> 
#> $horizontal_sep
#> [1] "o"
#> 
#> $col_gap
#> [1] 3
#> 
#> $listing_keycols
#> NULL
#> 
#> $table_inset
#> [1] 0
#> 
#> $has_topleft
#> [1] FALSE
#> 
#> $indent_size
#> [1] 2
#> 
#> $col_widths
#> rnms drat   wt qsec   vs   am 
#>   19    4    5    5    2    2 
#> 
#> $fontspec
#> $family
#> [1] "Courier"
#> 
#> $size
#> [1] 8
#> 
#> $lineheight
#> [1] 1
#> 
#> attr(,"class")
#> [1] "font_spec" "list"     
#> 
#> $num_rep_cols
#> [1] 0
#> 
#> $round_type
#> [1] "iec"
#> 
#> $ref_fnote_df
#> [1] row_path  col_path  row       col       symbol    ref_index msg      
#> [8] nlines   
#> <0 rows> (or 0-length row.names)
#> 
#> $col_info
#>   label name abs_rownumber path pos_in_siblings n_siblings self_extent
#> 5     5    5             5   NA               0          0           4
#> 6     6    6             6   NA               0          0           5
#> 7     7    7             7   NA               0          0           5
#> 8     8    8             8   NA               0          0           2
#> 9     9    9             9   NA               0          0           2
#>   par_extent reprint_inds node_class indent nrowrefs ncellrefs nreflines
#> 5          0                   stuff      0        0         0         0
#> 6          0                   stuff      0        0         0         0
#> 7          0                   stuff      0        0         0         0
#> 8          0                   stuff      0        0         0         0
#> 9          0                   stuff      0        0         0         0
#>   force_page page_title trailing_sep ref_info_df
#> 5      FALSE       <NA>         <NA>            
#> 6      FALSE       <NA>         <NA>            
#> 7      FALSE       <NA>         <NA>            
#> 8      FALSE       <NA>         <NA>            
#> 9      FALSE       <NA>         <NA>            
#> 
#> attr(,"nrow_header")
#> [1] 1
#> attr(,"ncols")
#> [1] 5
#> attr(,"class")
#> [1] "MatrixPrintForm" "list"           
#> 
#> [[6]]
#> $strings
#>       rnms                  gear   carb  
#>  [1,] ""                    "gear" "carb"
#>  [2,] "Merc 450SLC"         "3"    "3"   
#>  [3,] "Cadillac Fleetwood"  "3"    "4"   
#>  [4,] "Lincoln Continental" "3"    "4"   
#>  [5,] "Chrysler Imperial"   "3"    "4"   
#>  [6,] "Fiat 128"            "4"    "1"   
#>  [7,] "Honda Civic"         "4"    "2"   
#>  [8,] "Toyota Corolla"      "4"    "1"   
#>  [9,] "Toyota Corona"       "3"    "1"   
#> [10,] "Dodge Challenger"    "3"    "2"   
#> [11,] "AMC Javelin"         "3"    "2"   
#> [12,] "Camaro Z28"          "3"    "4"   
#> [13,] "Pontiac Firebird"    "3"    "2"   
#> [14,] "Fiat X1-9"           "4"    "1"   
#> 
#> $spans
#>       [,1] [,2] [,3]
#>  [1,]    1    1    1
#>  [2,]    1    1    1
#>  [3,]    1    1    1
#>  [4,]    1    1    1
#>  [5,]    1    1    1
#>  [6,]    1    1    1
#>  [7,]    1    1    1
#>  [8,]    1    1    1
#>  [9,]    1    1    1
#> [10,]    1    1    1
#> [11,]    1    1    1
#> [12,]    1    1    1
#> [13,]    1    1    1
#> [14,]    1    1    1
#> 
#> $aligns
#>       [,1]   [,2]     [,3]    
#>  [1,] "left" "center" "center"
#>  [2,] "left" "center" "center"
#>  [3,] "left" "center" "center"
#>  [4,] "left" "center" "center"
#>  [5,] "left" "center" "center"
#>  [6,] "left" "center" "center"
#>  [7,] "left" "center" "center"
#>  [8,] "left" "center" "center"
#>  [9,] "left" "center" "center"
#> [10,] "left" "center" "center"
#> [11,] "left" "center" "center"
#> [12,] "left" "center" "center"
#> [13,] "left" "center" "center"
#> [14,] "left" "center" "center"
#> 
#> $display
#>       [,1] [,2] [,3]
#>  [1,] TRUE TRUE TRUE
#>  [2,] TRUE TRUE TRUE
#>  [3,] TRUE TRUE TRUE
#>  [4,] TRUE TRUE TRUE
#>  [5,] TRUE TRUE TRUE
#>  [6,] TRUE TRUE TRUE
#>  [7,] TRUE TRUE TRUE
#>  [8,] TRUE TRUE TRUE
#>  [9,] TRUE TRUE TRUE
#> [10,] TRUE TRUE TRUE
#> [11,] TRUE TRUE TRUE
#> [12,] TRUE TRUE TRUE
#> [13,] TRUE TRUE TRUE
#> [14,] TRUE TRUE TRUE
#> 
#> $formats
#>    rnms gear carb
#> 1                
#> 15        xx   xx
#> 16        xx   xx
#> 17        xx   xx
#> 18        xx   xx
#> 19        xx   xx
#> 20        xx   xx
#> 21        xx   xx
#> 22        xx   xx
#> 23        xx   xx
#> 24        xx   xx
#> 25        xx   xx
#> 26        xx   xx
#> 27        xx   xx
#> 
#> $row_info
#>                                   label                name abs_rownumber
#> Merc 450SLC                 Merc 450SLC         Merc 450SLC             1
#> Cadillac Fleetwood   Cadillac Fleetwood  Cadillac Fleetwood             2
#> Lincoln Continental Lincoln Continental Lincoln Continental             3
#> Chrysler Imperial     Chrysler Imperial   Chrysler Imperial             4
#> Fiat 128                       Fiat 128            Fiat 128             5
#> Honda Civic                 Honda Civic         Honda Civic             6
#> Toyota Corolla           Toyota Corolla      Toyota Corolla             7
#> Toyota Corona             Toyota Corona       Toyota Corona             8
#> Dodge Challenger       Dodge Challenger    Dodge Challenger             9
#> AMC Javelin                 AMC Javelin         AMC Javelin            10
#> Camaro Z28                   Camaro Z28          Camaro Z28            11
#> Pontiac Firebird       Pontiac Firebird    Pontiac Firebird            12
#> Fiat X1-9                     Fiat X1-9           Fiat X1-9            13
#>                             path pos_in_siblings n_siblings self_extent
#> Merc 450SLC          Merc 450SLC              14         32           1
#> Cadillac Fleetwood  Cadillac....              15         32           1
#> Lincoln Continental Lincoln ....              16         32           1
#> Chrysler Imperial   Chrysler....              17         32           1
#> Fiat 128                Fiat 128              18         32           1
#> Honda Civic          Honda Civic              19         32           1
#> Toyota Corolla      Toyota C....              20         32           1
#> Toyota Corona       Toyota C....              21         32           1
#> Dodge Challenger    Dodge Ch....              22         32           1
#> AMC Javelin          AMC Javelin              23         32           1
#> Camaro Z28            Camaro Z28              24         32           1
#> Pontiac Firebird    Pontiac ....              25         32           1
#> Fiat X1-9              Fiat X1-9              26         32           1
#>                     par_extent reprint_inds node_class indent nrowrefs
#> Merc 450SLC                  0                 DataRow      0        0
#> Cadillac Fleetwood           0                 DataRow      0        0
#> Lincoln Continental          0                 DataRow      0        0
#> Chrysler Imperial            0                 DataRow      0        0
#> Fiat 128                     0                 DataRow      0        0
#> Honda Civic                  0                 DataRow      0        0
#> Toyota Corolla               0                 DataRow      0        0
#> Toyota Corona                0                 DataRow      0        0
#> Dodge Challenger             0                 DataRow      0        0
#> AMC Javelin                  0                 DataRow      0        0
#> Camaro Z28                   0                 DataRow      0        0
#> Pontiac Firebird             0                 DataRow      0        0
#> Fiat X1-9                    0                 DataRow      0        0
#>                     ncellrefs nreflines force_page page_title trailing_sep
#> Merc 450SLC                 0         0      FALSE       <NA>         <NA>
#> Cadillac Fleetwood          0         0      FALSE       <NA>         <NA>
#> Lincoln Continental         0         0      FALSE       <NA>         <NA>
#> Chrysler Imperial           0         0      FALSE       <NA>         <NA>
#> Fiat 128                    0         0      FALSE       <NA>         <NA>
#> Honda Civic                 0         0      FALSE       <NA>         <NA>
#> Toyota Corolla              0         0      FALSE       <NA>         <NA>
#> Toyota Corona               0         0      FALSE       <NA>         <NA>
#> Dodge Challenger            0         0      FALSE       <NA>         <NA>
#> AMC Javelin                 0         0      FALSE       <NA>         <NA>
#> Camaro Z28                  0         0      FALSE       <NA>         <NA>
#> Pontiac Firebird            0         0      FALSE       <NA>         <NA>
#> Fiat X1-9                   0         0      FALSE       <NA>         <NA>
#>                     ref_info_df
#> Merc 450SLC                    
#> Cadillac Fleetwood             
#> Lincoln Continental            
#> Chrysler Imperial              
#> Fiat 128                       
#> Honda Civic                    
#> Toyota Corolla                 
#> Toyota Corona                  
#> Dodge Challenger               
#> AMC Javelin                    
#> Camaro Z28                     
#> Pontiac Firebird               
#> Fiat X1-9                      
#> 
#> $line_grouping
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14
#> 
#> $main_title
#> [1] ""
#> 
#> $subtitles
#> character(0)
#> 
#> $page_titles
#> character(0)
#> 
#> $main_footer
#> [1] ""
#> 
#> $prov_footer
#> [1] "                                         page 6 of 9"
#> 
#> $header_section_div
#> [1] NA
#> 
#> $horizontal_sep
#> [1] "o"
#> 
#> $col_gap
#> [1] 3
#> 
#> $listing_keycols
#> NULL
#> 
#> $table_inset
#> [1] 0
#> 
#> $has_topleft
#> [1] FALSE
#> 
#> $indent_size
#> [1] 2
#> 
#> $col_widths
#> rnms gear carb 
#>   19    4    4 
#> 
#> $fontspec
#> $family
#> [1] "Courier"
#> 
#> $size
#> [1] 8
#> 
#> $lineheight
#> [1] 1
#> 
#> attr(,"class")
#> [1] "font_spec" "list"     
#> 
#> $num_rep_cols
#> [1] 0
#> 
#> $round_type
#> [1] "iec"
#> 
#> $ref_fnote_df
#> [1] row_path  col_path  row       col       symbol    ref_index msg      
#> [8] nlines   
#> <0 rows> (or 0-length row.names)
#> 
#> $col_info
#>    label name abs_rownumber path pos_in_siblings n_siblings self_extent
#> 10    10   10            10   NA               0          0           4
#> 11    11   11            11   NA               0          0           4
#>    par_extent reprint_inds node_class indent nrowrefs ncellrefs nreflines
#> 10          0                   stuff      0        0         0         0
#> 11          0                   stuff      0        0         0         0
#>    force_page page_title trailing_sep ref_info_df
#> 10      FALSE       <NA>         <NA>            
#> 11      FALSE       <NA>         <NA>            
#> 
#> attr(,"nrow_header")
#> [1] 1
#> attr(,"ncols")
#> [1] 2
#> attr(,"class")
#> [1] "MatrixPrintForm" "list"           
#> 
#> [[7]]
#> $strings
#>      rnms             mpg    cyl   disp    hp   
#> [1,] ""               "mpg"  "cyl" "disp"  "hp" 
#> [2,] "Porsche 914-2"  "26"   "4"   "120.3" "91" 
#> [3,] "Lotus Europa"   "30.4" "4"   "95.1"  "113"
#> [4,] "Ford Pantera L" "15.8" "8"   "351"   "264"
#> [5,] "Ferrari Dino"   "19.7" "6"   "145"   "175"
#> [6,] "Maserati Bora"  "15"   "8"   "301"   "335"
#> [7,] "Volvo 142E"     "21.4" "4"   "121"   "109"
#> 
#> $spans
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    1    1    1    1
#> [2,]    1    1    1    1    1
#> [3,]    1    1    1    1    1
#> [4,]    1    1    1    1    1
#> [5,]    1    1    1    1    1
#> [6,]    1    1    1    1    1
#> [7,]    1    1    1    1    1
#> 
#> $aligns
#>      [,1]   [,2]     [,3]     [,4]     [,5]    
#> [1,] "left" "center" "center" "center" "center"
#> [2,] "left" "center" "center" "center" "center"
#> [3,] "left" "center" "center" "center" "center"
#> [4,] "left" "center" "center" "center" "center"
#> [5,] "left" "center" "center" "center" "center"
#> [6,] "left" "center" "center" "center" "center"
#> [7,] "left" "center" "center" "center" "center"
#> 
#> $display
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] TRUE TRUE TRUE TRUE TRUE
#> [2,] TRUE TRUE TRUE TRUE TRUE
#> [3,] TRUE TRUE TRUE TRUE TRUE
#> [4,] TRUE TRUE TRUE TRUE TRUE
#> [5,] TRUE TRUE TRUE TRUE TRUE
#> [6,] TRUE TRUE TRUE TRUE TRUE
#> [7,] TRUE TRUE TRUE TRUE TRUE
#> 
#> $formats
#>    rnms mpg cyl disp hp
#> 1                      
#> 28       xx  xx   xx xx
#> 29       xx  xx   xx xx
#> 30       xx  xx   xx xx
#> 31       xx  xx   xx xx
#> 32       xx  xx   xx xx
#> 33       xx  xx   xx xx
#> 
#> $row_info
#>                         label           name abs_rownumber         path
#> Porsche 914-2   Porsche 914-2  Porsche 914-2             1 Porsche ....
#> Lotus Europa     Lotus Europa   Lotus Europa             2 Lotus Europa
#> Ford Pantera L Ford Pantera L Ford Pantera L             3 Ford Pan....
#> Ferrari Dino     Ferrari Dino   Ferrari Dino             4 Ferrari Dino
#> Maserati Bora   Maserati Bora  Maserati Bora             5 Maserati....
#> Volvo 142E         Volvo 142E     Volvo 142E             6   Volvo 142E
#>                pos_in_siblings n_siblings self_extent par_extent reprint_inds
#> Porsche 914-2               27         32           1          0             
#> Lotus Europa                28         32           1          0             
#> Ford Pantera L              29         32           1          0             
#> Ferrari Dino                30         32           1          0             
#> Maserati Bora               31         32           1          0             
#> Volvo 142E                  32         32           1          0             
#>                node_class indent nrowrefs ncellrefs nreflines force_page
#> Porsche 914-2     DataRow      0        0         0         0      FALSE
#> Lotus Europa      DataRow      0        0         0         0      FALSE
#> Ford Pantera L    DataRow      0        0         0         0      FALSE
#> Ferrari Dino      DataRow      0        0         0         0      FALSE
#> Maserati Bora     DataRow      0        0         0         0      FALSE
#> Volvo 142E        DataRow      0        0         0         0      FALSE
#>                page_title trailing_sep ref_info_df
#> Porsche 914-2        <NA>         <NA>            
#> Lotus Europa         <NA>         <NA>            
#> Ford Pantera L       <NA>         <NA>            
#> Ferrari Dino         <NA>         <NA>            
#> Maserati Bora        <NA>         <NA>            
#> Volvo 142E           <NA>         <NA>            
#> 
#> $line_grouping
#> [1] 1 2 3 4 5 6 7
#> 
#> $main_title
#> [1] ""
#> 
#> $subtitles
#> character(0)
#> 
#> $page_titles
#> character(0)
#> 
#> $main_footer
#> [1] ""
#> 
#> $prov_footer
#> [1] "                                         page 7 of 9"
#> 
#> $header_section_div
#> [1] NA
#> 
#> $horizontal_sep
#> [1] "o"
#> 
#> $col_gap
#> [1] 3
#> 
#> $listing_keycols
#> NULL
#> 
#> $table_inset
#> [1] 0
#> 
#> $has_topleft
#> [1] FALSE
#> 
#> $indent_size
#> [1] 2
#> 
#> $col_widths
#> rnms  mpg  cyl disp   hp 
#>   19    4    3    5    3 
#> 
#> $fontspec
#> $family
#> [1] "Courier"
#> 
#> $size
#> [1] 8
#> 
#> $lineheight
#> [1] 1
#> 
#> attr(,"class")
#> [1] "font_spec" "list"     
#> 
#> $num_rep_cols
#> [1] 0
#> 
#> $round_type
#> [1] "iec"
#> 
#> $ref_fnote_df
#> [1] row_path  col_path  row       col       symbol    ref_index msg      
#> [8] nlines   
#> <0 rows> (or 0-length row.names)
#> 
#> $col_info
#>   label name abs_rownumber path pos_in_siblings n_siblings self_extent
#> 1     1    1             1   NA               0          0           4
#> 2     2    2             2   NA               0          0           3
#> 3     3    3             3   NA               0          0           5
#> 4     4    4             4   NA               0          0           3
#>   par_extent reprint_inds node_class indent nrowrefs ncellrefs nreflines
#> 1          0                   stuff      0        0         0         0
#> 2          0                   stuff      0        0         0         0
#> 3          0                   stuff      0        0         0         0
#> 4          0                   stuff      0        0         0         0
#>   force_page page_title trailing_sep ref_info_df
#> 1      FALSE       <NA>         <NA>            
#> 2      FALSE       <NA>         <NA>            
#> 3      FALSE       <NA>         <NA>            
#> 4      FALSE       <NA>         <NA>            
#> 
#> attr(,"nrow_header")
#> [1] 1
#> attr(,"ncols")
#> [1] 4
#> attr(,"class")
#> [1] "MatrixPrintForm" "list"           
#> 
#> [[8]]
#> $strings
#>      rnms             drat   wt      qsec   vs   am  
#> [1,] ""               "drat" "wt"    "qsec" "vs" "am"
#> [2,] "Porsche 914-2"  "4.43" "2.14"  "16.7" "0"  "1" 
#> [3,] "Lotus Europa"   "3.77" "1.513" "16.9" "1"  "1" 
#> [4,] "Ford Pantera L" "4.22" "3.17"  "14.5" "0"  "1" 
#> [5,] "Ferrari Dino"   "3.62" "2.77"  "15.5" "0"  "1" 
#> [6,] "Maserati Bora"  "3.54" "3.57"  "14.6" "0"  "1" 
#> [7,] "Volvo 142E"     "4.11" "2.78"  "18.6" "1"  "1" 
#> 
#> $spans
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    1    1    1    1    1    1
#> [2,]    1    1    1    1    1    1
#> [3,]    1    1    1    1    1    1
#> [4,]    1    1    1    1    1    1
#> [5,]    1    1    1    1    1    1
#> [6,]    1    1    1    1    1    1
#> [7,]    1    1    1    1    1    1
#> 
#> $aligns
#>      [,1]   [,2]     [,3]     [,4]     [,5]     [,6]    
#> [1,] "left" "center" "center" "center" "center" "center"
#> [2,] "left" "center" "center" "center" "center" "center"
#> [3,] "left" "center" "center" "center" "center" "center"
#> [4,] "left" "center" "center" "center" "center" "center"
#> [5,] "left" "center" "center" "center" "center" "center"
#> [6,] "left" "center" "center" "center" "center" "center"
#> [7,] "left" "center" "center" "center" "center" "center"
#> 
#> $display
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [2,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [3,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [4,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [5,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [6,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [7,] TRUE TRUE TRUE TRUE TRUE TRUE
#> 
#> $formats
#>    rnms drat wt qsec vs am
#> 1                         
#> 28        xx xx   xx xx xx
#> 29        xx xx   xx xx xx
#> 30        xx xx   xx xx xx
#> 31        xx xx   xx xx xx
#> 32        xx xx   xx xx xx
#> 33        xx xx   xx xx xx
#> 
#> $row_info
#>                         label           name abs_rownumber         path
#> Porsche 914-2   Porsche 914-2  Porsche 914-2             1 Porsche ....
#> Lotus Europa     Lotus Europa   Lotus Europa             2 Lotus Europa
#> Ford Pantera L Ford Pantera L Ford Pantera L             3 Ford Pan....
#> Ferrari Dino     Ferrari Dino   Ferrari Dino             4 Ferrari Dino
#> Maserati Bora   Maserati Bora  Maserati Bora             5 Maserati....
#> Volvo 142E         Volvo 142E     Volvo 142E             6   Volvo 142E
#>                pos_in_siblings n_siblings self_extent par_extent reprint_inds
#> Porsche 914-2               27         32           1          0             
#> Lotus Europa                28         32           1          0             
#> Ford Pantera L              29         32           1          0             
#> Ferrari Dino                30         32           1          0             
#> Maserati Bora               31         32           1          0             
#> Volvo 142E                  32         32           1          0             
#>                node_class indent nrowrefs ncellrefs nreflines force_page
#> Porsche 914-2     DataRow      0        0         0         0      FALSE
#> Lotus Europa      DataRow      0        0         0         0      FALSE
#> Ford Pantera L    DataRow      0        0         0         0      FALSE
#> Ferrari Dino      DataRow      0        0         0         0      FALSE
#> Maserati Bora     DataRow      0        0         0         0      FALSE
#> Volvo 142E        DataRow      0        0         0         0      FALSE
#>                page_title trailing_sep ref_info_df
#> Porsche 914-2        <NA>         <NA>            
#> Lotus Europa         <NA>         <NA>            
#> Ford Pantera L       <NA>         <NA>            
#> Ferrari Dino         <NA>         <NA>            
#> Maserati Bora        <NA>         <NA>            
#> Volvo 142E           <NA>         <NA>            
#> 
#> $line_grouping
#> [1] 1 2 3 4 5 6 7
#> 
#> $main_title
#> [1] ""
#> 
#> $subtitles
#> character(0)
#> 
#> $page_titles
#> character(0)
#> 
#> $main_footer
#> [1] ""
#> 
#> $prov_footer
#> [1] "                                         page 8 of 9"
#> 
#> $header_section_div
#> [1] NA
#> 
#> $horizontal_sep
#> [1] "o"
#> 
#> $col_gap
#> [1] 3
#> 
#> $listing_keycols
#> NULL
#> 
#> $table_inset
#> [1] 0
#> 
#> $has_topleft
#> [1] FALSE
#> 
#> $indent_size
#> [1] 2
#> 
#> $col_widths
#> rnms drat   wt qsec   vs   am 
#>   19    4    5    5    2    2 
#> 
#> $fontspec
#> $family
#> [1] "Courier"
#> 
#> $size
#> [1] 8
#> 
#> $lineheight
#> [1] 1
#> 
#> attr(,"class")
#> [1] "font_spec" "list"     
#> 
#> $num_rep_cols
#> [1] 0
#> 
#> $round_type
#> [1] "iec"
#> 
#> $ref_fnote_df
#> [1] row_path  col_path  row       col       symbol    ref_index msg      
#> [8] nlines   
#> <0 rows> (or 0-length row.names)
#> 
#> $col_info
#>   label name abs_rownumber path pos_in_siblings n_siblings self_extent
#> 5     5    5             5   NA               0          0           4
#> 6     6    6             6   NA               0          0           5
#> 7     7    7             7   NA               0          0           5
#> 8     8    8             8   NA               0          0           2
#> 9     9    9             9   NA               0          0           2
#>   par_extent reprint_inds node_class indent nrowrefs ncellrefs nreflines
#> 5          0                   stuff      0        0         0         0
#> 6          0                   stuff      0        0         0         0
#> 7          0                   stuff      0        0         0         0
#> 8          0                   stuff      0        0         0         0
#> 9          0                   stuff      0        0         0         0
#>   force_page page_title trailing_sep ref_info_df
#> 5      FALSE       <NA>         <NA>            
#> 6      FALSE       <NA>         <NA>            
#> 7      FALSE       <NA>         <NA>            
#> 8      FALSE       <NA>         <NA>            
#> 9      FALSE       <NA>         <NA>            
#> 
#> attr(,"nrow_header")
#> [1] 1
#> attr(,"ncols")
#> [1] 5
#> attr(,"class")
#> [1] "MatrixPrintForm" "list"           
#> 
#> [[9]]
#> $strings
#>      rnms             gear   carb  
#> [1,] ""               "gear" "carb"
#> [2,] "Porsche 914-2"  "5"    "2"   
#> [3,] "Lotus Europa"   "5"    "2"   
#> [4,] "Ford Pantera L" "5"    "4"   
#> [5,] "Ferrari Dino"   "5"    "6"   
#> [6,] "Maserati Bora"  "5"    "8"   
#> [7,] "Volvo 142E"     "4"    "2"   
#> 
#> $spans
#>      [,1] [,2] [,3]
#> [1,]    1    1    1
#> [2,]    1    1    1
#> [3,]    1    1    1
#> [4,]    1    1    1
#> [5,]    1    1    1
#> [6,]    1    1    1
#> [7,]    1    1    1
#> 
#> $aligns
#>      [,1]   [,2]     [,3]    
#> [1,] "left" "center" "center"
#> [2,] "left" "center" "center"
#> [3,] "left" "center" "center"
#> [4,] "left" "center" "center"
#> [5,] "left" "center" "center"
#> [6,] "left" "center" "center"
#> [7,] "left" "center" "center"
#> 
#> $display
#>      [,1] [,2] [,3]
#> [1,] TRUE TRUE TRUE
#> [2,] TRUE TRUE TRUE
#> [3,] TRUE TRUE TRUE
#> [4,] TRUE TRUE TRUE
#> [5,] TRUE TRUE TRUE
#> [6,] TRUE TRUE TRUE
#> [7,] TRUE TRUE TRUE
#> 
#> $formats
#>    rnms gear carb
#> 1                
#> 28        xx   xx
#> 29        xx   xx
#> 30        xx   xx
#> 31        xx   xx
#> 32        xx   xx
#> 33        xx   xx
#> 
#> $row_info
#>                         label           name abs_rownumber         path
#> Porsche 914-2   Porsche 914-2  Porsche 914-2             1 Porsche ....
#> Lotus Europa     Lotus Europa   Lotus Europa             2 Lotus Europa
#> Ford Pantera L Ford Pantera L Ford Pantera L             3 Ford Pan....
#> Ferrari Dino     Ferrari Dino   Ferrari Dino             4 Ferrari Dino
#> Maserati Bora   Maserati Bora  Maserati Bora             5 Maserati....
#> Volvo 142E         Volvo 142E     Volvo 142E             6   Volvo 142E
#>                pos_in_siblings n_siblings self_extent par_extent reprint_inds
#> Porsche 914-2               27         32           1          0             
#> Lotus Europa                28         32           1          0             
#> Ford Pantera L              29         32           1          0             
#> Ferrari Dino                30         32           1          0             
#> Maserati Bora               31         32           1          0             
#> Volvo 142E                  32         32           1          0             
#>                node_class indent nrowrefs ncellrefs nreflines force_page
#> Porsche 914-2     DataRow      0        0         0         0      FALSE
#> Lotus Europa      DataRow      0        0         0         0      FALSE
#> Ford Pantera L    DataRow      0        0         0         0      FALSE
#> Ferrari Dino      DataRow      0        0         0         0      FALSE
#> Maserati Bora     DataRow      0        0         0         0      FALSE
#> Volvo 142E        DataRow      0        0         0         0      FALSE
#>                page_title trailing_sep ref_info_df
#> Porsche 914-2        <NA>         <NA>            
#> Lotus Europa         <NA>         <NA>            
#> Ford Pantera L       <NA>         <NA>            
#> Ferrari Dino         <NA>         <NA>            
#> Maserati Bora        <NA>         <NA>            
#> Volvo 142E           <NA>         <NA>            
#> 
#> $line_grouping
#> [1] 1 2 3 4 5 6 7
#> 
#> $main_title
#> [1] ""
#> 
#> $subtitles
#> character(0)
#> 
#> $page_titles
#> character(0)
#> 
#> $main_footer
#> [1] ""
#> 
#> $prov_footer
#> [1] "                                         page 9 of 9"
#> 
#> $header_section_div
#> [1] NA
#> 
#> $horizontal_sep
#> [1] "o"
#> 
#> $col_gap
#> [1] 3
#> 
#> $listing_keycols
#> NULL
#> 
#> $table_inset
#> [1] 0
#> 
#> $has_topleft
#> [1] FALSE
#> 
#> $indent_size
#> [1] 2
#> 
#> $col_widths
#> rnms gear carb 
#>   19    4    4 
#> 
#> $fontspec
#> $family
#> [1] "Courier"
#> 
#> $size
#> [1] 8
#> 
#> $lineheight
#> [1] 1
#> 
#> attr(,"class")
#> [1] "font_spec" "list"     
#> 
#> $num_rep_cols
#> [1] 0
#> 
#> $round_type
#> [1] "iec"
#> 
#> $ref_fnote_df
#> [1] row_path  col_path  row       col       symbol    ref_index msg      
#> [8] nlines   
#> <0 rows> (or 0-length row.names)
#> 
#> $col_info
#>    label name abs_rownumber path pos_in_siblings n_siblings self_extent
#> 10    10   10            10   NA               0          0           4
#> 11    11   11            11   NA               0          0           4
#>    par_extent reprint_inds node_class indent nrowrefs ncellrefs nreflines
#> 10          0                   stuff      0        0         0         0
#> 11          0                   stuff      0        0         0         0
#>    force_page page_title trailing_sep ref_info_df
#> 10      FALSE       <NA>         <NA>            
#> 11      FALSE       <NA>         <NA>            
#> 
#> attr(,"nrow_header")
#> [1] 1
#> attr(,"ncols")
#> [1] 2
#> attr(,"class")
#> [1] "MatrixPrintForm" "list"           
#> 

diagnose_pagination(mpf, pg_width = 5, pg_height = 3)
#> Determining lines required for header content: 0 title and 2 table header lines
#> Determining lines required for footer content: 3 lines
#> Lines per page available for tables rows: 13 (original: 18)
#> --------- ROW-WISE: Checking possible pagination for page 1
#> -> Attempting pagination between 1 and 13 row
#>   OK [13 lines]
#> --------- ROW-WISE: Checking possible pagination for page 2
#> -> Attempting pagination between 14 and 26 row
#>   OK [13 lines]
#> --------- ROW-WISE: Checking possible pagination for page 3
#> -> Attempting pagination between 27 and 32 row
#>   OK [6 lines]
#> Adjusted characters per page: 33 [original: 52, table inset: 0, row labels: 19]
#> ========= COLUMN-WISE: Checking possible pagination for page 1
#> -> Attempting pagination between 1 and 11 column
#>   FAIL: selected 11 columns require 107 chars, while only 52 are available. 
#>         details: [raw: 74 chars (11 cols), rep. cols: 0 chars (0 cols), tot. colgap: 33 chars].
#> -> Attempting pagination between 1 and 10 column
#>   FAIL: selected 10 columns require 97 chars, while only 52 are available. 
#>         details: [raw: 67 chars (10 cols), rep. cols: 0 chars (0 cols), tot. colgap: 30 chars].
#> -> Attempting pagination between 1 and 9 column
#>   FAIL: selected 9 columns require 87 chars, while only 52 are available. 
#>         details: [raw: 60 chars (9 cols), rep. cols: 0 chars (0 cols), tot. colgap: 27 chars].
#> -> Attempting pagination between 1 and 8 column
#>   FAIL: selected 8 columns require 79 chars, while only 52 are available. 
#>         details: [raw: 55 chars (8 cols), rep. cols: 0 chars (0 cols), tot. colgap: 24 chars].
#> -> Attempting pagination between 1 and 7 column
#>   FAIL: selected 7 columns require 71 chars, while only 52 are available. 
#>         details: [raw: 50 chars (7 cols), rep. cols: 0 chars (0 cols), tot. colgap: 21 chars].
#> -> Attempting pagination between 1 and 6 column
#>   FAIL: selected 6 columns require 60 chars, while only 52 are available. 
#>         details: [raw: 42 chars (6 cols), rep. cols: 0 chars (0 cols), tot. colgap: 18 chars].
#> -> Attempting pagination between 1 and 5 column
#>   FAIL: selected 5 columns require 49 chars, while only 52 are available. 
#>         details: [raw: 34 chars (5 cols), rep. cols: 0 chars (0 cols), tot. colgap: 15 chars].
#> -> Attempting pagination between 1 and 4 column
#>   OK [27 chars]
#> ========= COLUMN-WISE: Checking possible pagination for page 2
#> -> Attempting pagination between 5 and 11 column
#>   FAIL: selected 7 columns require 68 chars, while only 52 are available. 
#>         details: [raw: 47 chars (7 cols), rep. cols: 0 chars (0 cols), tot. colgap: 21 chars].
#> -> Attempting pagination between 5 and 10 column
#>   FAIL: selected 6 columns require 58 chars, while only 52 are available. 
#>         details: [raw: 40 chars (6 cols), rep. cols: 0 chars (0 cols), tot. colgap: 18 chars].
#> -> Attempting pagination between 5 and 9 column
#>   OK [33 chars]
#> ========= COLUMN-WISE: Checking possible pagination for page 3
#> -> Attempting pagination between 10 and 11 column
#>   OK [14 chars]
#> $lpp_diagnostics
#> character(0)
#> 
#> $row_diagnostics
#>    abs_rownumber               label self_extent par_extent node_class
#> 1              1           Mazda RX4           1          0    DataRow
#> 2              2       Mazda RX4 Wag           1          0    DataRow
#> 3              3          Datsun 710           1          0    DataRow
#> 4              4      Hornet 4 Drive           1          0    DataRow
#> 5              5   Hornet Sportabout           1          0    DataRow
#> 6              6             Valiant           1          0    DataRow
#> 7              7          Duster 360           1          0    DataRow
#> 8              8           Merc 240D           1          0    DataRow
#> 9              9            Merc 230           1          0    DataRow
#> 10            10            Merc 280           1          0    DataRow
#> 11            11           Merc 280C           1          0    DataRow
#> 12            12          Merc 450SE           1          0    DataRow
#> 13            13          Merc 450SL           1          0    DataRow
#> 14            14         Merc 450SLC           1          0    DataRow
#> 15            15  Cadillac Fleetwood           1          0    DataRow
#> 16            16 Lincoln Continental           1          0    DataRow
#> 17            17   Chrysler Imperial           1          0    DataRow
#> 18            18            Fiat 128           1          0    DataRow
#> 19            19         Honda Civic           1          0    DataRow
#> 20            20      Toyota Corolla           1          0    DataRow
#> 21            21       Toyota Corona           1          0    DataRow
#> 22            22    Dodge Challenger           1          0    DataRow
#> 23            23         AMC Javelin           1          0    DataRow
#> 24            24          Camaro Z28           1          0    DataRow
#> 25            25    Pontiac Firebird           1          0    DataRow
#> 26            26           Fiat X1-9           1          0    DataRow
#> 27            27       Porsche 914-2           1          0    DataRow
#> 28            28        Lotus Europa           1          0    DataRow
#> 29            29      Ford Pantera L           1          0    DataRow
#> 30            30        Ferrari Dino           1          0    DataRow
#> 31            31       Maserati Bora           1          0    DataRow
#> 32            32          Volvo 142E           1          0    DataRow
#>    pag_attempted final_pag_result
#> 1          FALSE             <NA>
#> 2          FALSE             <NA>
#> 3          FALSE             <NA>
#> 4          FALSE             <NA>
#> 5          FALSE             <NA>
#> 6          FALSE             <NA>
#> 7          FALSE             <NA>
#> 8          FALSE             <NA>
#> 9          FALSE             <NA>
#> 10         FALSE             <NA>
#> 11         FALSE             <NA>
#> 12         FALSE             <NA>
#> 13         FALSE             <NA>
#> 14         FALSE             <NA>
#> 15         FALSE             <NA>
#> 16         FALSE             <NA>
#> 17         FALSE             <NA>
#> 18         FALSE             <NA>
#> 19         FALSE             <NA>
#> 20         FALSE             <NA>
#> 21         FALSE             <NA>
#> 22         FALSE             <NA>
#> 23         FALSE             <NA>
#> 24         FALSE             <NA>
#> 25         FALSE             <NA>
#> 26         FALSE             <NA>
#> 27         FALSE             <NA>
#> 28         FALSE             <NA>
#> 29         FALSE             <NA>
#> 30         FALSE             <NA>
#> 31         FALSE             <NA>
#> 32         FALSE             <NA>
#> 
#> $cpp_diagnostics
#> character(0)
#> 
#> $col_diagnostics
#>    abs_colnumber self_extent pag_attempted final_pag_result
#> 1              1           4         FALSE             <NA>
#> 2              2           3         FALSE             <NA>
#> 3              3           5         FALSE             <NA>
#> 4              4           3         FALSE             <NA>
#> 5              5           4         FALSE             <NA>
#> 6              6           5         FALSE             <NA>
#> 7              7           5         FALSE             <NA>
#> 8              8           2         FALSE             <NA>
#> 9              9           2         FALSE             <NA>
#> 10            10           4         FALSE             <NA>
#> 11            11           4         FALSE             <NA>
#> 
clws <- propose_column_widths(mpf)
clws[1] <- floor(clws[1] / 3)
dgnost <- diagnose_pagination(mpf, pg_width = 5, pg_height = 3, colwidths = clws)
#> Determining lines required for header content: 0 title and 2 table header lines
#> Determining lines required for footer content: 3 lines
#> Lines per page available for tables rows: 13 (original: 18)
#> --------- ROW-WISE: Checking possible pagination for page 1
#> -> Attempting pagination between 1 and 13 row
#>   FAIL: rows selected for pagination require 29 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 29 lns (13 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 1 and 12 row
#>   FAIL: rows selected for pagination require 27 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 27 lns (12 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 1 and 11 row
#>   FAIL: rows selected for pagination require 25 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 25 lns (11 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 1 and 10 row
#>   FAIL: rows selected for pagination require 23 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 23 lns (10 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 1 and 9 row
#>   FAIL: rows selected for pagination require 21 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 21 lns (9 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 1 and 8 row
#>   FAIL: rows selected for pagination require 19 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 19 lns (8 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 1 and 7 row
#>   FAIL: rows selected for pagination require 17 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 17 lns (7 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 1 and 6 row
#>   FAIL: rows selected for pagination require 15 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 15 lns (6 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 1 and 5 row
#>   OK [13 lines]
#> --------- ROW-WISE: Checking possible pagination for page 2
#> -> Attempting pagination between 6 and 18 row
#>   FAIL: rows selected for pagination require 30 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 30 lns (13 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 6 and 17 row
#>   FAIL: rows selected for pagination require 28 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 28 lns (12 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 6 and 16 row
#>   FAIL: rows selected for pagination require 25 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 25 lns (11 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 6 and 15 row
#>   FAIL: rows selected for pagination require 21 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 21 lns (10 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 6 and 14 row
#>   FAIL: rows selected for pagination require 18 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 18 lns (9 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 6 and 13 row
#>   FAIL: rows selected for pagination require 16 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 16 lns (8 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 6 and 12 row
#>   FAIL: rows selected for pagination require 14 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 14 lns (7 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 6 and 11 row
#>   OK [12 lines]
#> --------- ROW-WISE: Checking possible pagination for page 3
#> -> Attempting pagination between 12 and 24 row
#>   FAIL: rows selected for pagination require 32 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 32 lns (13 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 12 and 23 row
#>   FAIL: rows selected for pagination require 30 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 30 lns (12 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 12 and 22 row
#>   FAIL: rows selected for pagination require 28 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 28 lns (11 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 12 and 21 row
#>   FAIL: rows selected for pagination require 25 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 25 lns (10 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 12 and 20 row
#>   FAIL: rows selected for pagination require 23 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 23 lns (9 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 12 and 19 row
#>   FAIL: rows selected for pagination require 20 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 20 lns (8 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 12 and 18 row
#>   FAIL: rows selected for pagination require 18 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 18 lns (7 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 12 and 17 row
#>   FAIL: rows selected for pagination require 16 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 16 lns (6 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 12 and 16 row
#>   OK [13 lines]
#> --------- ROW-WISE: Checking possible pagination for page 4
#> -> Attempting pagination between 17 and 29 row
#>   FAIL: rows selected for pagination require 32 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 32 lns (13 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 17 and 28 row
#>   FAIL: rows selected for pagination require 29 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 29 lns (12 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 17 and 27 row
#>   FAIL: rows selected for pagination require 27 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 27 lns (11 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 17 and 26 row
#>   FAIL: rows selected for pagination require 24 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 24 lns (10 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 17 and 25 row
#>   FAIL: rows selected for pagination require 22 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 22 lns (9 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 17 and 24 row
#>   FAIL: rows selected for pagination require 19 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 19 lns (8 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 17 and 23 row
#>   FAIL: rows selected for pagination require 17 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 17 lns (7 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 17 and 22 row
#>   FAIL: rows selected for pagination require 15 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 15 lns (6 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 17 and 21 row
#>   OK [12 lines]
#> --------- ROW-WISE: Checking possible pagination for page 5
#> -> Attempting pagination between 22 and 32 row
#>   FAIL: rows selected for pagination require 27 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 27 lns (11 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 22 and 31 row
#>   FAIL: rows selected for pagination require 25 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 25 lns (10 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 22 and 30 row
#>   FAIL: rows selected for pagination require 22 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 22 lns (9 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 22 and 29 row
#>   FAIL: rows selected for pagination require 20 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 20 lns (8 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 22 and 28 row
#>   FAIL: rows selected for pagination require 17 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 17 lns (7 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 22 and 27 row
#>   FAIL: rows selected for pagination require 15 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 15 lns (6 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 22 and 26 row
#>   OK [12 lines]
#> --------- ROW-WISE: Checking possible pagination for page 6
#> -> Attempting pagination between 27 and 32 row
#>   FAIL: rows selected for pagination require 15 lines while only 13 are available from lpp = 18 and 5 header/footers lines.
#>         details: [raw: 15 lns (6 rws), rep. context: 0 lns (0 rws), refs: 0 lns (0) sect. divs: 0 lns].
#> -> Attempting pagination between 27 and 31 row
#>   FAIL: last row had only 0 following siblings, needed 2
#> -> Attempting pagination between 27 and 30 row
#>   FAIL: last row had only 1 following siblings, needed 2
#> -> Attempting pagination between 27 and 29 row
#>   OK [8 lines]
#> --------- ROW-WISE: Checking possible pagination for page 7
#> -> Attempting pagination between 30 and 32 row
#>   OK [7 lines]
#> Adjusted characters per page: 46 [original: 52, table inset: 0, row labels: 6]
#> ========= COLUMN-WISE: Checking possible pagination for page 1
#> -> Attempting pagination between 1 and 11 column
#>   FAIL: selected 11 columns require 107 chars, while only 52 are available. 
#>         details: [raw: 74 chars (11 cols), rep. cols: 0 chars (0 cols), tot. colgap: 33 chars].
#> -> Attempting pagination between 1 and 10 column
#>   FAIL: selected 10 columns require 97 chars, while only 52 are available. 
#>         details: [raw: 67 chars (10 cols), rep. cols: 0 chars (0 cols), tot. colgap: 30 chars].
#> -> Attempting pagination between 1 and 9 column
#>   FAIL: selected 9 columns require 87 chars, while only 52 are available. 
#>         details: [raw: 60 chars (9 cols), rep. cols: 0 chars (0 cols), tot. colgap: 27 chars].
#> -> Attempting pagination between 1 and 8 column
#>   FAIL: selected 8 columns require 79 chars, while only 52 are available. 
#>         details: [raw: 55 chars (8 cols), rep. cols: 0 chars (0 cols), tot. colgap: 24 chars].
#> -> Attempting pagination between 1 and 7 column
#>   FAIL: selected 7 columns require 71 chars, while only 52 are available. 
#>         details: [raw: 50 chars (7 cols), rep. cols: 0 chars (0 cols), tot. colgap: 21 chars].
#> -> Attempting pagination between 1 and 6 column
#>   OK [42 chars]
#> ========= COLUMN-WISE: Checking possible pagination for page 2
#> -> Attempting pagination between 7 and 11 column
#>   OK [32 chars]
try(diagnose_pagination(mpf, pg_width = 1)) # fails
#> Determining lines required for header content: 0 title and 2 table header lines
#> Determining lines required for footer content: 3 lines
#> Lines per page available for tables rows: 85 (original: 90)
#> --------- ROW-WISE: Checking possible pagination for page 1
#> -> Attempting pagination between 1 and 32 row
#>   OK [32 lines]
#> Adjusted characters per page: -27 [original: -8, table inset: 0, row labels: 19]
#> Error in pag_indices_inner(mf_cinfo(mf), rlpp = rcpp, lpp_or_cpp = cpp,  : 
#>   Width of row labels equal to or larger than specified characters per page.
#> Error in diagnose_pagination(mpf, pg_width = 1) : 
#>   Width of row labels equal to or larger than specified characters per page.
```
