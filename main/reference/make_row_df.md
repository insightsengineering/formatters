# Make row layout summary data frames for use during pagination

All relevant information about table rows (e.g. indentations) is
summarized in a `data.frame`. This function works **only** on `rtables`
and `rlistings` objects, and not on their `print` counterparts (like
[`MatrixPrintForm`](https://insightsengineering.github.io/formatters/reference/MatrixPrintForm.md)).

## Usage

``` r
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
  max_width = NULL,
  fontspec = font_spec(),
  col_gap = 3L,
  round_type = obj_round_type(tt)
)

# S4 method for class 'MatrixPrintForm'
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
  max_width = NULL,
  fontspec = font_spec(),
  col_gap = mf_colgap(tt) %||% 3L,
  round_type = obj_round_type(tt)
)
```

## Arguments

- tt:

  (`ANY`)  
  object representing the table-like object to be summarized.

- colwidths:

  (`numeric`)  
  internal detail, do not set manually.

- visible_only:

  (`flag`)  
  should only visible aspects of the table structure be reflected in
  this summary. Defaults to `TRUE`. May not be supported by all methods.

- rownum:

  (`numeric(1)`)  
  internal detail, do not set manually.

- indent:

  (`integer(1)`)  
  internal detail, do not set manually.

- path:

  (`character`)  
  path to the (sub)table represented by `tt`. Defaults to
  [`character()`](https://rdrr.io/r/base/character.html).

- incontent:

  (`flag`)  
  internal detail, do not set manually.

- repr_ext:

  (`integer(1)`)  
  internal detail, do not set manually.

- repr_inds:

  (`integer`)  
  internal detail, do not set manually.

- sibpos:

  (`integer(1)`)  
  internal detail, do not set manually.

- nsibs:

  (`integer(1)`)  
  internal detail, do not set manually.

- max_width:

  (`numeric(1)` or `NULL`)  
  maximum width for title/footer materials.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

- col_gap:

  (`numeric(1)`)  
  the gap to be assumed between columns, in number of spaces with font
  specified by `fontspec`.

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

## Value

A `data.frame` of row/column-structure information used by the
pagination machinery.

## Details

When `visible_only` is `TRUE` (the default), methods should return a
`data.frame` with exactly one row per visible row in the table-like
object. This is useful when reasoning about how a table will print, but
does not reflect the full pathing space of the structure (though the
paths which are given will all work as is).

If supported, when `visible_only` is `FALSE`, every structural element
of the table (in row-space) will be reflected in the returned
`data.frame`, meaning the full pathing-space will be represented but
some rows in the layout summary will not represent printed rows in the
table as it is displayed.

Most arguments beyond `tt` and `visible_only` are present so that
`make_row_df` methods can call `make_row_df` recursively and retain
information, and should not be set during a top-level call.

## Note

The technically present root tree node is excluded from the summary
returned by both `make_row_df` and `make_col_df` (see relevant functions
in`rtables`), as it is the row/column structure of `tt` and thus not
useful for pathing or pagination.

## Examples

``` r
# Expected error with matrix_form. For real case examples consult {rtables} documentation
mf <- basic_matrix_form(iris)
# make_row_df(mf) # Use table obj instead
```
