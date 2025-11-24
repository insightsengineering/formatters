# Export as PDF

The PDF output from this function is based on the ASCII output created
with
[`toString()`](https://insightsengineering.github.io/formatters/reference/tostring.md).

## Usage

``` r
export_as_pdf(
  x,
  file,
  page_type = "letter",
  landscape = FALSE,
  pg_width = page_dim(page_type)[if (landscape) 2 else 1],
  pg_height = page_dim(page_type)[if (landscape) 1 else 2],
  width = lifecycle::deprecated(),
  height = lifecycle::deprecated(),
  margins = c(4, 4, 4, 4),
  min_siblings = 2,
  font_family = "Courier",
  font_size = 8,
  fontsize = font_size,
  lineheight = 1.2,
  paginate = TRUE,
  page_num = default_page_number(),
  lpp = NULL,
  cpp = NULL,
  hsep = "-",
  indent_size = 2,
  rep_cols = NULL,
  tf_wrap = TRUE,
  max_width = NULL,
  colwidths = NULL,
  fontspec = font_spec(font_family, font_size, lineheight),
  ttype_ok = FALSE,
  round_type = c("iec", "sas")
)
```

## Arguments

- x:

  (`ANY`)  
  a table-like object to export. Must have an applicable `matrix_form`
  method.

- file:

  (`string`)  
  file to write to, must have `.pdf` extension.

- page_type:

  (`string`)  
  name of a page type. See
  [`page_types`](https://insightsengineering.github.io/formatters/reference/page_types.md).
  Ignored when `pg_width` and `pg_height` are set directly.

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

- width:

  **\[deprecated\]** Please use the `pg_width` argument or specify
  `page_type` instead.

- height:

  **\[deprecated\]** Please use the `pg_height` argument or specify
  `page_type` instead.

- margins:

  (`numeric(4)`)  
  the number of lines/characters of the margin on the bottom, left, top,
  and right sides of the page, respectively.

- min_siblings:

  (`numeric`)  
  minimum sibling rows which must appear on either side of pagination
  row for a mid-subtable split to be valid. Defaults to 2 for tables. It
  is automatically turned off (set to 0) for listings.

- font_family:

  (`string`)  
  name of a font family. An error will be thrown if the family named is
  not monospaced. Defaults to `"Courier"`.

- font_size:

  (`numeric(1)`)  
  font size. Defaults to `12`.

- fontsize:

  **\[deprecated\]** Please use the `font_size` argument instead.

- lineheight:

  (`numeric(1)`)  
  line height. Defaults to `1`.

- paginate:

  (`flag`)  
  whether pagination should be performed. Defaults to `TRUE` if page
  size is specified (including the default).

- page_num:

  (`string`)  
  placeholder string for page numbers. See
  [default_page_number](https://insightsengineering.github.io/formatters/reference/default_page_number.md)
  for more information. Defaults to `NULL`.

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

- hsep:

  (`string`)  
  character to repeat to create header/body separator line. If `NULL`,
  the object value will be used. If `" "`, an empty separator will be
  printed. See
  [`default_hsep()`](https://insightsengineering.github.io/formatters/reference/default_horizontal_sep.md)
  for more information.

- indent_size:

  (`numeric(1)`)  
  indent size, in characters. Ignored when `x` is already a
  `MatrixPrintForm` object in favor of information there.

- rep_cols:

  (`numeric(1)`)  
  number of *columns* (not including row labels) to be repeated on every
  page. Defaults to 0.

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

- colwidths:

  (`numeric`)  
  vector of column widths (in characters) for use in vertical
  pagination.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

- ttype_ok:

  (`logical(1)`)  
  should truetype (non-monospace) fonts be allowed via `fontspec`.
  Defaults to `FALSE`. This parameter is primarily for internal testing
  and generally should not be set by end users.

- round_type:

  (`"iec"` or `"sas"`)  
  the type of rounding to perform. iec, the default, peforms rounding
  compliant with IEC 60559 (see details), while sas performs
  nearest-value rounding consistent with rounding within SAS.

## Details

By default, pagination is performed with default `cpp` and `lpp` defined
by specified page dimensions and margins. User-specified `lpp` and `cpp`
values override this, and should be used with caution.

Title and footer materials are also word-wrapped by default (unlike when
printed to the terminal), with `cpp` (as defined above) as the default
`max_width`.

## See also

[`export_as_txt()`](https://insightsengineering.github.io/formatters/reference/export_as_txt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
tf <- tempfile(fileext = ".pdf")
export_as_pdf(basic_matrix_form(mtcars), file = tf, pg_height = 4)

tf <- tempfile(fileext = ".pdf")
export_as_pdf(basic_matrix_form(mtcars), file = tf, lpp = 8)
} # }
```
