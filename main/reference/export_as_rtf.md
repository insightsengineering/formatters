# Export as RTF

Experimental export to the rich text format (RTF) format.

## Usage

``` r
export_as_rtf(
  x,
  file = NULL,
  colwidths = NULL,
  page_type = "letter",
  pg_width = page_dim(page_type)[if (landscape) 2 else 1],
  pg_height = page_dim(page_type)[if (landscape) 1 else 2],
  landscape = FALSE,
  margins = c(bottom = 0.5, left = 0.75, top = 0.5, right = 0.75),
  font_family = "Courier",
  font_size = 8,
  lineheight = 1,
  fontspec = font_spec(font_family, font_size, lineheight),
  paginate = TRUE,
  round_type = obj_round_type(x),
  ...
)
```

## Arguments

- x:

  (`ANY`)  
  a table-like object to export. Must have an applicable `matrix_form`
  method.

- file:

  (`string` or `NULL`)  
  if non-`NULL`, the path to write a text file to containing `x`
  rendered as ASCII text.

- colwidths:

  (`numeric`)  
  vector of column widths (in characters) for use in vertical
  pagination.

- page_type:

  (`string`)  
  name of a page type. See
  [`page_types`](https://insightsengineering.github.io/formatters/reference/page_types.md).
  Ignored when `pg_width` and `pg_height` are set directly.

- pg_width:

  (`numeric(1)`)  
  page width in inches.

- pg_height:

  (`numeric(1)`)  
  page height in inches.

- landscape:

  (`flag`)  
  whether the dimensions of `page_type` should be inverted for landscape
  orientation. Defaults to `FALSE`, ignored when `pg_width` and
  `pg_height` are set directly.

- margins:

  (`numeric(4)`)  
  named numeric vector containing `"bottom"`, `"left"`, `"top"`, and
  `"right"` margins in inches. Defaults to `.5` inches for both vertical
  margins and `.75` for both horizontal margins.

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

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

- paginate:

  (`flag`)  
  whether pagination should be performed. Defaults to `TRUE` if page
  size is specified (including the default).

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

- ...:

  additional parameters passed to
  [`paginate_to_mpfs()`](https://insightsengineering.github.io/formatters/reference/paginate_indices.md).

## Details

RTF export occurs via the following steps:

- The table is paginated to the specified page size (vertically and
  horizontally).

- Each separate page is converted to a `MatrixPrintForm` object and then
  to RTF-encoded text.

- Separate RTF text chunks are combined and written to a single RTF
  file.

Conversion of `MatrixPrintForm` objects to RTF is done via
[`mpf_to_rtf()`](https://insightsengineering.github.io/formatters/reference/mpf_to_rtf.md).
