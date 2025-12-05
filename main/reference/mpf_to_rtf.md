# Transform `MatrixPrintForm` to RTF

Experimental export to rich text format (RTF) via the `r2rtf` package.

## Usage

``` r
mpf_to_rtf(
  mpf,
  colwidths = NULL,
  page_type = "letter",
  pg_width = page_dim(page_type)[if (landscape) 2 else 1],
  pg_height = page_dim(page_type)[if (landscape) 1 else 2],
  landscape = FALSE,
  margins = c(4, 4, 4, 4),
  font_family = "Courier",
  font_size = 8,
  lineheight = 1,
  fontspec = font_spec(font_family, font_size, lineheight),
  round_type = obj_round_type(mpf),
  ...
)
```

## Arguments

- mpf:

  (`MatrixPrintForm`)  
  a `MatrixPrintForm` object.

- colwidths:

  (`numeric`)  
  column widths.

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

- round_type:

  (`string`)  
  The type of rounding to perform. Allowed values: (`"iec"`, `"iec_mod"`
  or `"sas"`) See
  [`round_fmt()`](https://insightsengineering.github.io/formatters/reference/round_fmt.md)
  for details.

- ...:

  additional parameters passed to individual methods.

## Value

An RTF object.

## Details

This function provides a low-level coercion of a `MatrixPrintForm`
object into text containing the corresponding table in RTF. Currently,
no pagination is done at this level, and should be done prior to calling
this function, though that may change in the future.
