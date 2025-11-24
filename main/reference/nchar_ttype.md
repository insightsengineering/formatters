# Calculate font-specific string width

This function returns the width of each element `x` *as a multiple of
the width of the space character for in declared font*, rounded up to
the nearest integer. This is used extensively in the text rendering
([`toString()`](https://insightsengineering.github.io/formatters/reference/tostring.md))
and pagination machinery for calculating word wrapping, default column
widths, lines per page, etc.

## Usage

``` r
nchar_ttype(
  x,
  fontspec = font_spec(),
  tol = sqrt(.Machine$double.eps),
  raw = FALSE
)
```

## Arguments

- x:

  (`character`)  
  the string(s) to calculate width(s) for.

- fontspec:

  (`font_spec` or `NULL`)  
  if non-NULL, the font to use for the calculations (as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md)).
  Defaults to "Courier", which is a monospace font. If NULL, the width
  will be returned in number of characters by calling `nchar` directly.

- tol:

  (`numeric(1)`)  
  the tolerance to use when determining if a multiple needs to be
  rounded up to the next integer. See Details.

- raw:

  (`logical(1)`)  
  whether unrounded widths should be returned. Defaults to `FALSE`.

## Details

String width is defined in terms of spaces within the specified font.
For monospace fonts, this definition collapses to the number of
characters in the string
([`nchar()`](https://rdrr.io/r/base/nchar.html)), but for truetype fonts
it does not.

For `raw = FALSE`, non-integer values (the norm in a truetype setting)
for the number of spaces a string takes up is rounded up, *unless the
multiple is less than `tol` above the last integer before it*. E.g., if
`k - num_spaces < tol` for an integer `k`, `k` is returned instead of
`k+1`.

## See also

[`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md)

## Examples

``` r
nchar_ttype("hi there!")
#> [1] 9

nchar_ttype("hi there!", font_spec("Times"))
#> [1] 14
```
