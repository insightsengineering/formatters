# Font size specification

Font size specification

## Usage

``` r
font_spec(font_family = "Courier", font_size = 8, lineheight = 1)
```

## Arguments

- font_family:

  (`character(1)`)  
  font family to use during string width and lines-per-page
  calculations. You can specify "Times New Roman" as "Times" or "serif",
  regardless of OS. Beyond that, see `family` entry in
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html) for details.

- font_size:

  (`numeric(1)`)  
  font size to use during string width calculations and lines-per-page
  calculations.

- lineheight:

  (`numeric(1)`)  
  line height to use during lines-per-page calculations.

## Details

Passing the output of this constructor to the rendering or pagination
machinery defines a font for use when calculating word wrapping and
pagination.

## Note

Specifying font in this way to, e.g.,
[`export_as_txt()`](https://insightsengineering.github.io/formatters/reference/export_as_txt.md)
or
[`toString()`](https://insightsengineering.github.io/formatters/reference/tostring.md)
will not affect the font size of the output, as these are both raw text
formats.
[`export_as_pdf()`](https://insightsengineering.github.io/formatters/reference/export_as_pdf.md)
will use the specified font.

## See also

[`nchar_ttype()`](https://insightsengineering.github.io/formatters/reference/nchar_ttype.md),
[`toString()`](https://insightsengineering.github.io/formatters/reference/tostring.md),
[`pagination_algo`](https://insightsengineering.github.io/formatters/reference/pagination_algo.md),
[`export_as_pdf()`](https://insightsengineering.github.io/formatters/reference/export_as_pdf.md)

## Examples

``` r
fspec <- font_spec("Courier", 8, 1)

lets <- paste(letters, collapse = "")

nchar_ttype(lets, fspec)
#> [1] 26

fspec2 <- font_spec("Times", 8, 1)

nchar_ttype(lets, fspec2)
#> [1] 48
```
