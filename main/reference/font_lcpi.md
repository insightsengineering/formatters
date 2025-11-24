# Calculate lines per inch and characters per inch for font

Calculate lines per inch and characters per inch for font

## Usage

``` r
font_lcpi(
  font_family = "Courier",
  font_size = 8,
  lineheight = 1,
  fontspec = font_spec(font_family, font_size, lineheight)
)
```

## Arguments

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

## Value

A named list with `cpi` and `lpi`, the characters and lines per inch,
respectively.

## Details

This function opens a PDF graphics device, writes to a temporary file,
then utilizes
[`grid::convertWidth()`](https://rdrr.io/r/grid/grid-defunct.html) and
[`grid::convertHeight()`](https://rdrr.io/r/grid/grid-defunct.html) to
calculate lines per inch and characters per inch for the specified font
family, size, and line height.

An error is thrown if the font is not monospaced (determined by
comparing the effective widths of the `M` and `.` glyphs).

## Examples

``` r
font_lcpi <- getFromNamespace("font_lcpi", "formatters")

font_lcpi()
#> $cpi
#> [1] 15
#> 
#> $lpi
#> [1] 9
#> 
font_lcpi(font_size = 8)
#> $cpi
#> [1] 15
#> 
#> $lpi
#> [1] 9
#> 
font_lcpi(font_size = 8, lineheight = 1.1)
#> $cpi
#> [1] 15
#> 
#> $lpi
#> [1] 8.181818
#> 
```
