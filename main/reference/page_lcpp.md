# Determine lines per page (LPP) and characters per page (CPP) based on font and page type

Determine lines per page (LPP) and characters per page (CPP) based on
font and page type

## Usage

``` r
page_lcpp(
  page_type = page_types(),
  landscape = FALSE,
  font_family = "Courier",
  font_size = 8,
  lineheight = 1,
  margins = c(top = 0.5, bottom = 0.5, left = 0.75, right = 0.75),
  pg_width = NULL,
  pg_height = NULL,
  fontspec = font_spec(font_family, font_size, lineheight)
)
```

## Arguments

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

- margins:

  (`numeric(4)`)  
  named numeric vector containing `"bottom"`, `"left"`, `"top"`, and
  `"right"` margins in inches. Defaults to `.5` inches for both vertical
  margins and `.75` for both horizontal margins.

- pg_width:

  (`numeric(1)`)  
  page width in inches.

- pg_height:

  (`numeric(1)`)  
  page height in inches.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

## Value

A named list containing LPP (lines per page) and CPP (characters per
page) elements suitable for use by the pagination machinery.

## Examples

``` r
page_lcpp()
#> $cpp
#> [1] 105
#> 
#> $lpp
#> [1] 90
#> 
page_lcpp(font_size = 10)
#> $cpp
#> [1] 84
#> 
#> $lpp
#> [1] 72
#> 
page_lcpp("a4", font_size = 10)
#> $cpp
#> [1] 81
#> 
#> $lpp
#> [1] 76
#> 

page_lcpp(margins = c(top = 1, bottom = 1, left = 1, right = 1))
#> $cpp
#> [1] 97
#> 
#> $lpp
#> [1] 81
#> 
page_lcpp(pg_width = 10, pg_height = 15)
#> $cpp
#> [1] 127
#> 
#> $lpp
#> [1] 126
#> 
```
