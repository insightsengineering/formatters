# Specify text format via a `sprintf` format string

Specify text format via a `sprintf` format string

## Usage

``` r
sprintf_format(format)
```

## Arguments

- format:

  (`string`)  
  a format string passed to
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html).

## Value

A formatting function which wraps and applies the specified
`sprintf`-style format to string `format`.

## See also

[`sprintf()`](https://rdrr.io/r/base/sprintf.html)

## Examples

``` r
fmtfun <- sprintf_format("(N=%i")
format_value(100, format = fmtfun)
#> [1] "(N=100"

fmtfun2 <- sprintf_format("%.4f - %.2f")
format_value(list(12.23456, 2.724))
#> [1] "12.23456, 2.724"
```
