# Converts a (possibly compound) value into a string using the `format` information

Converts a (possibly compound) value into a string using the `format`
information

## Usage

``` r
format_value(
  x,
  format = NULL,
  output = c("ascii", "html"),
  na_str = "NA",
  round_type = c("iec", "sas")
)
```

## Arguments

- x:

  (`ANY`)  
  the value to be formatted.

- format:

  (`string` or `function`)  
  the format label (string) or formatter function to apply to `x`.

- output:

  (`string`)  
  output type.

- na_str:

  (`character`)  
  character vector to display when the values of `x` are missing. If
  only one string is provided, it is applied for all missing values.
  Defaults to `"NA"`.

- round_type:

  (`"iec"` or `"sas"`)  
  the type of rounding to perform. iec, the default, peforms rounding
  compliant with IEC 60559 (see details), while sas performs
  nearest-value rounding consistent with rounding within SAS.

## Value

Formatted text representing the cell `x`.

## Details

A length-zero value for `na_str` will be interpreted as `"NA"`.

## See also

[`round_fmt()`](https://insightsengineering.github.io/formatters/reference/round_fmt.md)

## Examples

``` r
x <- format_value(pi, format = "xx.xx")
x
#> [1] "3.14"

format_value(x, output = "ascii")
#> [1] "3.14"

# na_str works with multiple values
format_value(c(NA, 1, NA), format = "xx.x (xx.x - xx.x)", na_str = c("NE", "<missing>"))
#> [1] "NE (1.0 - <missing>)"
```
