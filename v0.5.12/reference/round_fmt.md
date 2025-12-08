# Round and prepare a value for display

This function is used within
[`format_value()`](https://insightsengineering.github.io/formatters/reference/format_value.md)
to prepare numeric values within cells for formatting and display.

## Usage

``` r
valid_round_type

round_fmt(x, digits, na_str = "NA", round_type = valid_round_type)
```

## Format

An object of class `character` of length 3.

## Arguments

- x:

  (`numeric(1)`)  
  value to format.

- digits:

  (`numeric(1)`)  
  number of digits to round to, or `NA` to convert to a character value
  with no rounding.

- na_str:

  (`string`)  
  the value to return if `x` is `NA`.

- round_type:

  (`string`)  
  .  
  The type of rounding to perform. Allowed values: (`"iec"`, `"iec_mod"`
  or `"sas"`)  
  iec, the default, and iec_mod performs rounding compliant with IEC
  60559 (see notes in `round_fmt()`), while sas performs nearest-value
  rounding consistent with rounding within SAS.  
  In addition, the rounding of a negative number that rounds to zero
  will be presented as 0 (with the appropriate number of trailing zeros)
  for both `sas` and `iec_mod`, while for `iec`, it will be presented as
  -0 (with the appropriate number of trailing zeros).

## Value

A character value representing the value after rounding, containing any
trailing zeros required to display *exactly* `digits` elements.

## Details

This function combines rounding behavior with the strict decimal display
of [`sprintf()`](https://rdrr.io/r/base/sprintf.html). By default, R's
standards-compliant [`round()`](https://rdrr.io/r/base/Round.html)
function (see the Details section of that documentation) is used. The
exact behavior is as follows:

1.  If `x` is `NA`, the value of `na_str` is returned.

2.  If `x` is non-`NA` but `digits` is `NA`, `x` is converted to a
    character and returned.

3.  If `x` and `digits` are both non-NA,
    [`round()`](https://rdrr.io/r/base/Round.html) is called first, and
    then [`sprintf()`](https://rdrr.io/r/base/sprintf.html) is used to
    convert the rounded value to a character with the appropriate number
    of trailing zeros enforced.

## Note

This differs from the base R
[`round()`](https://rdrr.io/r/base/Round.html) function in that `NA`
digits indicate `x` should be converted to character and returned
unchanged whereas `round(x, digits=NA)` returns `NA` for all values of
`x`.

This behavior will differ from `as.character(round(x, digits = digits))`
in the case where there are not at least `digits` significant digits
after the decimal that remain after rounding. It *may* differ from
`sprintf("\%.Nf", x)` for values ending in `5` after the decimal place
on many popular operating systems due to `round`'s stricter adherence to
the IEC 60559 standard, particularly for R versions \> 4.0.0 (see
warning in [`round()`](https://rdrr.io/r/base/Round.html)
documentation).

## See also

[`format_value()`](https://insightsengineering.github.io/formatters/reference/format_value.md),
[`round()`](https://rdrr.io/r/base/Round.html),
[`sprintf()`](https://rdrr.io/r/base/sprintf.html)

## Examples

``` r
round_fmt(0, digits = 3)
#> [1] "0.000"
round_fmt(.395, digits = 2)
#> [1] "0.40"
round_fmt(NA, digits = 1)
#> [1] "NA"
round_fmt(NA, digits = 1, na_str = "-")
#> [1] "-"
round_fmt(2.765923, digits = NA)
#> [1] "2.765923"
round_fmt(0.845, digits = 2)
#> [1] "0.84"
round_fmt(0.845, digits = 2, round_type = "sas")
#> [1] "0.85"
round_fmt(-0.001, digits = 2, round_type = "iec")
#> [1] "-0.00"
round_fmt(-0.001, digits = 2, round_type = "sas")
#> [1] "0.00"
round_fmt(-0.001, digits = 2, round_type = "iec_mod")
#> [1] "0.00"
```
