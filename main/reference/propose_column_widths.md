# Propose column widths based on the `MatrixPrintForm` of an object

Row names are also considered a column for the output.

## Usage

``` r
propose_column_widths(
  x,
  indent_size = 2,
  fontspec = font_spec(),
  round_type = c("iec", "sas")
)
```

## Arguments

- x:

  (`ANY`)  
  a `MatrixPrintForm` object, or an object with a `matrix_form` method.

- indent_size:

  (`numeric(1)`)  
  indent size, in characters. Ignored when `x` is already a
  `MatrixPrintForm` object in favor of information there.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

- round_type:

  (`"iec"` or `"sas"`)  
  the type of rounding to perform. iec, the default, peforms rounding
  compliant with IEC 60559 (see details), while sas performs
  nearest-value rounding consistent with rounding within SAS.

## Value

A vector of column widths based on the content of `x` for use in
printing and pagination.

## Examples

``` r
mf <- basic_matrix_form(mtcars)
propose_column_widths(mf)
#> rnms  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>   19    4    3    5    3    4    5    5    2    2    4    4 
```
