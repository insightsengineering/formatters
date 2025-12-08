# Propose column widths based on the `MatrixPrintForm` of an object

Row names are also considered a column for the output.

## Usage

``` r
propose_column_widths(
  x,
  indent_size = 2,
  fontspec = font_spec(),
  round_type = obj_round_type(x)
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
