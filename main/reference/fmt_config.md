# Format configuration

Format configuration

## Usage

``` r
fmt_config(format = NULL, na_str = "NA", align = "center")
```

## Arguments

- format:

  (`string` or `function`)  
  a format label (string) or formatter function.

- na_str:

  (`string`)  
  string that should be displayed in place of missing values.

- align:

  (`string`)  
  alignment values should be rendered with.

## Value

An object of class `fmt_config` which contains the following elements:

- `format`

- `na_str`

- `align`

## Examples

``` r
fmt_config(format = "xx.xx", na_str = "-", align = "left")
#> An object of class "fmt_config"
#> Slot "format":
#> [1] "xx.xx"
#> 
#> Slot "format_na_str":
#> [1] "-"
#> 
#> Slot "align":
#> [1] "left"
#> 
fmt_config(format = "xx.xx - xx.xx", align = "right")
#> An object of class "fmt_config"
#> Slot "format":
#> [1] "xx.xx - xx.xx"
#> 
#> Slot "format_na_str":
#> [1] "NA"
#> 
#> Slot "align":
#> [1] "right"
#> 
```
