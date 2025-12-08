# Decimal alignment

Aligning decimal values of string matrix. Allowed alignments are:
`dec_left`, `dec_right`, and `decimal`.

## Usage

``` r
decimal_align(string_mat, align_mat)
```

## Arguments

- string_mat:

  (`character matrix`)  
  "string" matrix component of `MatrixPrintForm` object.

- align_mat:

  (`character matrix`)  
  "aligns" matrix component of `MatrixPrintForm` object. Should contain
  either `dec_left`, `dec_right`, or `decimal` for values to be decimal
  aligned.

## Value

A processed string matrix of class `MatrixPrintForm` with
decimal-aligned values.

## Details

Left and right decimal alignment (`dec_left` and `dec_right`) differ
from center decimal alignment (`decimal`) only when there is padding
present. This may occur if column widths are set wider via parameters
`widths` in `toString` or `colwidths` in `paginate_*`. More commonly, it
also occurs when column names are wider. Cell wrapping is not supported
when decimal alignment is used.

## See also

[`toString()`](https://insightsengineering.github.io/formatters/reference/tostring.md),
[`MatrixPrintForm()`](https://insightsengineering.github.io/formatters/reference/MatrixPrintForm.md)

## Examples

``` r
dfmf <- basic_matrix_form(mtcars[1:5, ])
aligns <- mf_aligns(dfmf)
aligns[, -c(1)] <- "dec_left"
decimal_align(mf_strings(dfmf), aligns)
#>      rnms                mpg    cyl   disp   hp    drat   wt      qsec    vs  
#> [1,] ""                  "mpg"  "cyl" "disp" "hp"  "drat" "wt"    "qsec"  "vs"
#> [2,] "Mazda RX4"         "21  " "6"   "160"  "110" "3.9 " "2.62 " "16.46" "0" 
#> [3,] "Mazda RX4 Wag"     "21  " "6"   "160"  "110" "3.9 " "2.875" "17.02" "0" 
#> [4,] "Datsun 710"        "22.8" "4"   "108"  " 93" "3.85" "2.32 " "18.61" "1" 
#> [5,] "Hornet 4 Drive"    "21.4" "6"   "258"  "110" "3.08" "3.215" "19.44" "1" 
#> [6,] "Hornet Sportabout" "18.7" "8"   "360"  "175" "3.15" "3.44 " "17.02" "0" 
#>      am   gear   carb  
#> [1,] "am" "gear" "carb"
#> [2,] "1"  "4"    "4"   
#> [3,] "1"  "4"    "4"   
#> [4,] "1"  "4"    "1"   
#> [5,] "0"  "3"    "1"   
#> [6,] "0"  "3"    "2"   
```
