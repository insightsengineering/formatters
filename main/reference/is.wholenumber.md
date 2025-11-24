# Check if a value is a whole number

Check if a value is a whole number

## Usage

``` r
is.wholenumber(x, tol = .Machine$double.eps^0.5)
```

## Arguments

- x:

  (`numeric(1)`)  
  a numeric value.

- tol:

  (`numeric(1)`)  
  a precision tolerance.

## Value

`TRUE` if `x` is within `tol` of zero, `FALSE` otherwise.

## Examples

``` r
is.wholenumber(5)
#> [1] TRUE
is.wholenumber(5.00000000000000001)
#> [1] TRUE
is.wholenumber(.5)
#> [1] FALSE
```
