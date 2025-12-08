# `%||%` (if length-0) alternative operator

`%||%` (if length-0) alternative operator

## Usage

``` r
a %||% b
```

## Arguments

- a:

  (`ANY`)  
  element to select *only* if it is not of length 0.

- b:

  (`ANY`)  
  element to select if `a` has length 0.

## Value

`a` if it is not of length 0, otherwise `b`.

## Examples

``` r
6 %||% 10
#> [1] 6

character() %||% "hi"
#> [1] "hi"

NULL %||% "hi"
#> [1] "hi"
```
