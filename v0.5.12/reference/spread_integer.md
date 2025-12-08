# Spread an integer to a given length

Spread an integer to a given length

## Usage

``` r
spread_integer(x, len)
```

## Arguments

- x:

  (`integer(1)`)  
  number to spread.

- len:

  (`integer(1)`)  
  number of times to repeat `x`.

## Value

If `x` is a scalar whole number value (see
[`is.wholenumber()`](https://insightsengineering.github.io/formatters/reference/is.wholenumber.md)),
the value `x` is repeated `len` times. Otherwise, an error is thrown.

## Examples

``` r
spread_integer(3, 1)
#> [1] 3
spread_integer(0, 3)
#> [1] 0 0 0
spread_integer(1, 3)
#> [1] 1 0 0
spread_integer(2, 3)
#> [1] 1 1 0
spread_integer(3, 3)
#> [1] 1 1 1
spread_integer(4, 3)
#> [1] 2 1 1
spread_integer(5, 3)
#> [1] 2 2 1
spread_integer(6, 3)
#> [1] 2 2 2
spread_integer(7, 3)
#> [1] 3 2 2
```
