# Check if a format or alignment is supported

Utility functions for checking formats and alignments.

## Usage

``` r
is_valid_format(x, stop_otherwise = FALSE)

check_aligns(algn)
```

## Arguments

- x:

  (`string` or `function`)  
  format string or an object returned by
  [`sprintf_format()`](https://insightsengineering.github.io/formatters/reference/sprintf_format.md)

- stop_otherwise:

  (`flag`)  
  whether an error should be thrown if `x` is not a valid format.

- algn:

  (`character`)  
  a character vector that indicates the requested cell alignments.

## Value

- `is_valid_format` returns `TRUE` if `x` is `NULL`, a supported format
  string, or a function, and `FALSE` otherwise.

&nbsp;

- `check_aligns` returns `TRUE` if the provided alignments are
  supported, otherwise, an error is thrown.

## Note

If `x` is a function, no check is performed to verify that it returns a
valid format.

## Examples

``` r
is_valid_format("xx.x")
#> [1] TRUE
is_valid_format("fakeyfake")
#> [1] FALSE

check_aligns(c("decimal", "dec_right"))
```
