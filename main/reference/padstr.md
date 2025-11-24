# Pad a string and align within string

Pad a string and align within string

## Usage

``` r
padstr(x, n, just = list_valid_aligns(), fontspec = font_spec())
```

## Arguments

- x:

  (`string`)  
  a string.

- n:

  (`integer(1)`)  
  number of characters in the output string. If `n < nchar(x)`, an error
  is thrown.

- just:

  (`string`)  
  text alignment justification to use. Defaults to `"center"`. Must be
  one of `"center"`, `"right"`, `"left"`, `"dec_right"`, `"dec_left"`,
  or `"decimal"`.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

## Value

`x`, padded to be a string of length `n`.

## Examples

``` r
padstr("abc", 3)
#> [1] "abc"
padstr("abc", 4)
#> [1] "abc "
padstr("abc", 5)
#> [1] "abc  "
padstr("abc", 5, "left")
#> [1] "abc  "
padstr("abc", 5, "right")
#> [1] "  abc"

if (FALSE) { # \dontrun{
# Expect error: "abc" has more than 1 characters
padstr("abc", 1)
} # }
```
