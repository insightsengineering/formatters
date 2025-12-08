# Wrap a string to a precise width

Core wrapping functionality that preserves whitespace. Newline character
`"\n"` is not supported by core functionality
[`stringi::stri_wrap()`](https://rdrr.io/pkg/stringi/man/stri_wrap.html).
This is usually solved beforehand by
[`matrix_form()`](https://insightsengineering.github.io/formatters/reference/matrix_form.md).
If the width is smaller than any large word, these will be truncated
after `width` characters. If the split leaves trailing groups of empty
spaces, they will be dropped.

## Usage

``` r
wrap_string(str, width, collapse = NULL, fontspec = font_spec())

wrap_txt(str, width, collapse = NULL, fontspec = font_spec())
```

## Arguments

- str:

  (`string`, `character`, or `list`)  
  string to be wrapped. If it is a `vector` or a `list`, it will be
  looped as a `list` and returned with `unlist(use.names = FALSE)`.

- width:

  (`numeric(1)`)  
  width, in characters, that the text should be wrapped to.

- collapse:

  (`string` or `NULL`)  
  collapse character used to separate segments of words that have been
  split and should be pasted together. This is usually done internally
  with `"\n"` to update the wrapping along with other internal values.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

## Value

A string if `str` is one element and if `collapse = NULL`. Otherwise, a
list of elements (if `length(str) > 1`) that can contain strings or
vectors of characters (if `collapse = NULL`).

## Details

Word wrapping happens similarly to
[`stringi::stri_wrap()`](https://rdrr.io/pkg/stringi/man/stri_wrap.html)
with the following difference: individual words which are longer than
`max_width` are broken up in a way that fits with other word wrapping.

## Functions

- `wrap_txt()`: Deprecated function. Please use `wrap_string()` instead.

## Examples

``` r
str <- list(
  "  , something really  \\tnot  very good", # \t needs to be escaped
  "  but I keep it12   "
)
wrap_string(str, 5, collapse = "\n")
#> [1] "  ,\nsomet\nhing\nreall\ny  \\t\nnot \nvery\ngood"
#> [2] " \nbut I\nkeep\nit12"                             

wrap_txt(str, 5, collapse = NULL)
#>  [1] "  ,"    "somet"  "hing"   "reall"  "y  \\t" "not "   "very"   "good"  
#>  [9] " "      "but I"  "keep"   "it12"  
```
