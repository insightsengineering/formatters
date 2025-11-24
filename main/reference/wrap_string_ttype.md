# wrap string given a Truetype font

wrap string given a Truetype font

## Usage

``` r
split_word_ttype(str, width, fontspec, min_ok_chars)

wrap_string_ttype(
  str,
  width,
  fontspec,
  collapse = NULL,
  min_ok_chars = min(floor(nchar(str)/2), 4, floor(width/2)),
  wordbreak_ok = TRUE
)
```

## Arguments

- str:

  (`string`, `character`, or `list`)  
  string to be wrapped. If it is a `vector` or a `list`, it will be
  looped as a `list` and returned with `unlist(use.names = FALSE)`.

- width:

  (`numeric(1)`)  
  width, in characters, that the text should be wrapped to.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

- min_ok_chars:

  (`numeric(1)`)  
  number of minimum characters that remain on either side when a word is
  split.

- collapse:

  (`string` or `NULL`)  
  collapse character used to separate segments of words that have been
  split and should be pasted together. This is usually done internally
  with `"\n"` to update the wrapping along with other internal values.

- wordbreak_ok:

  (`logical(1)`)  
  should breaking within a word be allowed? If, `FALSE`, attempts to
  wrap a string to a width narrower than its widest word will result in
  an error.

## Value

`str`, broken up into a word-wrapped vector
