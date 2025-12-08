# Supported named page types

List supported named page types.

## Usage

``` r
page_types()

page_dim(page_type)
```

## Arguments

- page_type:

  (`string`)  
  the name of a page size specification. Call `page_types()` for
  supported values.

## Value

- `page_types` returns a character vector of supported page types

- `page_dim` returns the dimensions (width, then height) of the selected
  page type.

## Examples

``` r
page_types()
#> [1] "letter" "a4"     "legal" 
page_dim("a4")
#> [1]  8.27 11.69
```
