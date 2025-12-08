# Default page number format

If set, the default page number string will appear on the bottom right
of every page of a paginated table. The current `cpp` is used to
position the string.

## Usage

``` r
default_page_number()

set_default_page_number(page_number)
```

## Arguments

- page_number:

  (`string`)  
  single string value to set the page number format. It should be
  formatted similarly to the following format: `"page {i}/{n}"`. `{i}`
  will be replaced with the current page number, and `{n}` will be
  replaced with the total page number. Current `cpp` is used to position
  the string in the bottom right corner.

## Value

The page number format string (`NULL` if not set).

## Examples

``` r
default_page_number()
#> NULL
set_default_page_number("page {i} of {n}")
default_page_number()
#> [1] "page {i} of {n}"
```
