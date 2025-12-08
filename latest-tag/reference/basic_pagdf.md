# Basic/spoof pagination info data frame

Returns a minimal pagination info `data.frame` (with no info on
siblings, footnotes, etc.).

## Usage

``` r
basic_pagdf(
  rnames,
  labs = rnames,
  rnums = seq_along(rnames),
  extents = 1L,
  rclass = "DataRow",
  parent_path = NULL,
  paths = lapply(rnames, function(x) c(parent_path, x)),
  fontspec = font_spec()
)
```

## Arguments

- rnames:

  (`character`)  
  vector of row names.

- labs:

  (`character`)  
  vector of row labels. Defaults to `rnames`.

- rnums:

  (`integer`)  
  vector of row numbers. Defaults to `seq_along(rnames)`.

- extents:

  (`integer`)  
  number of lines each row requires to print. Defaults to 1 for all
  rows.

- rclass:

  (`character`)  
  class(es) for the rows. Defaults to `"DataRow"`.

- parent_path:

  (`string`)  
  parent path that all rows should be "children of". Defaults to `NULL`,
  as usually this is not needed. It may be necessary to use `"root"`,
  for some specific scenarios.

- paths:

  (`list`)  
  list of paths to the rows. Defaults to
  `lapply(rnames, function(x) c(parent_path, x))`.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

## Value

A `data.frame` suitable for use in both the `MatrixPrintForm`
constructor and the pagination machinery.

## Examples

``` r
basic_pagdf(c("hi", "there"))
#>       label  name abs_rownumber  path pos_in_siblings n_siblings self_extent
#> hi       hi    hi             1    hi               1          2           1
#> there there there             2 there               2          2           1
#>       par_extent reprint_inds node_class indent nrowrefs ncellrefs nreflines
#> hi             0                 DataRow      0        0         0         0
#> there          0                 DataRow      0        0         0         0
#>       force_page page_title trailing_sep
#> hi         FALSE       <NA>         <NA>
#> there      FALSE       <NA>         <NA>
```
