# Create a row of a pagination data frame

Create a row of a pagination data frame

## Usage

``` r
pagdfrow(
  row,
  nm = obj_name(row),
  lab = obj_label(row),
  rnum,
  pth,
  sibpos = NA_integer_,
  nsibs = NA_integer_,
  extent = nlines(row, colwidths, fontspec = fontspec),
  colwidths = NULL,
  repext = 0L,
  repind = integer(),
  indent = 0L,
  rclass = class(row),
  nrowrefs = 0L,
  ncellrefs = 0L,
  nreflines = 0L,
  force_page = FALSE,
  page_title = NA_character_,
  trailing_sep = NA_character_,
  fontspec
)
```

## Arguments

- row:

  (`ANY`)  
  object representing the row, which is used for default values of `nm`,
  `lab`, `extent`, and `rclass` if provided. Must have methods for
  `obj_name`, `obj_label`, and `nlines`, to retrieve default values of
  `nm`, `lab`, and `extent`, respectively.

- nm:

  (`string`)  
  name.

- lab:

  (`string`)  
  label.

- rnum:

  (`numeric(1)`)  
  absolute row number.

- pth:

  (`character` or `NULL`)  
  path within larger table.

- sibpos:

  (`integer(1)`)  
  position among sibling rows.

- nsibs:

  (`integer(1)`)  
  number of siblings (including self).

- extent:

  (`numeric(1)`)  
  number of lines required to print the row.

- colwidths:

  (`numeric`)  
  column widths.

- repext:

  (`integer(1)`)  
  number of lines required to reprint all context for this row if it
  appears directly after pagination.

- repind:

  (`integer`)  
  vector of row numbers to be reprinted if this row appears directly
  after pagination.

- indent:

  (`integer`)  
  indent.

- rclass:

  (`string`)  
  class of row object.

- nrowrefs:

  (`integer(1)`)  
  number of row referential footnotes for this row.

- ncellrefs:

  (`integer(1)`)  
  number of cell referential footnotes for the cells in this row.

- nreflines:

  (`integer(1)`)  
  total number of lines required by all referential footnotes.

- force_page:

  (`flag`)  
  currently ignored.

- page_title:

  (`flag`)  
  currently ignored.

- trailing_sep:

  (`string`)  
  the string to use as a separator below this row during printing. If
  `NA_character_`, no separator is used.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

## Value

A single row `data.frame` with the appropriate columns for a pagination
info data frame.
