# Number of lines required to print a value

Number of lines required to print a value

## Usage

``` r
nlines(x, colwidths = NULL, max_width = NULL, fontspec, col_gap = NULL)

# S4 method for class 'list'
nlines(x, colwidths = NULL, max_width = NULL, fontspec, col_gap = NULL)

# S4 method for class 'NULL'
nlines(x, colwidths = NULL, max_width = NULL, fontspec, col_gap = NULL)

# S4 method for class 'character'
nlines(x, colwidths = NULL, max_width = NULL, fontspec, col_gap = NULL)
```

## Arguments

- x:

  (`ANY`)  
  the object to be printed.

- colwidths:

  (`numeric`)  
  column widths (if necessary). Principally used in `rtables`' method.

- max_width:

  (`numeric(1)`)  
  width that strings should be wrapped to when determining how many
  lines they require.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

- col_gap:

  (`numeric(1)`)  
  width of gap between columns in number of spaces. Only used by methods
  which must calculate span widths after wrapping.

## Value

The number of lines needed to render the object `x`.
