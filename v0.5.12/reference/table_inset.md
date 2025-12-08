# Access or (recursively) set table inset

Table inset is the amount of characters that the body of a table,
referential footnotes, and main footer material are inset from the
left-alignment of the titles and provenance footer materials.

## Usage

``` r
table_inset(obj)

# S4 method for class 'MatrixPrintForm'
table_inset(obj)

table_inset(obj) <- value

# S4 method for class 'MatrixPrintForm'
table_inset(obj) <- value
```

## Arguments

- obj:

  (`ANY`)  
  object to get or (recursively if necessary) set table inset for.

- value:

  (`string`)  
  string to use as new header/body separator.

## Value

- `table_inset` returns the integer value that the table body (including
  column heading information and section dividers), referential
  footnotes, and main footer should be inset from the left alignment of
  the titles and provenance footers during rendering.

- `table_inset<-` returns `obj` with the new table_inset value applied
  recursively to it and all its subtables.
