# Create a row for a referential footnote information data frame

Create a row for a referential footnote information data frame

## Usage

``` r
ref_df_row(
  row_path = NA_character_,
  col_path = NA_character_,
  row = NA_integer_,
  col = NA_integer_,
  symbol = NA_character_,
  ref_index = NA_integer_,
  msg = NA_character_,
  max_width = NULL
)
```

## Arguments

- row_path:

  (`character`)  
  row path (or `NA_character_` for none).

- col_path:

  (`character`)  
  column path (or `NA_character_` for none).

- row:

  (`integer(1)`)  
  integer position of the row.

- col:

  (`integer(1)`)  
  integer position of the column.

- symbol:

  (`string`)  
  symbol for the reference. `NA_character_` to use the `ref_index`
  automatically.

- ref_index:

  (`integer(1)`)  
  index of the footnote, used for ordering even when symbol is not `NA`.

- msg:

  (`string`)  
  the string message, not including the symbol portion (`{symbol} - `)

- max_width:

  (`numeric(1)`)  
  width that strings should be wrapped to when determining how many
  lines they require.

## Value

A single row data frame with the appropriate columns.
