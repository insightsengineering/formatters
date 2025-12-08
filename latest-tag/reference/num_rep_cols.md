# Number of repeated columns

When called on a table-like object using the formatters framework, this
method returns the number of columns which are mandatorily repeated
after each horizontal pagination.

## Usage

``` r
num_rep_cols(obj)

# S4 method for class 'ANY'
num_rep_cols(obj)

# S4 method for class 'MatrixPrintForm'
num_rep_cols(obj)

num_rep_cols(obj) <- value

# S4 method for class 'ANY'
num_rep_cols(obj) <- value

# S4 method for class 'MatrixPrintForm'
num_rep_cols(obj) <- value
```

## Arguments

- obj:

  (`ANY`)  
  a table-like object.

- value:

  (`numeric(1)`)  
  the new number of columns to repeat.

## Value

An integer.

## Details

Absent a class-specific method, this function returns 0, indicating no
always-repeated columns.

## Note

This number *does not* include row labels, the repetition of which is
handled separately.

## Examples

``` r
mpf <- basic_matrix_form(mtcars)
num_rep_cols(mpf)
#> [1] 0
lmpf <- basic_listing_mf(mtcars)
num_rep_cols(lmpf)
#> [1] 1
```
