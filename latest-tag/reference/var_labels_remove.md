# Remove variable labels of a `data.frame`

Remove `label` attribute from all variables in a data frame.

## Usage

``` r
var_labels_remove(x)
```

## Arguments

- x:

  (`data.frame`)  
  a `data.frame` object.

## Value

`x` with its variable labels stripped.

## Examples

``` r
x <- var_labels_remove(iris)
```
