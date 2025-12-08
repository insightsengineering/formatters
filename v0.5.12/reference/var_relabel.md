# Copy and change variable labels of a `data.frame`

Relabel a subset of the variables.

## Usage

``` r
var_relabel(x, ...)
```

## Arguments

- x:

  (`data.frame`)  
  a data frame object.

- ...:

  name-value pairs, where each name corresponds to a variable name in
  `x` and the value to the new variable label.

## Value

A copy of `x` with labels modified according to `...`

## Examples

``` r
x <- var_relabel(iris, Sepal.Length = "Sepal Length of iris flower")
var_labels(x)
#>                  Sepal.Length                   Sepal.Width 
#> "Sepal Length of iris flower"                            NA 
#>                  Petal.Length                   Petal.Width 
#>                            NA                            NA 
#>                       Species 
#>                            NA 
```
