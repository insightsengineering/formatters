# Get label attributes of variables in a `data.frame`

Variable labels can be stored as a `label` attribute for each variable.
This functions returns a named character vector with the variable labels
(or empty strings if not specified).

## Usage

``` r
var_labels(x, fill = FALSE)
```

## Arguments

- x:

  (`data.frame`)  
  a data frame object.

- fill:

  (`flag`)  
  whether variable names should be returned for variables for which the
  `label` attribute does not exist. If `FALSE`, these variables are
  filled with `NA`s instead.

## Value

a named character vector of variable labels from `x`, with names
corresponding to variable names.

## Examples

``` r
x <- iris
var_labels(x)
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
#>           NA           NA           NA           NA           NA 
var_labels(x) <- paste("label for", names(iris))
var_labels(x)
#>             Sepal.Length              Sepal.Width             Petal.Length 
#> "label for Sepal.Length"  "label for Sepal.Width" "label for Petal.Length" 
#>              Petal.Width                  Species 
#>  "label for Petal.Width"      "label for Species" 
```
