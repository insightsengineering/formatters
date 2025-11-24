# Set label attributes of all variables in a `data.frame`

Variable labels can be stored as the `label` attribute for each
variable. This functions sets all non-missing (non-`NA`) variable labels
in a `data.frame`.

## Usage

``` r
var_labels(x) <- value
```

## Arguments

- x:

  (`data.frame`)  
  a data frame object.

- value:

  (`character`)  
  a vector of new variable labels. If any values are `NA`, the label for
  that variable is removed.

## Value

`x` with modified variable labels.

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

if (interactive()) {
  View(x) # in RStudio data viewer labels are displayed
}
```
