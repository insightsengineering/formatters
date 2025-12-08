# Return an object with a label attribute

Return an object with a label attribute

## Usage

``` r
with_label(x, label)
```

## Arguments

- x:

  (`ANY`)  
  an object.

- label:

  (`string`)  
  label attribute to attach to `x`.

## Value

`x` labeled by `label`. Note that the exact mechanism of labeling should
be considered an internal implementation detail, but the label can
always be retrieved via `obj_label`.

## Examples

``` r
x <- with_label(c(1, 2, 3), label = "Test")
obj_label(x)
#> [1] "Test"
```
