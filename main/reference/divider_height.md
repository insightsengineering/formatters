# Divider height

Divider height

## Usage

``` r
divider_height(obj)

# S4 method for class 'ANY'
divider_height(obj)
```

## Arguments

- obj:

  (`ANY`)  
  object.

## Value

The height, in lines of text, of the divider between header and body.
Currently returns `1L` for the default method.

## Examples

``` r
divider_height(mtcars)
#> [1] 1
```
