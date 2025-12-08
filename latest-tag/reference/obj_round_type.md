# Rounding Type

When called on a table-like object using the formatters framework, this
method returns the rounding type of the object.

## Usage

``` r
obj_round_type(obj)

# S4 method for class 'MatrixPrintForm'
obj_round_type(obj)

# S4 method for class 'list'
obj_round_type(obj)

obj_round_type(obj) <- value

# S4 method for class 'list'
obj_round_type(obj) <- value

# S4 method for class 'MatrixPrintForm'
obj_round_type(obj) <- value
```

## Arguments

- obj:

  (`ANY`)  
  a table-like object.

- value:

  The new rounding type of the object (see
  [`round_fmt()`](https://insightsengineering.github.io/formatters/reference/round_fmt.md)
  for details)

## Value

The rounding type of the object (see
[`round_fmt()`](https://insightsengineering.github.io/formatters/reference/round_fmt.md)
for details).

## Note

The setter method should only be created/used for pre-MatrixPrintForm
objects, as resetting the rounding type after rounding occurs (which is
during MPF creation) will not effect output when printing/exporting.

round_type cannot not be updated on a `MatrixPrintForm` object as
rounding occurs during creation of MatrixPrintForm object
