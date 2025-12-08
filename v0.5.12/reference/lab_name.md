# Label, name, and format accessor generics

Getters and setters for basic, relatively universal attributes of
"table-like" objects.

## Usage

``` r
obj_name(obj)

obj_name(obj) <- value

obj_label(obj)

obj_label(obj) <- value

# S4 method for class 'ANY'
obj_label(obj)

# S4 method for class 'ANY'
obj_label(obj) <- value

obj_format(obj)

# S4 method for class 'ANY'
obj_format(obj)

# S4 method for class 'fmt_config'
obj_format(obj)

obj_format(obj) <- value

# S4 method for class 'ANY'
obj_format(obj) <- value

# S4 method for class 'fmt_config'
obj_format(obj) <- value

obj_na_str(obj)

# S4 method for class 'ANY'
obj_na_str(obj)

# S4 method for class 'fmt_config'
obj_na_str(obj)

obj_na_str(obj) <- value

# S4 method for class 'ANY'
obj_na_str(obj) <- value

# S4 method for class 'fmt_config'
obj_na_str(obj) <- value

obj_align(obj)

# S4 method for class 'ANY'
obj_align(obj)

# S4 method for class 'fmt_config'
obj_align(obj)

obj_align(obj) <- value

# S4 method for class 'ANY'
obj_align(obj) <- value

# S4 method for class 'fmt_config'
obj_align(obj) <- value
```

## Arguments

- obj:

  (`ANY`)  
  the object.

- value:

  character(1). The new label

## Value

The name, format, or label of `obj` for getters, or `obj` after
modification for setters.

## See also

with_label
