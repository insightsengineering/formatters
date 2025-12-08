# Generic for performing "forced" pagination

Forced pagination is pagination which happens regardless of position on
page. The object is expected to have all information necessary to locate
such page breaks, and the `do_forced_pag` method is expected to fully
perform those paginations.

## Usage

``` r
do_forced_paginate(obj)

# S4 method for class 'ANY'
do_forced_paginate(obj)
```

## Arguments

- obj:

  (`ANY`)  
  object to be paginated. The `ANY` method simply returns a list of
  length one, containing `obj`.

## Value

A list of sub-objects, which will be further paginated by the standard
pagination algorithm.
