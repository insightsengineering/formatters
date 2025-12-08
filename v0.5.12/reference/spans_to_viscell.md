# Transform a vector of spans (with duplication) into a visibility vector

Transform a vector of spans (with duplication) into a visibility vector

## Usage

``` r
spans_to_viscell(spans)
```

## Arguments

- spans:

  (`numeric`)  
  a vector of spans, with each span value repeated for the cells it
  covers.

## Value

A logical vector the same length as `spans` indicating whether the
contents of a string vector with those spans is valid.

## Details

The values of `spans` are assumed to be repeated such that each
individual position covered by the span has the repeated value.

This means that each block of values in `spans` must be of a length at
least equal to its value (i.e. two 2s, three 3s, etc).

This function correctly handles cases where two spans of the same size
are next to each other; i.e., a block of four 2s represents two large
cells each of which spans two individual cells.

## Note

Currently no checking or enforcement is done to verify that the vector
of spans is valid according to the specifications described in the
Details section above.

## Examples

``` r
spans_to_viscell(c(2, 2, 2, 2, 1, 3, 3, 3))
#> [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE
```
