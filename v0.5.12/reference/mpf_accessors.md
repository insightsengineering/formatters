# Getters and setters for aspects of `MatrixPrintForm` objects

Most of these functions, particularly the setters, are intended almost
exclusively for internal use in, e.g.,
[`matrix_form`](https://insightsengineering.github.io/formatters/reference/matrix_form.md)
methods, and should generally not be called by end users.

## Usage

``` r
mf_strings(mf)

mf_spans(mf)

mf_aligns(mf)

mf_display(mf)

mf_formats(mf)

mf_rinfo(mf)

mf_cinfo(mf)

mf_has_topleft(mf)

mf_lgrouping(mf)

mf_rfnotes(mf)

mf_nlheader(mf)

mf_nrheader(mf)

mf_colgap(mf)

mf_fontspec(mf)

mf_fontspec(mf) <- value

mf_strings(mf) <- value

mf_spans(mf) <- value

mf_aligns(mf) <- value

mf_display(mf) <- value

mf_formats(mf) <- value

mf_rinfo(mf) <- value

mf_cinfo(mf) <- value

mf_lgrouping(mf) <- value

mf_rfnotes(mf) <- value

mf_nrheader(mf) <- value

mf_colgap(mf) <- value

mf_ncol(mf)

mf_nrow(mf)

mf_ncol(mf) <- value

# S4 method for class 'MatrixPrintForm'
ncol(x)

mpf_has_rlabels(mf)

mf_has_rlabels(mf)
```

## Arguments

- mf:

  (`MatrixPrintForm`)  
  a `MatrixPrintForm` object.

- value:

  (`ANY`)  
  the new value for the component in question.

- x:

  `MatrixPrintForm`. The object.

## Value

- Getters return the associated element of `mf`.

- Setters return the modified `mf` object.
