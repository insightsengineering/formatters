## formatters 0.3.3
* `vert_pag_indices` now accepts `colwidths` argument

## formatters 0.3.2.5
 * `label_vars<-` now correctly adheres to names of value, even in different order than `names(x)`. Fixes rlistings#8

## formatters 0.3.2.4
 * `vert_pag_indices` now accepts `colwidths` argument

## formatters 0.3.2.3
 * fix regression caused by `0.3.2.2` in `vert_pag_indices` when `obj` is not a `MatrixPrintForm` object

## formatters 0.3.2.2
 * Added support for `trailing_sep` column to row_info data.frames (e.g, `pagdfrow` constructor)
 * `trailing_sep` info now used in both printing and pagination
 * `col_gap` is now an element of `MatrixPrintForm` objects
 * `vert_pag_indices` fixed to correctly take gap between printed columns into account

## formatters 0.3.2.1
 * calling `var_labels` on a data.frame with no columns is no longer an error (roche/rtables#224)

## formatters 0.3.2
 * Change warning when non-UTF line sep default is used to message, and display it only during interactive sessions (once per sessoin).

## formatters 0.3.1
 * Released on CRAN

## formatters 0.3.0
 * add exported `default_hsep` function for use her and in reverse-dependencies (incl `rtables`)
 * format_value now respects `na_str` even when format is `xx` (previously it returned `"NA"` always)
 * rename `linesep` argument to `hsep` in `toString` generic signature 
 * add `indent_size` argument to `matrix_form` generic signature
 * add a number of `"__ (__)"` (no pct) formats in response to #23
 * Switch to `testthat` as testing framework

## formatters 0.2.0
 * Updated Documentation
 * Released on CRAN

## formatters 0.1.0.0003
 * Fix regression in support for new lines in columns in rtables

## formatters 0.1.0.0002
 * Add `formats` argument to matrix_form informal class/constructor for use in rtables::table_shell
