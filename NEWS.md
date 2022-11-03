## formatters 0.3.4.3
 * placeholder

## formatters 0.3.4
 * `nlines` now accepts both `colwidths` and `max_width`, defaulting to `NULL` for both
 * `pag_indices_inner`, `find_pag` and `valid_pag` now accept `have_col_fnotes` and `div_height` arguments
 * fix bug which lead to Roche/rtables#414 (`nlines("", <non-null>)` threw an error
 * paginate related generics now accept max_width for use in ensuring pagination now takes wordwrapping into account for titles and footers.
 * nlines character method has changed, not takes the sum of the nlines of its elements instead of the max in the case of a vector of length > 1
 * new `wrap_title_footer` function for use on everything which implements the generic interface we design.
 * export new `page_dim` function which returns values of page dims for a named type.
 * `tf_wrap` functionality now breaks up words that are too large with respect to surrounding wordwrapping.
 * `tf_wrap` behavior no longer emits a warning when breaking up 'words' larger than `max_width`
 * `tf_wrap` `max_width = "auto"` now sets the `max_width` to `sum(widths) + table_inset(x)` instead of `sum(widths)`
 * Experimental `tf_wrap` functionality added to `toString` `MatrixPrintForm` method
 * `MatrixPrintForm` class now includes a `table_inset` element
 * Add `table_inset` and `table_inset<-` generics now exported
 * `toString` now respects `table_inset` values set on a `MatrixPrintForm` object
 * Use `exact=TRUE` for all calls to `attr` in accessor functions.
 * length-zero values of `na_str` are now silently interpreted as the default value (`"NA"`),
   as are any missing values in `na_str` of length > 0.
 * adding wrapping of titles, subtitles, and footers.
 * `page_lcpp` function now provided to map page size and font combinations to lines
   and characters per page.
 * getters and setters for the components of `MatrixPrintForm` objects are now exported 
   (i.e., `mf_spans` and friends).
 * `na_str` can now be length >1, and is matched positionally to NAs in that case.
 * `format_value` now returns solely the `na_str` value when all-NA values are formatted with certain formats 
    (currently`"(N=xx)"`, `">999.9"`, `">999.99"`, `"x.xxxx | (<0.0001)"`)

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
