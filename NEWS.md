## formatters 0.5.6.9006

* Deprecate `width`, `height` and `fontsize` arguments of `export_as_pdf()` using `lifecycle` package.
* Add `lifecycle` badge files for documentation.
* Fixed bug causing recursive appending of page number text during pagination.

## formatters 0.5.6
 * Added "N=xx" format and unit test for it.
 * Added error catch for `\r` recursive special character.
 * Fixed pagination unexpected counts for `rlistings`' pagination by removing the manual subsetting workaround and fixing [`insightsengineering/rlistings#155`](https://github.com/insightsengineering/rlistings/issues/155).
 * Fixed mismatch between pagination and exports regarding the value assigned to parameter `max_width`. Introduced general handler `.handle_max_width` for pagination, exports, and `toString`.
 * Fixed bug in `format_value` causing a warning for vectors containing both NA and non-NA values.
 * Fixed issue with `var_label` assignment that needed to be of non-named strings.
 * Included indentation and split rows with their `LabelRow` assignment in `basic_matrix_form`.
 * Allowed tables with content rows in the end be exported.
 * Moved new line expansion for decorations from `rtables`' `matrix_form` to `formatters`' constructor `MatrixPrintForm` so to cover also `rlistings`.
 * Improved relevant information feedback during pagination.
 * Updated `export_as_txt` to allow lists of tables/listings as input. This enables listing pagination with pages by parameter.
 * Removed the possibility of setting `min_siblings > 0` when dealing with listings. This allows smooth pagination when there are only 2 lines.
 * Removed redundant references to `matrix_print_form` constructor (now only `MatrixPrintForm`).

## formatters 0.5.5
 * Applied `styler` and resolved package lint. Changed default indentation from 4 spaces to 2.
 * Added the possibility of setting a general default using `set_default_hsep()` that sets up the option `getOption("formatters_default_hsep")`.
 * Allowed section divider between header and table body.
 * Added support for combining duplicate referential footnotes.
 * Migrated `export_as_pdf` from `rtables`. Now using `paginate_to_mpfs` function. Made `font_lcpi` function internal.
 * Fixed wrapping and section dividers error.
 * Fixed infinite loop in `wrap_string` that was caused by a bug in `stringi::stri_wrap` not wrapping small
   strings with dots and spaces correctly.

## formatters 0.5.4
 * Fixed a bug in `paginate_to_mpfs()` so that formatting in listings key columns is retained with pagination [`insightsengineering/rlistings#155`](https://github.com/insightsengineering/rlistings/issues/155).
 * Improved error message for pagination when `cpp` or `lpp` is too small in comparison to the column or row widths.
 * Added full support of newline characters in any part of `rtables` objects.
 * Modified default vertical alignment for top left information to bottom.
 * Rework of `wrap_string` so to allow space characters to be used and wrapped.

## formatters 0.5.3
 * Decimal alignment now throws informative error if scientific notation is used.
 * Specified minimal version of package dependencies.
 * Updated hex sticker logo.

## formatters 0.5.2
 * `paginate_to_mpfs` can handle single column tables and listings.
 * Added decimal alignment support with `decimal`, `dec_right`, and `dec_left`.
 * Updated `export_as_txt` to pass `rep_cols` argument to pagination function.
 * Added `list_valid_aligns()` that lists the available alignments.
 * Set default values for `na_str` and `align` to `NULL` in the format configuration function `fmt_config`.
 * Fixed a bug that threw warning when `prov_footer` length is greater than `1` [`insightsengineering/rtables#705`](https://github.com/insightsengineering/rtables/issues/705).

## formatters 0.5.1
 * Updated `export_as_txt` to print split label when using `page_by` with only one level.
 * Reduced the amount of spelling issues.
 * New generic `getter` and `setter` for `align` (`obj_align` and `obj_align<-`).
 * New `fmt_config` class to bundle together `format`, `na_str`, and `align` instructions.
 * Reverting default for alignment (from `NULL` to `center`) and `na_str`
   from `NULL` to `"NA"`. This affects only `rlistings`, where the new default takes effect.

## formatters 0.5.0
 * Fix bug in `MPF` pagination (and thus export_as_txt) when column labels had newlines ([#150](https://github.com/insightsengineering/formatters/issues/150), [`insightsengineering/rtables#634`](https://github.com/insightsengineering/rtables/issues/634))
 * `font_size` is now 8 consistently across the pagination machinery.
 * When specified, margins are in inches (including the default).
 * Fix off-by-one error in pagination machinery for certain cases.
 * Refined informative messages when pagination is verbose (`verbose = TRUE`).
 * Referential footnotes with specified (non-number) symbols are now printed once per page regardless of the number of elements on the page they refer to.
 * New `export_as_txt` function operates now on any object with an applicable `matrix_form` method (including pagination).
 * New `paginate_indices` and `paginate_to_mpfs` functions.
 * New `diagnose_pagination` function accepts the same arguments as `paginate_to_mpfs` and returns information about the pagination algorithm outcome at both row and column positions.
 * `MatrixPrintForm` objects now pay closer attention to referential footnote information than they did in the past.
 * `MatrixPrintForm` objects infer detailed referential footnote information from their `strings` element for backward compatibility.
 * Fix to test that failed on old Windows CRAN machine due to imperfect UTF support there.
 * If `lpp` and `cpp` in pagination or exporter functions are assigned to `NULL`, no pagination in the vertical or horizontal direction happens, respectively.
 * The new default of `NA_integer_` for `lpp` and `cpp` now means those values should be inferred from page size.
 * Added `hexSticker` logo.

## formatters 0.4.0
 * Cell values and row labels are now word-wrapped based on column widths (`widths` in `toString` and `colwidths` in pagination and exporters.
 * New "N=xx (xx%)" format support (https://github.com/insightsengineering/rtables/issues/516).
 * New generic `getter` and `setter` for `na`-string (`obj_na_str` and `obj_na_str<-`, migrated from `rtables`).
 * `MatrixPrintForm` class now carries around `has_topleft` information explicitly, `getter` `mf_has_topleft`.
 * Number of header lines in a `MatrixPrintForm` object (`mf_nlheader`) is now a computed attribute rather than a stored one, `mf_nlheader<-` has been removed.
 * New `MatrixPrintForm` accessor: `mpf_has_rlabels` determines whether the `MPF$strings` has a column which are label rows (i.e. no column index in originating object).
 * `wrap_text` and `wrap_string` now accept the `hard` argument (default `FALSE`) indicating whether 'hard'
   word-wrapping (i.e. explicit embedding of newline characters) should be performed.
 * `vert_pag_indices` now accepts `rep_cols` to control how many columns act as 'row-label-like', as
   required by `rlistings`.
 * Documentation of pagination algorithm greatly expanded.
 * Pagination with `verbose = TRUE` is now significantly more informative.
 * fix bug where indenting was not correct when row-labels were word wrapped (#84, #85).
 * fix bug where portions of table after last section div (e.g., non-nested further analyses) were omitted (#77).
 * fix bug in `mf_rinfo<-` checked the new value against the wrong dimensions.
 * Fixed off-by-one error in basic pagination machinery.
 * Fix bug in position of header separator when columns have word wrapping (#68).
 * Bug rendering `mf_lgrouping` non-functional has been fixed.

## formatters 0.3.4
 * `nlines` now accepts both `colwidths` and `max_width`, defaulting to `NULL` for both
 * `pag_indices_inner`, `find_pag` and `valid_pag` now accept `have_col_fnotes` and `div_height` arguments
 * fix bug which lead to [`insightsengineering/rtables#414`](https://github.com/insightsengineering/rtables/issues/414) (`nlines("", <non-null>)` threw an error
 * paginate related generics now accept max_width for use in ensuring pagination now takes word wrapping into account for titles and footers.
 * `nlines` character method has changed, not takes the sum of the `nlines` of its elements instead of the max in the case of a vector of length > 1
 * new `wrap_title_footer` function for use on everything which implements the generic interface we design.
 * export new `page_dim` function which returns values of page dims for a named type.
 * `tf_wrap` functionality now breaks up words that are too large with respect to surrounding word wrapping.
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
 * `getters` and `setters` for the components of `MatrixPrintForm` objects are now exported
   (i.e., `mf_spans` and friends).
 * `na_str` can now be length >1, and is matched in position to NAs in that case.
 * `format_value` now returns solely the `na_str` value when all-NA values are formatted with certain formats
    (currently`"(N=xx)"`, `">999.9"`, `">999.99"`, `"x.xxxx | (<0.0001)"`)

## formatters 0.3.3
 * `vert_pag_indices` now accepts `colwidths` argument

## formatters 0.3.2.5
 * `label_vars<-` now correctly adheres to names of value, even in different order than `names(x)`. Fixes [`insightsengineering/rlistings#8`](https://github.com/insightsengineering/rlistings/issues/8)

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
 * calling `var_labels` on a data.frame with no columns is no longer an error ([`insightsengineering/rtables#224`](https://github.com/insightsengineering/rtables/issues/224))

## formatters 0.3.2
 * Change warning when non-UTF line separator default is used to message, and displays it only during interactive sessions (once per session).

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
 * Fix regression in support for new lines in columns in `rtables`

## formatters 0.1.0.0002
 * Add `formats` argument to matrix_form informal class/constructor for use in `rtables::table_shell`
