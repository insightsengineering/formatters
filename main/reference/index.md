# Package index

## Formatters Overview

- [`formatters`](https://insightsengineering.github.io/formatters/reference/formatters-package.md)
  [`formatters-package`](https://insightsengineering.github.io/formatters/reference/formatters-package.md)
  : formatters Package

## Formatting Values

Functions related to applying formats to values

- [`format_value()`](https://insightsengineering.github.io/formatters/reference/format_value.md)
  :

  Converts a (possibly compound) value into a string using the `format`
  information

- [`round_fmt()`](https://insightsengineering.github.io/formatters/reference/round_fmt.md)
  : Round and prepare a value for display

- [`is_valid_format()`](https://insightsengineering.github.io/formatters/reference/check_formats.md)
  [`check_aligns()`](https://insightsengineering.github.io/formatters/reference/check_formats.md)
  : Check if a format or alignment is supported

- [`is.wholenumber()`](https://insightsengineering.github.io/formatters/reference/is.wholenumber.md)
  : Check if a value is a whole number

- [`list_valid_format_labels()`](https://insightsengineering.github.io/formatters/reference/list_formats.md)
  [`list_valid_aligns()`](https://insightsengineering.github.io/formatters/reference/list_formats.md)
  : List of currently supported formats and vertical alignments

- [`sprintf_format()`](https://insightsengineering.github.io/formatters/reference/sprintf_format.md)
  :

  Specify text format via a `sprintf` format string

- [`fmt_config()`](https://insightsengineering.github.io/formatters/reference/fmt_config.md)
  : Format configuration

## Generics Interface For Pagination

Generics required for pagination machinery

- [`make_row_df()`](https://insightsengineering.github.io/formatters/reference/make_row_df.md)
  : Make row layout summary data frames for use during pagination
- [`pagdfrow()`](https://insightsengineering.github.io/formatters/reference/pagdfrow.md)
  : Create a row of a pagination data frame
- [`nlines()`](https://insightsengineering.github.io/formatters/reference/nlines.md)
  : Number of lines required to print a value
- [`divider_height()`](https://insightsengineering.github.io/formatters/reference/divider_height.md)
  : Divider height
- [`main_title()`](https://insightsengineering.github.io/formatters/reference/title_footer.md)
  [`` `main_title<-`() ``](https://insightsengineering.github.io/formatters/reference/title_footer.md)
  [`subtitles()`](https://insightsengineering.github.io/formatters/reference/title_footer.md)
  [`` `subtitles<-`() ``](https://insightsengineering.github.io/formatters/reference/title_footer.md)
  [`page_titles()`](https://insightsengineering.github.io/formatters/reference/title_footer.md)
  [`` `page_titles<-`() ``](https://insightsengineering.github.io/formatters/reference/title_footer.md)
  [`main_footer()`](https://insightsengineering.github.io/formatters/reference/title_footer.md)
  [`` `main_footer<-`() ``](https://insightsengineering.github.io/formatters/reference/title_footer.md)
  [`prov_footer()`](https://insightsengineering.github.io/formatters/reference/title_footer.md)
  [`` `prov_footer<-`() ``](https://insightsengineering.github.io/formatters/reference/title_footer.md)
  [`all_footers()`](https://insightsengineering.github.io/formatters/reference/title_footer.md)
  [`all_titles()`](https://insightsengineering.github.io/formatters/reference/title_footer.md)
  : General title and footer accessors
- [`num_rep_cols()`](https://insightsengineering.github.io/formatters/reference/num_rep_cols.md)
  [`` `num_rep_cols<-`() ``](https://insightsengineering.github.io/formatters/reference/num_rep_cols.md)
  : Number of repeated columns
- [`do_forced_paginate()`](https://insightsengineering.github.io/formatters/reference/do_forced_paginate.md)
  : Generic for performing "forced" pagination

## Generics Interface for ASCII Rendering

Generics required for rendering table-like object to ASCII

- [`matrix_form()`](https://insightsengineering.github.io/formatters/reference/matrix_form.md)
  :

  Transform `rtable` to a list of matrices which can be used for
  outputting

- [`basic_matrix_form()`](https://insightsengineering.github.io/formatters/reference/test_matrix_form.md)
  [`basic_listing_mf()`](https://insightsengineering.github.io/formatters/reference/test_matrix_form.md)
  : Create spoof matrix form from a data frame

## MatrixPrintForm

Intermediate representation for ASCII table printing

- [`MatrixPrintForm()`](https://insightsengineering.github.io/formatters/reference/MatrixPrintForm.md)
  : Constructor for Matrix Print Form
- [`MatrixPrintForm-class`](https://insightsengineering.github.io/formatters/reference/MatrixPrintForm_class.md)
  : Class for Matrix Print Form

## ASCII Rendering of MatrixPrintForm objects

ASCII rendering-related functions which operate on MatrixPrintForm
objects

- [`toString()`](https://insightsengineering.github.io/formatters/reference/tostring.md)
  : Transform objects into string representations

- [`spans_to_viscell()`](https://insightsengineering.github.io/formatters/reference/spans_to_viscell.md)
  : Transform a vector of spans (with duplication) into a visibility
  vector

- [`propose_column_widths()`](https://insightsengineering.github.io/formatters/reference/propose_column_widths.md)
  :

  Propose column widths based on the `MatrixPrintForm` of an object

- [`decimal_align()`](https://insightsengineering.github.io/formatters/reference/decimal_align.md)
  : Decimal alignment

- [`list_valid_format_labels()`](https://insightsengineering.github.io/formatters/reference/list_formats.md)
  [`list_valid_aligns()`](https://insightsengineering.github.io/formatters/reference/list_formats.md)
  : List of currently supported formats and vertical alignments

- [`padstr()`](https://insightsengineering.github.io/formatters/reference/padstr.md)
  : Pad a string and align within string

- [`spread_integer()`](https://insightsengineering.github.io/formatters/reference/spread_integer.md)
  : Spread an integer to a given length

- [`is.wholenumber()`](https://insightsengineering.github.io/formatters/reference/is.wholenumber.md)
  : Check if a value is a whole number

## Pagination

- [`paginate_indices()`](https://insightsengineering.github.io/formatters/reference/paginate_indices.md)
  [`paginate_to_mpfs()`](https://insightsengineering.github.io/formatters/reference/paginate_indices.md)
  [`diagnose_pagination()`](https://insightsengineering.github.io/formatters/reference/paginate_indices.md)
  : Paginate a table-like object for rendering
- [`pagination_algo`](https://insightsengineering.github.io/formatters/reference/pagination_algo.md)
  : Pagination
- [`pag_indices_inner()`](https://insightsengineering.github.io/formatters/reference/pag_indices_inner.md)
  : Find pagination indices from pagination info data frame
- [`vert_pag_indices()`](https://insightsengineering.github.io/formatters/reference/vert_pag_indices.md)
  : Find column indices for vertical pagination
- [`pagdfrow()`](https://insightsengineering.github.io/formatters/reference/pagdfrow.md)
  : Create a row of a pagination data frame
- [`basic_pagdf()`](https://insightsengineering.github.io/formatters/reference/basic_pagdf.md)
  : Basic/spoof pagination info data frame

## Variable Labels

Functions for assigning labels to variables in data

- [`` `var_labels<-`() ``](https://insightsengineering.github.io/formatters/reference/var_labels-set.md)
  :

  Set label attributes of all variables in a `data.frame`

- [`var_labels()`](https://insightsengineering.github.io/formatters/reference/var_labels.md)
  :

  Get label attributes of variables in a `data.frame`

- [`var_labels_remove()`](https://insightsengineering.github.io/formatters/reference/var_labels_remove.md)
  :

  Remove variable labels of a `data.frame`

- [`var_relabel()`](https://insightsengineering.github.io/formatters/reference/var_relabel.md)
  :

  Copy and change variable labels of a `data.frame`

- [`with_label()`](https://insightsengineering.github.io/formatters/reference/with_label.md)
  : Return an object with a label attribute

## General Accessors

- [`obj_name()`](https://insightsengineering.github.io/formatters/reference/lab_name.md)
  [`` `obj_name<-`() ``](https://insightsengineering.github.io/formatters/reference/lab_name.md)
  [`obj_label()`](https://insightsengineering.github.io/formatters/reference/lab_name.md)
  [`` `obj_label<-`() ``](https://insightsengineering.github.io/formatters/reference/lab_name.md)
  [`obj_format()`](https://insightsengineering.github.io/formatters/reference/lab_name.md)
  [`` `obj_format<-`() ``](https://insightsengineering.github.io/formatters/reference/lab_name.md)
  [`obj_na_str()`](https://insightsengineering.github.io/formatters/reference/lab_name.md)
  [`` `obj_na_str<-`() ``](https://insightsengineering.github.io/formatters/reference/lab_name.md)
  [`obj_align()`](https://insightsengineering.github.io/formatters/reference/lab_name.md)
  [`` `obj_align<-`() ``](https://insightsengineering.github.io/formatters/reference/lab_name.md)
  : Label, name, and format accessor generics

## Data

Data included with the package

- [`ex_adsl`](https://insightsengineering.github.io/formatters/reference/cdisc_data.md)
  [`ex_adae`](https://insightsengineering.github.io/formatters/reference/cdisc_data.md)
  [`ex_adaette`](https://insightsengineering.github.io/formatters/reference/cdisc_data.md)
  [`ex_adtte`](https://insightsengineering.github.io/formatters/reference/cdisc_data.md)
  [`ex_adcm`](https://insightsengineering.github.io/formatters/reference/cdisc_data.md)
  [`ex_adlb`](https://insightsengineering.github.io/formatters/reference/cdisc_data.md)
  [`ex_admh`](https://insightsengineering.github.io/formatters/reference/cdisc_data.md)
  [`ex_adqs`](https://insightsengineering.github.io/formatters/reference/cdisc_data.md)
  [`ex_adrs`](https://insightsengineering.github.io/formatters/reference/cdisc_data.md)
  [`ex_advs`](https://insightsengineering.github.io/formatters/reference/cdisc_data.md)
  : Simulated CDISC-like data for examples
- [`DM`](https://insightsengineering.github.io/formatters/reference/DM.md)
  : DM data

## Export

Export into different object types

- [`export_as_txt()`](https://insightsengineering.github.io/formatters/reference/export_as_txt.md)
  : Export a table-like object to plain (ASCII) text with page breaks

- [`export_as_rtf()`](https://insightsengineering.github.io/formatters/reference/export_as_rtf.md)
  : Export as RTF

- [`export_as_pdf()`](https://insightsengineering.github.io/formatters/reference/export_as_pdf.md)
  : Export as PDF

- [`mpf_to_rtf()`](https://insightsengineering.github.io/formatters/reference/mpf_to_rtf.md)
  :

  Transform `MatrixPrintForm` to RTF

## Miscellany

Other documented functions used by developers of this package

- [`print(`*`<ANY>`*`)`](https://insightsengineering.github.io/formatters/reference/basemethods.md)
  : Print

- [`wrap_string()`](https://insightsengineering.github.io/formatters/reference/wrap_string.md)
  [`wrap_txt()`](https://insightsengineering.github.io/formatters/reference/wrap_string.md)
  : Wrap a string to a precise width

- [`split_word_ttype()`](https://insightsengineering.github.io/formatters/reference/wrap_string_ttype.md)
  [`wrap_string_ttype()`](https://insightsengineering.github.io/formatters/reference/wrap_string_ttype.md)
  : wrap string given a Truetype font

- [`` `%||%` ``](https://insightsengineering.github.io/formatters/reference/ifnotlen0.md)
  :

  `%||%` (if length-0) alternative operator

- [`table_inset()`](https://insightsengineering.github.io/formatters/reference/table_inset.md)
  [`` `table_inset<-`() ``](https://insightsengineering.github.io/formatters/reference/table_inset.md)
  : Access or (recursively) set table inset

- [`default_hsep()`](https://insightsengineering.github.io/formatters/reference/default_horizontal_sep.md)
  [`set_default_hsep()`](https://insightsengineering.github.io/formatters/reference/default_horizontal_sep.md)
  : Default horizontal separator

- [`mf_strings()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_spans()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_aligns()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_display()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_formats()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_rinfo()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_cinfo()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_has_topleft()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_lgrouping()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_rfnotes()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_nlheader()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_nrheader()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_colgap()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_fontspec()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_fontspec<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_strings<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_spans<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_aligns<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_display<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_formats<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_rinfo<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_cinfo<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_lgrouping<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_rfnotes<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_nrheader<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_colgap<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_ncol()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_nrow()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`` `mf_ncol<-`() ``](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`ncol(`*`<MatrixPrintForm>`*`)`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mpf_has_rlabels()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  [`mf_has_rlabels()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  :

  Getters and setters for aspects of `MatrixPrintForm` objects

- [`page_lcpp()`](https://insightsengineering.github.io/formatters/reference/page_lcpp.md)
  : Determine lines per page (LPP) and characters per page (CPP) based
  on font and page type

- [`page_types()`](https://insightsengineering.github.io/formatters/reference/page_types.md)
  [`page_dim()`](https://insightsengineering.github.io/formatters/reference/page_types.md)
  : Supported named page types

- [`ref_df_row()`](https://insightsengineering.github.io/formatters/reference/ref_df_row.md)
  : Create a row for a referential footnote information data frame

- [`default_page_number()`](https://insightsengineering.github.io/formatters/reference/default_page_number.md)
  [`set_default_page_number()`](https://insightsengineering.github.io/formatters/reference/default_page_number.md)
  : Default page number format

## Font support

Functions related to font support for pagination

- [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md)
  : Font size specification
- [`nchar_ttype()`](https://insightsengineering.github.io/formatters/reference/nchar_ttype.md)
  : Calculate font-specific string width
- [`open_font_dev()`](https://insightsengineering.github.io/formatters/reference/open_font_dev.md)
  [`close_font_dev()`](https://insightsengineering.github.io/formatters/reference/open_font_dev.md)
  [`debug_font_dev()`](https://insightsengineering.github.io/formatters/reference/open_font_dev.md)
  [`undebug_font_dev()`](https://insightsengineering.github.io/formatters/reference/open_font_dev.md)
  : Activate font state
