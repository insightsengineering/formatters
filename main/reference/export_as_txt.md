# Export a table-like object to plain (ASCII) text with page breaks

This function converts `x` to a `MatrixPrintForm` object via
[`matrix_form()`](https://insightsengineering.github.io/formatters/reference/matrix_form.md),
paginates it via
[`paginate_to_mpfs()`](https://insightsengineering.github.io/formatters/reference/paginate_indices.md),
converts each page to ASCII text via
[`toString()`](https://insightsengineering.github.io/formatters/reference/tostring.md),
and outputs the strings, separated by `page_break`, to `file`.

## Usage

``` r
export_as_txt(
  x,
  file = NULL,
  page_type = NULL,
  landscape = FALSE,
  pg_width = page_dim(page_type)[if (landscape) 2 else 1],
  pg_height = page_dim(page_type)[if (landscape) 1 else 2],
  font_family = "Courier",
  font_size = 8,
  lineheight = 1L,
  margins = c(top = 0.5, bottom = 0.5, left = 0.75, right = 0.75),
  paginate = TRUE,
  cpp = NA_integer_,
  lpp = NA_integer_,
  ...,
  hsep = NULL,
  indent_size = 2,
  tf_wrap = paginate,
  max_width = NULL,
  colwidths = NULL,
  min_siblings = 2,
  nosplitin = character(),
  rep_cols = NULL,
  verbose = FALSE,
  page_break = "\\s\\n",
  page_num = default_page_number(),
  fontspec = font_spec(font_family, font_size, lineheight),
  col_gap = 3,
  round_type = c("iec", "sas")
)
```

## Arguments

- x:

  (`ANY`)  
  a table-like object to export. Must have an applicable `matrix_form`
  method.

- file:

  (`string` or `NULL`)  
  if non-`NULL`, the path to write a text file to containing `x`
  rendered as ASCII text.

- page_type:

  (`string`)  
  name of a page type. See
  [`page_types`](https://insightsengineering.github.io/formatters/reference/page_types.md).
  Ignored when `pg_width` and `pg_height` are set directly.

- landscape:

  (`flag`)  
  whether the dimensions of `page_type` should be inverted for landscape
  orientation. Defaults to `FALSE`, ignored when `pg_width` and
  `pg_height` are set directly.

- pg_width:

  (`numeric(1)`)  
  page width in inches.

- pg_height:

  (`numeric(1)`)  
  page height in inches.

- font_family:

  (`string`)  
  name of a font family. An error will be thrown if the family named is
  not monospaced. Defaults to `"Courier"`.

- font_size:

  (`numeric(1)`)  
  font size. Defaults to `12`.

- lineheight:

  (`numeric(1)`)  
  line height. Defaults to `1`.

- margins:

  (`numeric(4)`)  
  named numeric vector containing `"bottom"`, `"left"`, `"top"`, and
  `"right"` margins in inches. Defaults to `.5` inches for both vertical
  margins and `.75` for both horizontal margins.

- paginate:

  (`flag`)  
  whether pagination should be performed. Defaults to `TRUE` if page
  size is specified (including the default).

- cpp:

  (`numeric(1)` or `NULL`)  
  width (in characters) per page. If `NA` (the default), this is
  calculated automatically based on the specified page size). `NULL`
  indicates no horizontal pagination should occur.

- lpp:

  (`numeric(1)` or `NULL`)  
  lines per page. If `NA` (the default), this is calculated
  automatically based on the specified page size). `NULL` indicates no
  vertical pagination should occur.

- ...:

  additional parameters passed to
  [`paginate_to_mpfs()`](https://insightsengineering.github.io/formatters/reference/paginate_indices.md).

- hsep:

  (`string`)  
  character to repeat to create header/body separator line. If `NULL`,
  the object value will be used. If `" "`, an empty separator will be
  printed. See
  [`default_hsep()`](https://insightsengineering.github.io/formatters/reference/default_horizontal_sep.md)
  for more information.

- indent_size:

  (`numeric(1)`)  
  indent size, in characters. Ignored when `x` is already a
  `MatrixPrintForm` object in favor of information there.

- tf_wrap:

  (`flag`)  
  whether the text for title, subtitles, and footnotes should be
  wrapped.

- max_width:

  (`integer(1)`, `string` or `NULL`)  
  width that title and footer (including footnotes) materials should be
  word-wrapped to. If `NULL`, it is set to the current print width of
  the session (`getOption("width")`). If set to `"auto"`, the width of
  the table (plus any table inset) is used. Parameter is ignored if
  `tf_wrap = FALSE`.

- colwidths:

  (`numeric`)  
  vector of column widths (in characters) for use in vertical
  pagination.

- min_siblings:

  (`numeric`)  
  minimum sibling rows which must appear on either side of pagination
  row for a mid-subtable split to be valid. Defaults to 2 for tables. It
  is automatically turned off (set to 0) for listings.

- nosplitin:

  (`character`)  
  list of names of subtables where page breaks are not allowed,
  regardless of other considerations. Defaults to none.

- rep_cols:

  (`numeric(1)`)  
  number of *columns* (not including row labels) to be repeated on every
  page. Defaults to 0.

- verbose:

  (`flag`)  
  whether additional informative messages about the search for
  pagination breaks should be shown. Defaults to `FALSE`.

- page_break:

  (`string`)  
  page break symbol (defaults to `"\\n\\s"`).

- page_num:

  (`string`)  
  placeholder string for page numbers. See
  [default_page_number](https://insightsengineering.github.io/formatters/reference/default_page_number.md)
  for more information. Defaults to `NULL`.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

- col_gap:

  (`numeric(1)`)  
  The number of spaces to be placed between columns in the rendered
  table (and assumed for horizontal pagination).

- round_type:

  (`"iec"` or `"sas"`)  
  the type of rounding to perform. iec, the default, peforms rounding
  compliant with IEC 60559 (see details), while sas performs
  nearest-value rounding consistent with rounding within SAS.

## Value

If `file` is `NULL`, the full paginated and concatenated string value is
returned, otherwise the output is written to `file` and no value
(invisible `NULL`) is returned.

## Details

If `x` has a `num_rep_cols` method, the value returned by it will be
used for `rep_cols` by default. Otherwise, 0 will be used.

If `x` has an applicable `do_forced_paginate` method, it will be invoked
during the pagination process.

## Examples

``` r
export_as_txt(basic_matrix_form(mtcars), pg_height = 5, pg_width = 4)
#> [1] "                      mpg    cyl\noooooooooooooooooooooooooooooooo\nMazda RX4              21     6 \nMazda RX4 Wag          21     6 \nDatsun 710            22.8    4 \nHornet 4 Drive        21.4    6 \nHornet Sportabout     18.7    8 \nValiant               18.1    6 \nDuster 360            14.3    8 \nMerc 240D             24.4    4 \nMerc 230              22.8    4 \nMerc 280              19.2    6 \nMerc 280C             17.8    6 \nMerc 450SE            16.4    8 \nMerc 450SL            17.3    8 \nMerc 450SLC           15.2    8 \nCadillac Fleetwood    10.4    8 \nLincoln Continental   10.4    8 \nChrysler Imperial     14.7    8 \nFiat 128              32.4    4 \nHonda Civic           30.4    4 \nToyota Corolla        33.9    4 \nToyota Corona         21.5    4 \nDodge Challenger      15.5    8 \nAMC Javelin           15.2    8 \nCamaro Z28            13.3    8 \nPontiac Firebird      19.2    8 \nFiat X1-9             27.3    4 \nPorsche 914-2          26     4 \nLotus Europa          30.4    4 \nFord Pantera L        15.8    8 \noooooooooooooooooooooooooooooooo\n\n\n                         page 1 of 10\n\\s\\n                      disp    hp \nooooooooooooooooooooooooooooooooo\nMazda RX4              160    110\nMazda RX4 Wag          160    110\nDatsun 710             108    93 \nHornet 4 Drive         258    110\nHornet Sportabout      360    175\nValiant                225    105\nDuster 360             360    245\nMerc 240D             146.7   62 \nMerc 230              140.8   95 \nMerc 280              167.6   123\nMerc 280C             167.6   123\nMerc 450SE            275.8   180\nMerc 450SL            275.8   180\nMerc 450SLC           275.8   180\nCadillac Fleetwood     472    205\nLincoln Continental    460    215\nChrysler Imperial      440    230\nFiat 128              78.7    66 \nHonda Civic           75.7    52 \nToyota Corolla        71.1    65 \nToyota Corona         120.1   97 \nDodge Challenger       318    150\nAMC Javelin            304    150\nCamaro Z28             350    245\nPontiac Firebird       400    175\nFiat X1-9              79     66 \nPorsche 914-2         120.3   91 \nLotus Europa          95.1    113\nFord Pantera L         351    264\nooooooooooooooooooooooooooooooooo\n\n\n                         page 2 of 10\n\\s\\n                      drat    wt  \noooooooooooooooooooooooooooooooooo\nMazda RX4             3.9    2.62 \nMazda RX4 Wag         3.9    2.875\nDatsun 710            3.85   2.32 \nHornet 4 Drive        3.08   3.215\nHornet Sportabout     3.15   3.44 \nValiant               2.76   3.46 \nDuster 360            3.21   3.57 \nMerc 240D             3.69   3.19 \nMerc 230              3.92   3.15 \nMerc 280              3.92   3.44 \nMerc 280C             3.92   3.44 \nMerc 450SE            3.07   4.07 \nMerc 450SL            3.07   3.73 \nMerc 450SLC           3.07   3.78 \nCadillac Fleetwood    2.93   5.25 \nLincoln Continental    3     5.424\nChrysler Imperial     3.23   5.345\nFiat 128              4.08    2.2 \nHonda Civic           4.93   1.615\nToyota Corolla        4.22   1.835\nToyota Corona         3.7    2.465\nDodge Challenger      2.76   3.52 \nAMC Javelin           3.15   3.435\nCamaro Z28            3.73   3.84 \nPontiac Firebird      3.08   3.845\nFiat X1-9             4.08   1.935\nPorsche 914-2         4.43   2.14 \nLotus Europa          3.77   1.513\nFord Pantera L        4.22   3.17 \noooooooooooooooooooooooooooooooooo\n\n\n                         page 3 of 10\n\\s\\n                      qsec    vs   am\nooooooooooooooooooooooooooooooooooooo\nMazda RX4             16.46   0    1 \nMazda RX4 Wag         17.02   0    1 \nDatsun 710            18.61   1    1 \nHornet 4 Drive        19.44   1    0 \nHornet Sportabout     17.02   0    0 \nValiant               20.22   1    0 \nDuster 360            15.84   0    0 \nMerc 240D              20     1    0 \nMerc 230              22.9    1    0 \nMerc 280              18.3    1    0 \nMerc 280C             18.9    1    0 \nMerc 450SE            17.4    0    0 \nMerc 450SL            17.6    0    0 \nMerc 450SLC            18     0    0 \nCadillac Fleetwood    17.98   0    0 \nLincoln Continental   17.82   0    0 \nChrysler Imperial     17.42   0    0 \nFiat 128              19.47   1    1 \nHonda Civic           18.52   1    1 \nToyota Corolla        19.9    1    1 \nToyota Corona         20.01   1    0 \nDodge Challenger      16.87   0    0 \nAMC Javelin           17.3    0    0 \nCamaro Z28            15.41   0    0 \nPontiac Firebird      17.05   0    0 \nFiat X1-9             18.9    1    1 \nPorsche 914-2         16.7    0    1 \nLotus Europa          16.9    1    1 \nFord Pantera L        14.5    0    1 \nooooooooooooooooooooooooooooooooooooo\n\n\n                         page 4 of 10\n\\s\\n                      gear   carb\nooooooooooooooooooooooooooooooooo\nMazda RX4              4      4  \nMazda RX4 Wag          4      4  \nDatsun 710             4      1  \nHornet 4 Drive         3      1  \nHornet Sportabout      3      2  \nValiant                3      1  \nDuster 360             3      4  \nMerc 240D              4      2  \nMerc 230               4      2  \nMerc 280               4      4  \nMerc 280C              4      4  \nMerc 450SE             3      3  \nMerc 450SL             3      3  \nMerc 450SLC            3      3  \nCadillac Fleetwood     3      4  \nLincoln Continental    3      4  \nChrysler Imperial      3      4  \nFiat 128               4      1  \nHonda Civic            4      2  \nToyota Corolla         4      1  \nToyota Corona          3      1  \nDodge Challenger       3      2  \nAMC Javelin            3      2  \nCamaro Z28             3      4  \nPontiac Firebird       3      2  \nFiat X1-9              4      1  \nPorsche 914-2          5      2  \nLotus Europa           5      2  \nFord Pantera L         5      4  \nooooooooooooooooooooooooooooooooo\n\n\n                         page 5 of 10\n\\s\\n                      mpg    cyl\noooooooooooooooooooooooooooooooo\nFerrari Dino          19.7    6 \nMaserati Bora          15     8 \nVolvo 142E            21.4    4 \noooooooooooooooooooooooooooooooo\n\n\n                         page 6 of 10\n\\s\\n                      disp    hp \nooooooooooooooooooooooooooooooooo\nFerrari Dino           145    175\nMaserati Bora          301    335\nVolvo 142E             121    109\nooooooooooooooooooooooooooooooooo\n\n\n                         page 7 of 10\n\\s\\n                      drat    wt  \noooooooooooooooooooooooooooooooooo\nFerrari Dino          3.62   2.77 \nMaserati Bora         3.54   3.57 \nVolvo 142E            4.11   2.78 \noooooooooooooooooooooooooooooooooo\n\n\n                         page 8 of 10\n\\s\\n                      qsec    vs   am\nooooooooooooooooooooooooooooooooooooo\nFerrari Dino          15.5    0    1 \nMaserati Bora         14.6    0    1 \nVolvo 142E            18.6    1    1 \nooooooooooooooooooooooooooooooooooooo\n\n\n                         page 9 of 10\n\\s\\n                      gear   carb\nooooooooooooooooooooooooooooooooo\nFerrari Dino           5      6  \nMaserati Bora          5      8  \nVolvo 142E             4      2  \nooooooooooooooooooooooooooooooooo\n\n\n                        page 10 of 10\n"
```
