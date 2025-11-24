# Constructor for Matrix Print Form

Constructor for `MatrixPrintForm`, an intermediate representation for
ASCII table printing.

## Usage

``` r
MatrixPrintForm(
  strings = NULL,
  spans,
  aligns,
  formats,
  row_info,
  colpaths = NULL,
  line_grouping = seq_len(NROW(strings)),
  ref_fnotes = list(),
  nlines_header,
  nrow_header,
  has_topleft = TRUE,
  has_rowlabs = has_topleft,
  expand_newlines = TRUE,
  main_title = "",
  subtitles = character(),
  page_titles = character(),
  listing_keycols = NULL,
  main_footer = "",
  prov_footer = character(),
  header_section_div = NA_character_,
  horizontal_sep = default_hsep(),
  col_gap = 3,
  table_inset = 0L,
  colwidths = NULL,
  indent_size = 2,
  fontspec = font_spec(),
  rep_cols = 0L
)
```

## Arguments

- strings:

  (`character matrix`)  
  matrix of formatted, ready-to-display strings organized as they will
  be positioned when rendered. Elements that span more than one column
  must be followed by the correct number of placeholders (typically
  either empty strings or repeats of the value).

- spans:

  (`numeric matrix`)  
  matrix of same dimension as `strings` giving the spanning information
  for each element. Must be repeated to match placeholders in `strings`.

- aligns:

  (`character matrix`)  
  matrix of same dimension as `strings` giving the text alignment
  information for each element. Must be repeated to match placeholders
  in `strings`. Must be a supported text alignment. See
  [decimal_align](https://insightsengineering.github.io/formatters/reference/decimal_align.md)
  for allowed values.

- formats:

  (`matrix`)  
  matrix of same dimension as `strings` giving the text format
  information for each element. Must be repeated to match placeholders
  in `strings`.

- row_info:

  (`data.frame`)  
  data frame with row-information necessary for pagination (see
  [`basic_pagdf()`](https://insightsengineering.github.io/formatters/reference/basic_pagdf.md)
  for more details).

- colpaths:

  (`list` or `NULL`)  
  `NULL`, or a list of paths to each leaf column, for use during
  horizontal pagination.

- line_grouping:

  (`integer`)  
  sequence of integers indicating how print lines correspond to semantic
  rows in the object. Typically this should not be set manually unless
  `expand_newlines` is set to `FALSE`.

- ref_fnotes:

  (`list`)  
  referential footnote information, if applicable.

- nlines_header:

  (`numeric(1)`)  
  number of lines taken up by the values of the header (i.e. not
  including the divider).

- nrow_header:

  (`numeric(1)`)  
  number of *rows* corresponding to the header.

- has_topleft:

  (`flag`)  
  does the corresponding table have "top left information" which should
  be treated differently when expanding newlines. Ignored if
  `expand_newlines` is `FALSE`.

- has_rowlabs:

  (`flag`)  
  do the matrices (`strings`, `spans`, `aligns`) each contain a column
  that corresponds with row labels (rather than with table cell values).
  Defaults to `TRUE`.

- expand_newlines:

  (`flag`)  
  whether the matrix form generated should expand rows whose values
  contain newlines into multiple 'physical' rows (as they will appear
  when rendered into ASCII). Defaults to `TRUE`.

- main_title:

  (`string`)  
  main title as a string.

- subtitles:

  (`character`)  
  subtitles, as a character vector.

- page_titles:

  (`character`)  
  page-specific titles, as a character vector.

- listing_keycols:

  (`character`)  
  . if matrix form of a listing, this contains the key columns as a
  character vector.

- main_footer:

  (`character`)  
  main footer, as a character vector.

- prov_footer:

  (`character`)  
  provenance footer information, as a character vector.

- header_section_div:

  (`string`)  
  divider to be used between header and body sections.

- horizontal_sep:

  (`string`)  
  horizontal separator to be used for printing divisors between header
  and table body and between different footers.

- col_gap:

  (`numeric(1)`)  
  space (in characters) between columns.

- table_inset:

  (`numeric(1)`)  
  table inset. See
  [`table_inset()`](https://insightsengineering.github.io/formatters/reference/table_inset.md).

- colwidths:

  (`numeric` or `NULL`)  
  column rendering widths. If non-`NULL`, must have length equal to
  `ncol(strings)`.

- indent_size:

  (`numeric(1)`)  
  number of spaces to be used per level of indent (if supported by the
  relevant method). Defaults to 2.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

- rep_cols:

  (`numeric(1)`)  
  number of columns to be repeated as context during horizontal
  pagination.

## Value

An object of class `MatrixPrintForm`. Currently this is implemented as
an S3 class inheriting from list with the following elements:

- `strings`:

  see argument.

- `spans`:

  see argument.

- `aligns`:

  see argument.

- `display`:

  logical matrix of same dimension as `strings` that specifies whether
  an element in `strings` will be displayed when the table is rendered.

- `formats`:

  see argument.

- `row_info`:

  see argument.

- `line_grouping`:

  see argument.

- `ref_footnotes`:

  see argument.

- `main_title`:

  see argument.

- `subtitles`:

  see argument.

- `page_titles`:

  see argument.

- `main_footer`:

  see argument.

- `prov_footer`:

  see argument.

- `header_section_div`:

  see argument.

- `horizontal_sep`:

  see argument.

- `col_gap`:

  see argument.

- `table_inset`:

  see argument.

as well as the following attributes:

- `nlines_header`:

  see argument.

- `nrow_header`:

  see argument.

- `ncols`:

  number of columns *of the table*, not including any row names/row
  labels

## Note

The bare constructor for the `MatrixPrintForm` should generally only be
called by `matrix_form` custom methods, and almost never from other
code.

## Examples

``` r
basic_matrix_form(iris) # calls matrix_form which calls this constructor
#> $strings
#>        rnms  Sepal.Length   Sepal.Width   Petal.Length   Petal.Width  
#>   [1,] ""    "Sepal.Length" "Sepal.Width" "Petal.Length" "Petal.Width"
#>   [2,] "1"   "5.1"          "3.5"         "1.4"          "0.2"        
#>   [3,] "2"   "4.9"          "3"           "1.4"          "0.2"        
#>   [4,] "3"   "4.7"          "3.2"         "1.3"          "0.2"        
#>   [5,] "4"   "4.6"          "3.1"         "1.5"          "0.2"        
#>   [6,] "5"   "5"            "3.6"         "1.4"          "0.2"        
#>   [7,] "6"   "5.4"          "3.9"         "1.7"          "0.4"        
#>   [8,] "7"   "4.6"          "3.4"         "1.4"          "0.3"        
#>   [9,] "8"   "5"            "3.4"         "1.5"          "0.2"        
#>  [10,] "9"   "4.4"          "2.9"         "1.4"          "0.2"        
#>  [11,] "10"  "4.9"          "3.1"         "1.5"          "0.1"        
#>  [12,] "11"  "5.4"          "3.7"         "1.5"          "0.2"        
#>  [13,] "12"  "4.8"          "3.4"         "1.6"          "0.2"        
#>  [14,] "13"  "4.8"          "3"           "1.4"          "0.1"        
#>  [15,] "14"  "4.3"          "3"           "1.1"          "0.1"        
#>  [16,] "15"  "5.8"          "4"           "1.2"          "0.2"        
#>  [17,] "16"  "5.7"          "4.4"         "1.5"          "0.4"        
#>  [18,] "17"  "5.4"          "3.9"         "1.3"          "0.4"        
#>  [19,] "18"  "5.1"          "3.5"         "1.4"          "0.3"        
#>  [20,] "19"  "5.7"          "3.8"         "1.7"          "0.3"        
#>  [21,] "20"  "5.1"          "3.8"         "1.5"          "0.3"        
#>  [22,] "21"  "5.4"          "3.4"         "1.7"          "0.2"        
#>  [23,] "22"  "5.1"          "3.7"         "1.5"          "0.4"        
#>  [24,] "23"  "4.6"          "3.6"         "1"            "0.2"        
#>  [25,] "24"  "5.1"          "3.3"         "1.7"          "0.5"        
#>  [26,] "25"  "4.8"          "3.4"         "1.9"          "0.2"        
#>  [27,] "26"  "5"            "3"           "1.6"          "0.2"        
#>  [28,] "27"  "5"            "3.4"         "1.6"          "0.4"        
#>  [29,] "28"  "5.2"          "3.5"         "1.5"          "0.2"        
#>  [30,] "29"  "5.2"          "3.4"         "1.4"          "0.2"        
#>  [31,] "30"  "4.7"          "3.2"         "1.6"          "0.2"        
#>  [32,] "31"  "4.8"          "3.1"         "1.6"          "0.2"        
#>  [33,] "32"  "5.4"          "3.4"         "1.5"          "0.4"        
#>  [34,] "33"  "5.2"          "4.1"         "1.5"          "0.1"        
#>  [35,] "34"  "5.5"          "4.2"         "1.4"          "0.2"        
#>  [36,] "35"  "4.9"          "3.1"         "1.5"          "0.2"        
#>  [37,] "36"  "5"            "3.2"         "1.2"          "0.2"        
#>  [38,] "37"  "5.5"          "3.5"         "1.3"          "0.2"        
#>  [39,] "38"  "4.9"          "3.6"         "1.4"          "0.1"        
#>  [40,] "39"  "4.4"          "3"           "1.3"          "0.2"        
#>  [41,] "40"  "5.1"          "3.4"         "1.5"          "0.2"        
#>  [42,] "41"  "5"            "3.5"         "1.3"          "0.3"        
#>  [43,] "42"  "4.5"          "2.3"         "1.3"          "0.3"        
#>  [44,] "43"  "4.4"          "3.2"         "1.3"          "0.2"        
#>  [45,] "44"  "5"            "3.5"         "1.6"          "0.6"        
#>  [46,] "45"  "5.1"          "3.8"         "1.9"          "0.4"        
#>  [47,] "46"  "4.8"          "3"           "1.4"          "0.3"        
#>  [48,] "47"  "5.1"          "3.8"         "1.6"          "0.2"        
#>  [49,] "48"  "4.6"          "3.2"         "1.4"          "0.2"        
#>  [50,] "49"  "5.3"          "3.7"         "1.5"          "0.2"        
#>  [51,] "50"  "5"            "3.3"         "1.4"          "0.2"        
#>  [52,] "51"  "7"            "3.2"         "4.7"          "1.4"        
#>  [53,] "52"  "6.4"          "3.2"         "4.5"          "1.5"        
#>  [54,] "53"  "6.9"          "3.1"         "4.9"          "1.5"        
#>  [55,] "54"  "5.5"          "2.3"         "4"            "1.3"        
#>  [56,] "55"  "6.5"          "2.8"         "4.6"          "1.5"        
#>  [57,] "56"  "5.7"          "2.8"         "4.5"          "1.3"        
#>  [58,] "57"  "6.3"          "3.3"         "4.7"          "1.6"        
#>  [59,] "58"  "4.9"          "2.4"         "3.3"          "1"          
#>  [60,] "59"  "6.6"          "2.9"         "4.6"          "1.3"        
#>  [61,] "60"  "5.2"          "2.7"         "3.9"          "1.4"        
#>  [62,] "61"  "5"            "2"           "3.5"          "1"          
#>  [63,] "62"  "5.9"          "3"           "4.2"          "1.5"        
#>  [64,] "63"  "6"            "2.2"         "4"            "1"          
#>  [65,] "64"  "6.1"          "2.9"         "4.7"          "1.4"        
#>  [66,] "65"  "5.6"          "2.9"         "3.6"          "1.3"        
#>  [67,] "66"  "6.7"          "3.1"         "4.4"          "1.4"        
#>  [68,] "67"  "5.6"          "3"           "4.5"          "1.5"        
#>  [69,] "68"  "5.8"          "2.7"         "4.1"          "1"          
#>  [70,] "69"  "6.2"          "2.2"         "4.5"          "1.5"        
#>  [71,] "70"  "5.6"          "2.5"         "3.9"          "1.1"        
#>  [72,] "71"  "5.9"          "3.2"         "4.8"          "1.8"        
#>  [73,] "72"  "6.1"          "2.8"         "4"            "1.3"        
#>  [74,] "73"  "6.3"          "2.5"         "4.9"          "1.5"        
#>  [75,] "74"  "6.1"          "2.8"         "4.7"          "1.2"        
#>  [76,] "75"  "6.4"          "2.9"         "4.3"          "1.3"        
#>  [77,] "76"  "6.6"          "3"           "4.4"          "1.4"        
#>  [78,] "77"  "6.8"          "2.8"         "4.8"          "1.4"        
#>  [79,] "78"  "6.7"          "3"           "5"            "1.7"        
#>  [80,] "79"  "6"            "2.9"         "4.5"          "1.5"        
#>  [81,] "80"  "5.7"          "2.6"         "3.5"          "1"          
#>  [82,] "81"  "5.5"          "2.4"         "3.8"          "1.1"        
#>  [83,] "82"  "5.5"          "2.4"         "3.7"          "1"          
#>  [84,] "83"  "5.8"          "2.7"         "3.9"          "1.2"        
#>  [85,] "84"  "6"            "2.7"         "5.1"          "1.6"        
#>  [86,] "85"  "5.4"          "3"           "4.5"          "1.5"        
#>  [87,] "86"  "6"            "3.4"         "4.5"          "1.6"        
#>  [88,] "87"  "6.7"          "3.1"         "4.7"          "1.5"        
#>  [89,] "88"  "6.3"          "2.3"         "4.4"          "1.3"        
#>  [90,] "89"  "5.6"          "3"           "4.1"          "1.3"        
#>  [91,] "90"  "5.5"          "2.5"         "4"            "1.3"        
#>  [92,] "91"  "5.5"          "2.6"         "4.4"          "1.2"        
#>  [93,] "92"  "6.1"          "3"           "4.6"          "1.4"        
#>  [94,] "93"  "5.8"          "2.6"         "4"            "1.2"        
#>  [95,] "94"  "5"            "2.3"         "3.3"          "1"          
#>  [96,] "95"  "5.6"          "2.7"         "4.2"          "1.3"        
#>  [97,] "96"  "5.7"          "3"           "4.2"          "1.2"        
#>  [98,] "97"  "5.7"          "2.9"         "4.2"          "1.3"        
#>  [99,] "98"  "6.2"          "2.9"         "4.3"          "1.3"        
#> [100,] "99"  "5.1"          "2.5"         "3"            "1.1"        
#> [101,] "100" "5.7"          "2.8"         "4.1"          "1.3"        
#> [102,] "101" "6.3"          "3.3"         "6"            "2.5"        
#> [103,] "102" "5.8"          "2.7"         "5.1"          "1.9"        
#> [104,] "103" "7.1"          "3"           "5.9"          "2.1"        
#> [105,] "104" "6.3"          "2.9"         "5.6"          "1.8"        
#> [106,] "105" "6.5"          "3"           "5.8"          "2.2"        
#> [107,] "106" "7.6"          "3"           "6.6"          "2.1"        
#> [108,] "107" "4.9"          "2.5"         "4.5"          "1.7"        
#> [109,] "108" "7.3"          "2.9"         "6.3"          "1.8"        
#> [110,] "109" "6.7"          "2.5"         "5.8"          "1.8"        
#> [111,] "110" "7.2"          "3.6"         "6.1"          "2.5"        
#> [112,] "111" "6.5"          "3.2"         "5.1"          "2"          
#> [113,] "112" "6.4"          "2.7"         "5.3"          "1.9"        
#> [114,] "113" "6.8"          "3"           "5.5"          "2.1"        
#> [115,] "114" "5.7"          "2.5"         "5"            "2"          
#> [116,] "115" "5.8"          "2.8"         "5.1"          "2.4"        
#> [117,] "116" "6.4"          "3.2"         "5.3"          "2.3"        
#> [118,] "117" "6.5"          "3"           "5.5"          "1.8"        
#> [119,] "118" "7.7"          "3.8"         "6.7"          "2.2"        
#> [120,] "119" "7.7"          "2.6"         "6.9"          "2.3"        
#> [121,] "120" "6"            "2.2"         "5"            "1.5"        
#> [122,] "121" "6.9"          "3.2"         "5.7"          "2.3"        
#> [123,] "122" "5.6"          "2.8"         "4.9"          "2"          
#> [124,] "123" "7.7"          "2.8"         "6.7"          "2"          
#> [125,] "124" "6.3"          "2.7"         "4.9"          "1.8"        
#> [126,] "125" "6.7"          "3.3"         "5.7"          "2.1"        
#> [127,] "126" "7.2"          "3.2"         "6"            "1.8"        
#> [128,] "127" "6.2"          "2.8"         "4.8"          "1.8"        
#> [129,] "128" "6.1"          "3"           "4.9"          "1.8"        
#> [130,] "129" "6.4"          "2.8"         "5.6"          "2.1"        
#> [131,] "130" "7.2"          "3"           "5.8"          "1.6"        
#> [132,] "131" "7.4"          "2.8"         "6.1"          "1.9"        
#> [133,] "132" "7.9"          "3.8"         "6.4"          "2"          
#> [134,] "133" "6.4"          "2.8"         "5.6"          "2.2"        
#> [135,] "134" "6.3"          "2.8"         "5.1"          "1.5"        
#> [136,] "135" "6.1"          "2.6"         "5.6"          "1.4"        
#> [137,] "136" "7.7"          "3"           "6.1"          "2.3"        
#> [138,] "137" "6.3"          "3.4"         "5.6"          "2.4"        
#> [139,] "138" "6.4"          "3.1"         "5.5"          "1.8"        
#> [140,] "139" "6"            "3"           "4.8"          "1.8"        
#> [141,] "140" "6.9"          "3.1"         "5.4"          "2.1"        
#> [142,] "141" "6.7"          "3.1"         "5.6"          "2.4"        
#> [143,] "142" "6.9"          "3.1"         "5.1"          "2.3"        
#> [144,] "143" "5.8"          "2.7"         "5.1"          "1.9"        
#> [145,] "144" "6.8"          "3.2"         "5.9"          "2.3"        
#> [146,] "145" "6.7"          "3.3"         "5.7"          "2.5"        
#> [147,] "146" "6.7"          "3"           "5.2"          "2.3"        
#> [148,] "147" "6.3"          "2.5"         "5"            "1.9"        
#> [149,] "148" "6.5"          "3"           "5.2"          "2"          
#> [150,] "149" "6.2"          "3.4"         "5.4"          "2.3"        
#> [151,] "150" "5.9"          "3"           "5.1"          "1.8"        
#>        Species     
#>   [1,] "Species"   
#>   [2,] "setosa"    
#>   [3,] "setosa"    
#>   [4,] "setosa"    
#>   [5,] "setosa"    
#>   [6,] "setosa"    
#>   [7,] "setosa"    
#>   [8,] "setosa"    
#>   [9,] "setosa"    
#>  [10,] "setosa"    
#>  [11,] "setosa"    
#>  [12,] "setosa"    
#>  [13,] "setosa"    
#>  [14,] "setosa"    
#>  [15,] "setosa"    
#>  [16,] "setosa"    
#>  [17,] "setosa"    
#>  [18,] "setosa"    
#>  [19,] "setosa"    
#>  [20,] "setosa"    
#>  [21,] "setosa"    
#>  [22,] "setosa"    
#>  [23,] "setosa"    
#>  [24,] "setosa"    
#>  [25,] "setosa"    
#>  [26,] "setosa"    
#>  [27,] "setosa"    
#>  [28,] "setosa"    
#>  [29,] "setosa"    
#>  [30,] "setosa"    
#>  [31,] "setosa"    
#>  [32,] "setosa"    
#>  [33,] "setosa"    
#>  [34,] "setosa"    
#>  [35,] "setosa"    
#>  [36,] "setosa"    
#>  [37,] "setosa"    
#>  [38,] "setosa"    
#>  [39,] "setosa"    
#>  [40,] "setosa"    
#>  [41,] "setosa"    
#>  [42,] "setosa"    
#>  [43,] "setosa"    
#>  [44,] "setosa"    
#>  [45,] "setosa"    
#>  [46,] "setosa"    
#>  [47,] "setosa"    
#>  [48,] "setosa"    
#>  [49,] "setosa"    
#>  [50,] "setosa"    
#>  [51,] "setosa"    
#>  [52,] "versicolor"
#>  [53,] "versicolor"
#>  [54,] "versicolor"
#>  [55,] "versicolor"
#>  [56,] "versicolor"
#>  [57,] "versicolor"
#>  [58,] "versicolor"
#>  [59,] "versicolor"
#>  [60,] "versicolor"
#>  [61,] "versicolor"
#>  [62,] "versicolor"
#>  [63,] "versicolor"
#>  [64,] "versicolor"
#>  [65,] "versicolor"
#>  [66,] "versicolor"
#>  [67,] "versicolor"
#>  [68,] "versicolor"
#>  [69,] "versicolor"
#>  [70,] "versicolor"
#>  [71,] "versicolor"
#>  [72,] "versicolor"
#>  [73,] "versicolor"
#>  [74,] "versicolor"
#>  [75,] "versicolor"
#>  [76,] "versicolor"
#>  [77,] "versicolor"
#>  [78,] "versicolor"
#>  [79,] "versicolor"
#>  [80,] "versicolor"
#>  [81,] "versicolor"
#>  [82,] "versicolor"
#>  [83,] "versicolor"
#>  [84,] "versicolor"
#>  [85,] "versicolor"
#>  [86,] "versicolor"
#>  [87,] "versicolor"
#>  [88,] "versicolor"
#>  [89,] "versicolor"
#>  [90,] "versicolor"
#>  [91,] "versicolor"
#>  [92,] "versicolor"
#>  [93,] "versicolor"
#>  [94,] "versicolor"
#>  [95,] "versicolor"
#>  [96,] "versicolor"
#>  [97,] "versicolor"
#>  [98,] "versicolor"
#>  [99,] "versicolor"
#> [100,] "versicolor"
#> [101,] "versicolor"
#> [102,] "virginica" 
#> [103,] "virginica" 
#> [104,] "virginica" 
#> [105,] "virginica" 
#> [106,] "virginica" 
#> [107,] "virginica" 
#> [108,] "virginica" 
#> [109,] "virginica" 
#> [110,] "virginica" 
#> [111,] "virginica" 
#> [112,] "virginica" 
#> [113,] "virginica" 
#> [114,] "virginica" 
#> [115,] "virginica" 
#> [116,] "virginica" 
#> [117,] "virginica" 
#> [118,] "virginica" 
#> [119,] "virginica" 
#> [120,] "virginica" 
#> [121,] "virginica" 
#> [122,] "virginica" 
#> [123,] "virginica" 
#> [124,] "virginica" 
#> [125,] "virginica" 
#> [126,] "virginica" 
#> [127,] "virginica" 
#> [128,] "virginica" 
#> [129,] "virginica" 
#> [130,] "virginica" 
#> [131,] "virginica" 
#> [132,] "virginica" 
#> [133,] "virginica" 
#> [134,] "virginica" 
#> [135,] "virginica" 
#> [136,] "virginica" 
#> [137,] "virginica" 
#> [138,] "virginica" 
#> [139,] "virginica" 
#> [140,] "virginica" 
#> [141,] "virginica" 
#> [142,] "virginica" 
#> [143,] "virginica" 
#> [144,] "virginica" 
#> [145,] "virginica" 
#> [146,] "virginica" 
#> [147,] "virginica" 
#> [148,] "virginica" 
#> [149,] "virginica" 
#> [150,] "virginica" 
#> [151,] "virginica" 
#> 
#> $spans
#>        [,1] [,2] [,3] [,4] [,5] [,6]
#>   [1,]    1    1    1    1    1    1
#>   [2,]    1    1    1    1    1    1
#>   [3,]    1    1    1    1    1    1
#>   [4,]    1    1    1    1    1    1
#>   [5,]    1    1    1    1    1    1
#>   [6,]    1    1    1    1    1    1
#>   [7,]    1    1    1    1    1    1
#>   [8,]    1    1    1    1    1    1
#>   [9,]    1    1    1    1    1    1
#>  [10,]    1    1    1    1    1    1
#>  [11,]    1    1    1    1    1    1
#>  [12,]    1    1    1    1    1    1
#>  [13,]    1    1    1    1    1    1
#>  [14,]    1    1    1    1    1    1
#>  [15,]    1    1    1    1    1    1
#>  [16,]    1    1    1    1    1    1
#>  [17,]    1    1    1    1    1    1
#>  [18,]    1    1    1    1    1    1
#>  [19,]    1    1    1    1    1    1
#>  [20,]    1    1    1    1    1    1
#>  [21,]    1    1    1    1    1    1
#>  [22,]    1    1    1    1    1    1
#>  [23,]    1    1    1    1    1    1
#>  [24,]    1    1    1    1    1    1
#>  [25,]    1    1    1    1    1    1
#>  [26,]    1    1    1    1    1    1
#>  [27,]    1    1    1    1    1    1
#>  [28,]    1    1    1    1    1    1
#>  [29,]    1    1    1    1    1    1
#>  [30,]    1    1    1    1    1    1
#>  [31,]    1    1    1    1    1    1
#>  [32,]    1    1    1    1    1    1
#>  [33,]    1    1    1    1    1    1
#>  [34,]    1    1    1    1    1    1
#>  [35,]    1    1    1    1    1    1
#>  [36,]    1    1    1    1    1    1
#>  [37,]    1    1    1    1    1    1
#>  [38,]    1    1    1    1    1    1
#>  [39,]    1    1    1    1    1    1
#>  [40,]    1    1    1    1    1    1
#>  [41,]    1    1    1    1    1    1
#>  [42,]    1    1    1    1    1    1
#>  [43,]    1    1    1    1    1    1
#>  [44,]    1    1    1    1    1    1
#>  [45,]    1    1    1    1    1    1
#>  [46,]    1    1    1    1    1    1
#>  [47,]    1    1    1    1    1    1
#>  [48,]    1    1    1    1    1    1
#>  [49,]    1    1    1    1    1    1
#>  [50,]    1    1    1    1    1    1
#>  [51,]    1    1    1    1    1    1
#>  [52,]    1    1    1    1    1    1
#>  [53,]    1    1    1    1    1    1
#>  [54,]    1    1    1    1    1    1
#>  [55,]    1    1    1    1    1    1
#>  [56,]    1    1    1    1    1    1
#>  [57,]    1    1    1    1    1    1
#>  [58,]    1    1    1    1    1    1
#>  [59,]    1    1    1    1    1    1
#>  [60,]    1    1    1    1    1    1
#>  [61,]    1    1    1    1    1    1
#>  [62,]    1    1    1    1    1    1
#>  [63,]    1    1    1    1    1    1
#>  [64,]    1    1    1    1    1    1
#>  [65,]    1    1    1    1    1    1
#>  [66,]    1    1    1    1    1    1
#>  [67,]    1    1    1    1    1    1
#>  [68,]    1    1    1    1    1    1
#>  [69,]    1    1    1    1    1    1
#>  [70,]    1    1    1    1    1    1
#>  [71,]    1    1    1    1    1    1
#>  [72,]    1    1    1    1    1    1
#>  [73,]    1    1    1    1    1    1
#>  [74,]    1    1    1    1    1    1
#>  [75,]    1    1    1    1    1    1
#>  [76,]    1    1    1    1    1    1
#>  [77,]    1    1    1    1    1    1
#>  [78,]    1    1    1    1    1    1
#>  [79,]    1    1    1    1    1    1
#>  [80,]    1    1    1    1    1    1
#>  [81,]    1    1    1    1    1    1
#>  [82,]    1    1    1    1    1    1
#>  [83,]    1    1    1    1    1    1
#>  [84,]    1    1    1    1    1    1
#>  [85,]    1    1    1    1    1    1
#>  [86,]    1    1    1    1    1    1
#>  [87,]    1    1    1    1    1    1
#>  [88,]    1    1    1    1    1    1
#>  [89,]    1    1    1    1    1    1
#>  [90,]    1    1    1    1    1    1
#>  [91,]    1    1    1    1    1    1
#>  [92,]    1    1    1    1    1    1
#>  [93,]    1    1    1    1    1    1
#>  [94,]    1    1    1    1    1    1
#>  [95,]    1    1    1    1    1    1
#>  [96,]    1    1    1    1    1    1
#>  [97,]    1    1    1    1    1    1
#>  [98,]    1    1    1    1    1    1
#>  [99,]    1    1    1    1    1    1
#> [100,]    1    1    1    1    1    1
#> [101,]    1    1    1    1    1    1
#> [102,]    1    1    1    1    1    1
#> [103,]    1    1    1    1    1    1
#> [104,]    1    1    1    1    1    1
#> [105,]    1    1    1    1    1    1
#> [106,]    1    1    1    1    1    1
#> [107,]    1    1    1    1    1    1
#> [108,]    1    1    1    1    1    1
#> [109,]    1    1    1    1    1    1
#> [110,]    1    1    1    1    1    1
#> [111,]    1    1    1    1    1    1
#> [112,]    1    1    1    1    1    1
#> [113,]    1    1    1    1    1    1
#> [114,]    1    1    1    1    1    1
#> [115,]    1    1    1    1    1    1
#> [116,]    1    1    1    1    1    1
#> [117,]    1    1    1    1    1    1
#> [118,]    1    1    1    1    1    1
#> [119,]    1    1    1    1    1    1
#> [120,]    1    1    1    1    1    1
#> [121,]    1    1    1    1    1    1
#> [122,]    1    1    1    1    1    1
#> [123,]    1    1    1    1    1    1
#> [124,]    1    1    1    1    1    1
#> [125,]    1    1    1    1    1    1
#> [126,]    1    1    1    1    1    1
#> [127,]    1    1    1    1    1    1
#> [128,]    1    1    1    1    1    1
#> [129,]    1    1    1    1    1    1
#> [130,]    1    1    1    1    1    1
#> [131,]    1    1    1    1    1    1
#> [132,]    1    1    1    1    1    1
#> [133,]    1    1    1    1    1    1
#> [134,]    1    1    1    1    1    1
#> [135,]    1    1    1    1    1    1
#> [136,]    1    1    1    1    1    1
#> [137,]    1    1    1    1    1    1
#> [138,]    1    1    1    1    1    1
#> [139,]    1    1    1    1    1    1
#> [140,]    1    1    1    1    1    1
#> [141,]    1    1    1    1    1    1
#> [142,]    1    1    1    1    1    1
#> [143,]    1    1    1    1    1    1
#> [144,]    1    1    1    1    1    1
#> [145,]    1    1    1    1    1    1
#> [146,]    1    1    1    1    1    1
#> [147,]    1    1    1    1    1    1
#> [148,]    1    1    1    1    1    1
#> [149,]    1    1    1    1    1    1
#> [150,]    1    1    1    1    1    1
#> [151,]    1    1    1    1    1    1
#> 
#> $aligns
#>        [,1]   [,2]     [,3]     [,4]     [,5]     [,6]    
#>   [1,] "left" "center" "center" "center" "center" "center"
#>   [2,] "left" "center" "center" "center" "center" "center"
#>   [3,] "left" "center" "center" "center" "center" "center"
#>   [4,] "left" "center" "center" "center" "center" "center"
#>   [5,] "left" "center" "center" "center" "center" "center"
#>   [6,] "left" "center" "center" "center" "center" "center"
#>   [7,] "left" "center" "center" "center" "center" "center"
#>   [8,] "left" "center" "center" "center" "center" "center"
#>   [9,] "left" "center" "center" "center" "center" "center"
#>  [10,] "left" "center" "center" "center" "center" "center"
#>  [11,] "left" "center" "center" "center" "center" "center"
#>  [12,] "left" "center" "center" "center" "center" "center"
#>  [13,] "left" "center" "center" "center" "center" "center"
#>  [14,] "left" "center" "center" "center" "center" "center"
#>  [15,] "left" "center" "center" "center" "center" "center"
#>  [16,] "left" "center" "center" "center" "center" "center"
#>  [17,] "left" "center" "center" "center" "center" "center"
#>  [18,] "left" "center" "center" "center" "center" "center"
#>  [19,] "left" "center" "center" "center" "center" "center"
#>  [20,] "left" "center" "center" "center" "center" "center"
#>  [21,] "left" "center" "center" "center" "center" "center"
#>  [22,] "left" "center" "center" "center" "center" "center"
#>  [23,] "left" "center" "center" "center" "center" "center"
#>  [24,] "left" "center" "center" "center" "center" "center"
#>  [25,] "left" "center" "center" "center" "center" "center"
#>  [26,] "left" "center" "center" "center" "center" "center"
#>  [27,] "left" "center" "center" "center" "center" "center"
#>  [28,] "left" "center" "center" "center" "center" "center"
#>  [29,] "left" "center" "center" "center" "center" "center"
#>  [30,] "left" "center" "center" "center" "center" "center"
#>  [31,] "left" "center" "center" "center" "center" "center"
#>  [32,] "left" "center" "center" "center" "center" "center"
#>  [33,] "left" "center" "center" "center" "center" "center"
#>  [34,] "left" "center" "center" "center" "center" "center"
#>  [35,] "left" "center" "center" "center" "center" "center"
#>  [36,] "left" "center" "center" "center" "center" "center"
#>  [37,] "left" "center" "center" "center" "center" "center"
#>  [38,] "left" "center" "center" "center" "center" "center"
#>  [39,] "left" "center" "center" "center" "center" "center"
#>  [40,] "left" "center" "center" "center" "center" "center"
#>  [41,] "left" "center" "center" "center" "center" "center"
#>  [42,] "left" "center" "center" "center" "center" "center"
#>  [43,] "left" "center" "center" "center" "center" "center"
#>  [44,] "left" "center" "center" "center" "center" "center"
#>  [45,] "left" "center" "center" "center" "center" "center"
#>  [46,] "left" "center" "center" "center" "center" "center"
#>  [47,] "left" "center" "center" "center" "center" "center"
#>  [48,] "left" "center" "center" "center" "center" "center"
#>  [49,] "left" "center" "center" "center" "center" "center"
#>  [50,] "left" "center" "center" "center" "center" "center"
#>  [51,] "left" "center" "center" "center" "center" "center"
#>  [52,] "left" "center" "center" "center" "center" "center"
#>  [53,] "left" "center" "center" "center" "center" "center"
#>  [54,] "left" "center" "center" "center" "center" "center"
#>  [55,] "left" "center" "center" "center" "center" "center"
#>  [56,] "left" "center" "center" "center" "center" "center"
#>  [57,] "left" "center" "center" "center" "center" "center"
#>  [58,] "left" "center" "center" "center" "center" "center"
#>  [59,] "left" "center" "center" "center" "center" "center"
#>  [60,] "left" "center" "center" "center" "center" "center"
#>  [61,] "left" "center" "center" "center" "center" "center"
#>  [62,] "left" "center" "center" "center" "center" "center"
#>  [63,] "left" "center" "center" "center" "center" "center"
#>  [64,] "left" "center" "center" "center" "center" "center"
#>  [65,] "left" "center" "center" "center" "center" "center"
#>  [66,] "left" "center" "center" "center" "center" "center"
#>  [67,] "left" "center" "center" "center" "center" "center"
#>  [68,] "left" "center" "center" "center" "center" "center"
#>  [69,] "left" "center" "center" "center" "center" "center"
#>  [70,] "left" "center" "center" "center" "center" "center"
#>  [71,] "left" "center" "center" "center" "center" "center"
#>  [72,] "left" "center" "center" "center" "center" "center"
#>  [73,] "left" "center" "center" "center" "center" "center"
#>  [74,] "left" "center" "center" "center" "center" "center"
#>  [75,] "left" "center" "center" "center" "center" "center"
#>  [76,] "left" "center" "center" "center" "center" "center"
#>  [77,] "left" "center" "center" "center" "center" "center"
#>  [78,] "left" "center" "center" "center" "center" "center"
#>  [79,] "left" "center" "center" "center" "center" "center"
#>  [80,] "left" "center" "center" "center" "center" "center"
#>  [81,] "left" "center" "center" "center" "center" "center"
#>  [82,] "left" "center" "center" "center" "center" "center"
#>  [83,] "left" "center" "center" "center" "center" "center"
#>  [84,] "left" "center" "center" "center" "center" "center"
#>  [85,] "left" "center" "center" "center" "center" "center"
#>  [86,] "left" "center" "center" "center" "center" "center"
#>  [87,] "left" "center" "center" "center" "center" "center"
#>  [88,] "left" "center" "center" "center" "center" "center"
#>  [89,] "left" "center" "center" "center" "center" "center"
#>  [90,] "left" "center" "center" "center" "center" "center"
#>  [91,] "left" "center" "center" "center" "center" "center"
#>  [92,] "left" "center" "center" "center" "center" "center"
#>  [93,] "left" "center" "center" "center" "center" "center"
#>  [94,] "left" "center" "center" "center" "center" "center"
#>  [95,] "left" "center" "center" "center" "center" "center"
#>  [96,] "left" "center" "center" "center" "center" "center"
#>  [97,] "left" "center" "center" "center" "center" "center"
#>  [98,] "left" "center" "center" "center" "center" "center"
#>  [99,] "left" "center" "center" "center" "center" "center"
#> [100,] "left" "center" "center" "center" "center" "center"
#> [101,] "left" "center" "center" "center" "center" "center"
#> [102,] "left" "center" "center" "center" "center" "center"
#> [103,] "left" "center" "center" "center" "center" "center"
#> [104,] "left" "center" "center" "center" "center" "center"
#> [105,] "left" "center" "center" "center" "center" "center"
#> [106,] "left" "center" "center" "center" "center" "center"
#> [107,] "left" "center" "center" "center" "center" "center"
#> [108,] "left" "center" "center" "center" "center" "center"
#> [109,] "left" "center" "center" "center" "center" "center"
#> [110,] "left" "center" "center" "center" "center" "center"
#> [111,] "left" "center" "center" "center" "center" "center"
#> [112,] "left" "center" "center" "center" "center" "center"
#> [113,] "left" "center" "center" "center" "center" "center"
#> [114,] "left" "center" "center" "center" "center" "center"
#> [115,] "left" "center" "center" "center" "center" "center"
#> [116,] "left" "center" "center" "center" "center" "center"
#> [117,] "left" "center" "center" "center" "center" "center"
#> [118,] "left" "center" "center" "center" "center" "center"
#> [119,] "left" "center" "center" "center" "center" "center"
#> [120,] "left" "center" "center" "center" "center" "center"
#> [121,] "left" "center" "center" "center" "center" "center"
#> [122,] "left" "center" "center" "center" "center" "center"
#> [123,] "left" "center" "center" "center" "center" "center"
#> [124,] "left" "center" "center" "center" "center" "center"
#> [125,] "left" "center" "center" "center" "center" "center"
#> [126,] "left" "center" "center" "center" "center" "center"
#> [127,] "left" "center" "center" "center" "center" "center"
#> [128,] "left" "center" "center" "center" "center" "center"
#> [129,] "left" "center" "center" "center" "center" "center"
#> [130,] "left" "center" "center" "center" "center" "center"
#> [131,] "left" "center" "center" "center" "center" "center"
#> [132,] "left" "center" "center" "center" "center" "center"
#> [133,] "left" "center" "center" "center" "center" "center"
#> [134,] "left" "center" "center" "center" "center" "center"
#> [135,] "left" "center" "center" "center" "center" "center"
#> [136,] "left" "center" "center" "center" "center" "center"
#> [137,] "left" "center" "center" "center" "center" "center"
#> [138,] "left" "center" "center" "center" "center" "center"
#> [139,] "left" "center" "center" "center" "center" "center"
#> [140,] "left" "center" "center" "center" "center" "center"
#> [141,] "left" "center" "center" "center" "center" "center"
#> [142,] "left" "center" "center" "center" "center" "center"
#> [143,] "left" "center" "center" "center" "center" "center"
#> [144,] "left" "center" "center" "center" "center" "center"
#> [145,] "left" "center" "center" "center" "center" "center"
#> [146,] "left" "center" "center" "center" "center" "center"
#> [147,] "left" "center" "center" "center" "center" "center"
#> [148,] "left" "center" "center" "center" "center" "center"
#> [149,] "left" "center" "center" "center" "center" "center"
#> [150,] "left" "center" "center" "center" "center" "center"
#> [151,] "left" "center" "center" "center" "center" "center"
#> 
#> $display
#>        [,1] [,2] [,3] [,4] [,5] [,6]
#>   [1,] TRUE TRUE TRUE TRUE TRUE TRUE
#>   [2,] TRUE TRUE TRUE TRUE TRUE TRUE
#>   [3,] TRUE TRUE TRUE TRUE TRUE TRUE
#>   [4,] TRUE TRUE TRUE TRUE TRUE TRUE
#>   [5,] TRUE TRUE TRUE TRUE TRUE TRUE
#>   [6,] TRUE TRUE TRUE TRUE TRUE TRUE
#>   [7,] TRUE TRUE TRUE TRUE TRUE TRUE
#>   [8,] TRUE TRUE TRUE TRUE TRUE TRUE
#>   [9,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [10,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [11,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [12,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [13,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [14,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [15,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [16,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [17,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [18,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [19,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [20,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [21,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [22,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [23,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [24,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [25,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [26,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [27,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [28,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [29,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [30,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [31,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [32,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [33,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [34,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [35,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [36,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [37,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [38,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [39,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [40,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [41,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [42,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [43,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [44,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [45,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [46,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [47,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [48,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [49,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [50,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [51,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [52,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [53,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [54,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [55,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [56,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [57,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [58,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [59,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [60,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [61,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [62,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [63,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [64,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [65,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [66,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [67,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [68,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [69,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [70,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [71,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [72,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [73,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [74,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [75,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [76,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [77,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [78,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [79,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [80,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [81,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [82,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [83,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [84,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [85,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [86,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [87,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [88,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [89,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [90,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [91,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [92,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [93,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [94,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [95,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [96,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [97,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [98,] TRUE TRUE TRUE TRUE TRUE TRUE
#>  [99,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [100,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [101,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [102,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [103,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [104,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [105,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [106,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [107,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [108,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [109,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [110,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [111,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [112,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [113,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [114,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [115,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [116,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [117,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [118,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [119,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [120,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [121,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [122,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [123,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [124,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [125,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [126,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [127,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [128,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [129,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [130,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [131,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [132,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [133,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [134,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [135,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [136,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [137,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [138,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [139,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [140,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [141,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [142,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [143,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [144,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [145,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [146,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [147,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [148,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [149,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [150,] TRUE TRUE TRUE TRUE TRUE TRUE
#> [151,] TRUE TRUE TRUE TRUE TRUE TRUE
#> 
#> $formats
#>     rnms Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1                                                                 
#> 2                  xx          xx           xx          xx      xx
#> 3                  xx          xx           xx          xx      xx
#> 4                  xx          xx           xx          xx      xx
#> 5                  xx          xx           xx          xx      xx
#> 6                  xx          xx           xx          xx      xx
#> 7                  xx          xx           xx          xx      xx
#> 8                  xx          xx           xx          xx      xx
#> 9                  xx          xx           xx          xx      xx
#> 10                 xx          xx           xx          xx      xx
#> 11                 xx          xx           xx          xx      xx
#> 12                 xx          xx           xx          xx      xx
#> 13                 xx          xx           xx          xx      xx
#> 14                 xx          xx           xx          xx      xx
#> 15                 xx          xx           xx          xx      xx
#> 16                 xx          xx           xx          xx      xx
#> 17                 xx          xx           xx          xx      xx
#> 18                 xx          xx           xx          xx      xx
#> 19                 xx          xx           xx          xx      xx
#> 20                 xx          xx           xx          xx      xx
#> 21                 xx          xx           xx          xx      xx
#> 22                 xx          xx           xx          xx      xx
#> 23                 xx          xx           xx          xx      xx
#> 24                 xx          xx           xx          xx      xx
#> 25                 xx          xx           xx          xx      xx
#> 26                 xx          xx           xx          xx      xx
#> 27                 xx          xx           xx          xx      xx
#> 28                 xx          xx           xx          xx      xx
#> 29                 xx          xx           xx          xx      xx
#> 30                 xx          xx           xx          xx      xx
#> 31                 xx          xx           xx          xx      xx
#> 32                 xx          xx           xx          xx      xx
#> 33                 xx          xx           xx          xx      xx
#> 34                 xx          xx           xx          xx      xx
#> 35                 xx          xx           xx          xx      xx
#> 36                 xx          xx           xx          xx      xx
#> 37                 xx          xx           xx          xx      xx
#> 38                 xx          xx           xx          xx      xx
#> 39                 xx          xx           xx          xx      xx
#> 40                 xx          xx           xx          xx      xx
#> 41                 xx          xx           xx          xx      xx
#> 42                 xx          xx           xx          xx      xx
#> 43                 xx          xx           xx          xx      xx
#> 44                 xx          xx           xx          xx      xx
#> 45                 xx          xx           xx          xx      xx
#> 46                 xx          xx           xx          xx      xx
#> 47                 xx          xx           xx          xx      xx
#> 48                 xx          xx           xx          xx      xx
#> 49                 xx          xx           xx          xx      xx
#> 50                 xx          xx           xx          xx      xx
#> 51                 xx          xx           xx          xx      xx
#> 52                 xx          xx           xx          xx      xx
#> 53                 xx          xx           xx          xx      xx
#> 54                 xx          xx           xx          xx      xx
#> 55                 xx          xx           xx          xx      xx
#> 56                 xx          xx           xx          xx      xx
#> 57                 xx          xx           xx          xx      xx
#> 58                 xx          xx           xx          xx      xx
#> 59                 xx          xx           xx          xx      xx
#> 60                 xx          xx           xx          xx      xx
#> 61                 xx          xx           xx          xx      xx
#> 62                 xx          xx           xx          xx      xx
#> 63                 xx          xx           xx          xx      xx
#> 64                 xx          xx           xx          xx      xx
#> 65                 xx          xx           xx          xx      xx
#> 66                 xx          xx           xx          xx      xx
#> 67                 xx          xx           xx          xx      xx
#> 68                 xx          xx           xx          xx      xx
#> 69                 xx          xx           xx          xx      xx
#> 70                 xx          xx           xx          xx      xx
#> 71                 xx          xx           xx          xx      xx
#> 72                 xx          xx           xx          xx      xx
#> 73                 xx          xx           xx          xx      xx
#> 74                 xx          xx           xx          xx      xx
#> 75                 xx          xx           xx          xx      xx
#> 76                 xx          xx           xx          xx      xx
#> 77                 xx          xx           xx          xx      xx
#> 78                 xx          xx           xx          xx      xx
#> 79                 xx          xx           xx          xx      xx
#> 80                 xx          xx           xx          xx      xx
#> 81                 xx          xx           xx          xx      xx
#> 82                 xx          xx           xx          xx      xx
#> 83                 xx          xx           xx          xx      xx
#> 84                 xx          xx           xx          xx      xx
#> 85                 xx          xx           xx          xx      xx
#> 86                 xx          xx           xx          xx      xx
#> 87                 xx          xx           xx          xx      xx
#> 88                 xx          xx           xx          xx      xx
#> 89                 xx          xx           xx          xx      xx
#> 90                 xx          xx           xx          xx      xx
#> 91                 xx          xx           xx          xx      xx
#> 92                 xx          xx           xx          xx      xx
#> 93                 xx          xx           xx          xx      xx
#> 94                 xx          xx           xx          xx      xx
#> 95                 xx          xx           xx          xx      xx
#> 96                 xx          xx           xx          xx      xx
#> 97                 xx          xx           xx          xx      xx
#> 98                 xx          xx           xx          xx      xx
#> 99                 xx          xx           xx          xx      xx
#> 100                xx          xx           xx          xx      xx
#> 101                xx          xx           xx          xx      xx
#> 102                xx          xx           xx          xx      xx
#> 103                xx          xx           xx          xx      xx
#> 104                xx          xx           xx          xx      xx
#> 105                xx          xx           xx          xx      xx
#> 106                xx          xx           xx          xx      xx
#> 107                xx          xx           xx          xx      xx
#> 108                xx          xx           xx          xx      xx
#> 109                xx          xx           xx          xx      xx
#> 110                xx          xx           xx          xx      xx
#> 111                xx          xx           xx          xx      xx
#> 112                xx          xx           xx          xx      xx
#> 113                xx          xx           xx          xx      xx
#> 114                xx          xx           xx          xx      xx
#> 115                xx          xx           xx          xx      xx
#> 116                xx          xx           xx          xx      xx
#> 117                xx          xx           xx          xx      xx
#> 118                xx          xx           xx          xx      xx
#> 119                xx          xx           xx          xx      xx
#> 120                xx          xx           xx          xx      xx
#> 121                xx          xx           xx          xx      xx
#> 122                xx          xx           xx          xx      xx
#> 123                xx          xx           xx          xx      xx
#> 124                xx          xx           xx          xx      xx
#> 125                xx          xx           xx          xx      xx
#> 126                xx          xx           xx          xx      xx
#> 127                xx          xx           xx          xx      xx
#> 128                xx          xx           xx          xx      xx
#> 129                xx          xx           xx          xx      xx
#> 130                xx          xx           xx          xx      xx
#> 131                xx          xx           xx          xx      xx
#> 132                xx          xx           xx          xx      xx
#> 133                xx          xx           xx          xx      xx
#> 134                xx          xx           xx          xx      xx
#> 135                xx          xx           xx          xx      xx
#> 136                xx          xx           xx          xx      xx
#> 137                xx          xx           xx          xx      xx
#> 138                xx          xx           xx          xx      xx
#> 139                xx          xx           xx          xx      xx
#> 140                xx          xx           xx          xx      xx
#> 141                xx          xx           xx          xx      xx
#> 142                xx          xx           xx          xx      xx
#> 143                xx          xx           xx          xx      xx
#> 144                xx          xx           xx          xx      xx
#> 145                xx          xx           xx          xx      xx
#> 146                xx          xx           xx          xx      xx
#> 147                xx          xx           xx          xx      xx
#> 148                xx          xx           xx          xx      xx
#> 149                xx          xx           xx          xx      xx
#> 150                xx          xx           xx          xx      xx
#> 151                xx          xx           xx          xx      xx
#> 
#> $row_info
#>     label name abs_rownumber path pos_in_siblings n_siblings self_extent
#> 1       1    1             1    1               1        150           1
#> 2       2    2             2    2               2        150           1
#> 3       3    3             3    3               3        150           1
#> 4       4    4             4    4               4        150           1
#> 5       5    5             5    5               5        150           1
#> 6       6    6             6    6               6        150           1
#> 7       7    7             7    7               7        150           1
#> 8       8    8             8    8               8        150           1
#> 9       9    9             9    9               9        150           1
#> 10     10   10            10   10              10        150           1
#> 11     11   11            11   11              11        150           1
#> 12     12   12            12   12              12        150           1
#> 13     13   13            13   13              13        150           1
#> 14     14   14            14   14              14        150           1
#> 15     15   15            15   15              15        150           1
#> 16     16   16            16   16              16        150           1
#> 17     17   17            17   17              17        150           1
#> 18     18   18            18   18              18        150           1
#> 19     19   19            19   19              19        150           1
#> 20     20   20            20   20              20        150           1
#> 21     21   21            21   21              21        150           1
#> 22     22   22            22   22              22        150           1
#> 23     23   23            23   23              23        150           1
#> 24     24   24            24   24              24        150           1
#> 25     25   25            25   25              25        150           1
#> 26     26   26            26   26              26        150           1
#> 27     27   27            27   27              27        150           1
#> 28     28   28            28   28              28        150           1
#> 29     29   29            29   29              29        150           1
#> 30     30   30            30   30              30        150           1
#> 31     31   31            31   31              31        150           1
#> 32     32   32            32   32              32        150           1
#> 33     33   33            33   33              33        150           1
#> 34     34   34            34   34              34        150           1
#> 35     35   35            35   35              35        150           1
#> 36     36   36            36   36              36        150           1
#> 37     37   37            37   37              37        150           1
#> 38     38   38            38   38              38        150           1
#> 39     39   39            39   39              39        150           1
#> 40     40   40            40   40              40        150           1
#> 41     41   41            41   41              41        150           1
#> 42     42   42            42   42              42        150           1
#> 43     43   43            43   43              43        150           1
#> 44     44   44            44   44              44        150           1
#> 45     45   45            45   45              45        150           1
#> 46     46   46            46   46              46        150           1
#> 47     47   47            47   47              47        150           1
#> 48     48   48            48   48              48        150           1
#> 49     49   49            49   49              49        150           1
#> 50     50   50            50   50              50        150           1
#> 51     51   51            51   51              51        150           1
#> 52     52   52            52   52              52        150           1
#> 53     53   53            53   53              53        150           1
#> 54     54   54            54   54              54        150           1
#> 55     55   55            55   55              55        150           1
#> 56     56   56            56   56              56        150           1
#> 57     57   57            57   57              57        150           1
#> 58     58   58            58   58              58        150           1
#> 59     59   59            59   59              59        150           1
#> 60     60   60            60   60              60        150           1
#> 61     61   61            61   61              61        150           1
#> 62     62   62            62   62              62        150           1
#> 63     63   63            63   63              63        150           1
#> 64     64   64            64   64              64        150           1
#> 65     65   65            65   65              65        150           1
#> 66     66   66            66   66              66        150           1
#> 67     67   67            67   67              67        150           1
#> 68     68   68            68   68              68        150           1
#> 69     69   69            69   69              69        150           1
#> 70     70   70            70   70              70        150           1
#> 71     71   71            71   71              71        150           1
#> 72     72   72            72   72              72        150           1
#> 73     73   73            73   73              73        150           1
#> 74     74   74            74   74              74        150           1
#> 75     75   75            75   75              75        150           1
#> 76     76   76            76   76              76        150           1
#> 77     77   77            77   77              77        150           1
#> 78     78   78            78   78              78        150           1
#> 79     79   79            79   79              79        150           1
#> 80     80   80            80   80              80        150           1
#> 81     81   81            81   81              81        150           1
#> 82     82   82            82   82              82        150           1
#> 83     83   83            83   83              83        150           1
#> 84     84   84            84   84              84        150           1
#> 85     85   85            85   85              85        150           1
#> 86     86   86            86   86              86        150           1
#> 87     87   87            87   87              87        150           1
#> 88     88   88            88   88              88        150           1
#> 89     89   89            89   89              89        150           1
#> 90     90   90            90   90              90        150           1
#> 91     91   91            91   91              91        150           1
#> 92     92   92            92   92              92        150           1
#> 93     93   93            93   93              93        150           1
#> 94     94   94            94   94              94        150           1
#> 95     95   95            95   95              95        150           1
#> 96     96   96            96   96              96        150           1
#> 97     97   97            97   97              97        150           1
#> 98     98   98            98   98              98        150           1
#> 99     99   99            99   99              99        150           1
#> 100   100  100           100  100             100        150           1
#> 101   101  101           101  101             101        150           1
#> 102   102  102           102  102             102        150           1
#> 103   103  103           103  103             103        150           1
#> 104   104  104           104  104             104        150           1
#> 105   105  105           105  105             105        150           1
#> 106   106  106           106  106             106        150           1
#> 107   107  107           107  107             107        150           1
#> 108   108  108           108  108             108        150           1
#> 109   109  109           109  109             109        150           1
#> 110   110  110           110  110             110        150           1
#> 111   111  111           111  111             111        150           1
#> 112   112  112           112  112             112        150           1
#> 113   113  113           113  113             113        150           1
#> 114   114  114           114  114             114        150           1
#> 115   115  115           115  115             115        150           1
#> 116   116  116           116  116             116        150           1
#> 117   117  117           117  117             117        150           1
#> 118   118  118           118  118             118        150           1
#> 119   119  119           119  119             119        150           1
#> 120   120  120           120  120             120        150           1
#> 121   121  121           121  121             121        150           1
#> 122   122  122           122  122             122        150           1
#> 123   123  123           123  123             123        150           1
#> 124   124  124           124  124             124        150           1
#> 125   125  125           125  125             125        150           1
#> 126   126  126           126  126             126        150           1
#> 127   127  127           127  127             127        150           1
#> 128   128  128           128  128             128        150           1
#> 129   129  129           129  129             129        150           1
#> 130   130  130           130  130             130        150           1
#> 131   131  131           131  131             131        150           1
#> 132   132  132           132  132             132        150           1
#> 133   133  133           133  133             133        150           1
#> 134   134  134           134  134             134        150           1
#> 135   135  135           135  135             135        150           1
#> 136   136  136           136  136             136        150           1
#> 137   137  137           137  137             137        150           1
#> 138   138  138           138  138             138        150           1
#> 139   139  139           139  139             139        150           1
#> 140   140  140           140  140             140        150           1
#> 141   141  141           141  141             141        150           1
#> 142   142  142           142  142             142        150           1
#> 143   143  143           143  143             143        150           1
#> 144   144  144           144  144             144        150           1
#> 145   145  145           145  145             145        150           1
#> 146   146  146           146  146             146        150           1
#> 147   147  147           147  147             147        150           1
#> 148   148  148           148  148             148        150           1
#> 149   149  149           149  149             149        150           1
#> 150   150  150           150  150             150        150           1
#>     par_extent reprint_inds node_class indent nrowrefs ncellrefs nreflines
#> 1            0                 DataRow      0        0         0         0
#> 2            0                 DataRow      0        0         0         0
#> 3            0                 DataRow      0        0         0         0
#> 4            0                 DataRow      0        0         0         0
#> 5            0                 DataRow      0        0         0         0
#> 6            0                 DataRow      0        0         0         0
#> 7            0                 DataRow      0        0         0         0
#> 8            0                 DataRow      0        0         0         0
#> 9            0                 DataRow      0        0         0         0
#> 10           0                 DataRow      0        0         0         0
#> 11           0                 DataRow      0        0         0         0
#> 12           0                 DataRow      0        0         0         0
#> 13           0                 DataRow      0        0         0         0
#> 14           0                 DataRow      0        0         0         0
#> 15           0                 DataRow      0        0         0         0
#> 16           0                 DataRow      0        0         0         0
#> 17           0                 DataRow      0        0         0         0
#> 18           0                 DataRow      0        0         0         0
#> 19           0                 DataRow      0        0         0         0
#> 20           0                 DataRow      0        0         0         0
#> 21           0                 DataRow      0        0         0         0
#> 22           0                 DataRow      0        0         0         0
#> 23           0                 DataRow      0        0         0         0
#> 24           0                 DataRow      0        0         0         0
#> 25           0                 DataRow      0        0         0         0
#> 26           0                 DataRow      0        0         0         0
#> 27           0                 DataRow      0        0         0         0
#> 28           0                 DataRow      0        0         0         0
#> 29           0                 DataRow      0        0         0         0
#> 30           0                 DataRow      0        0         0         0
#> 31           0                 DataRow      0        0         0         0
#> 32           0                 DataRow      0        0         0         0
#> 33           0                 DataRow      0        0         0         0
#> 34           0                 DataRow      0        0         0         0
#> 35           0                 DataRow      0        0         0         0
#> 36           0                 DataRow      0        0         0         0
#> 37           0                 DataRow      0        0         0         0
#> 38           0                 DataRow      0        0         0         0
#> 39           0                 DataRow      0        0         0         0
#> 40           0                 DataRow      0        0         0         0
#> 41           0                 DataRow      0        0         0         0
#> 42           0                 DataRow      0        0         0         0
#> 43           0                 DataRow      0        0         0         0
#> 44           0                 DataRow      0        0         0         0
#> 45           0                 DataRow      0        0         0         0
#> 46           0                 DataRow      0        0         0         0
#> 47           0                 DataRow      0        0         0         0
#> 48           0                 DataRow      0        0         0         0
#> 49           0                 DataRow      0        0         0         0
#> 50           0                 DataRow      0        0         0         0
#> 51           0                 DataRow      0        0         0         0
#> 52           0                 DataRow      0        0         0         0
#> 53           0                 DataRow      0        0         0         0
#> 54           0                 DataRow      0        0         0         0
#> 55           0                 DataRow      0        0         0         0
#> 56           0                 DataRow      0        0         0         0
#> 57           0                 DataRow      0        0         0         0
#> 58           0                 DataRow      0        0         0         0
#> 59           0                 DataRow      0        0         0         0
#> 60           0                 DataRow      0        0         0         0
#> 61           0                 DataRow      0        0         0         0
#> 62           0                 DataRow      0        0         0         0
#> 63           0                 DataRow      0        0         0         0
#> 64           0                 DataRow      0        0         0         0
#> 65           0                 DataRow      0        0         0         0
#> 66           0                 DataRow      0        0         0         0
#> 67           0                 DataRow      0        0         0         0
#> 68           0                 DataRow      0        0         0         0
#> 69           0                 DataRow      0        0         0         0
#> 70           0                 DataRow      0        0         0         0
#> 71           0                 DataRow      0        0         0         0
#> 72           0                 DataRow      0        0         0         0
#> 73           0                 DataRow      0        0         0         0
#> 74           0                 DataRow      0        0         0         0
#> 75           0                 DataRow      0        0         0         0
#> 76           0                 DataRow      0        0         0         0
#> 77           0                 DataRow      0        0         0         0
#> 78           0                 DataRow      0        0         0         0
#> 79           0                 DataRow      0        0         0         0
#> 80           0                 DataRow      0        0         0         0
#> 81           0                 DataRow      0        0         0         0
#> 82           0                 DataRow      0        0         0         0
#> 83           0                 DataRow      0        0         0         0
#> 84           0                 DataRow      0        0         0         0
#> 85           0                 DataRow      0        0         0         0
#> 86           0                 DataRow      0        0         0         0
#> 87           0                 DataRow      0        0         0         0
#> 88           0                 DataRow      0        0         0         0
#> 89           0                 DataRow      0        0         0         0
#> 90           0                 DataRow      0        0         0         0
#> 91           0                 DataRow      0        0         0         0
#> 92           0                 DataRow      0        0         0         0
#> 93           0                 DataRow      0        0         0         0
#> 94           0                 DataRow      0        0         0         0
#> 95           0                 DataRow      0        0         0         0
#> 96           0                 DataRow      0        0         0         0
#> 97           0                 DataRow      0        0         0         0
#> 98           0                 DataRow      0        0         0         0
#> 99           0                 DataRow      0        0         0         0
#> 100          0                 DataRow      0        0         0         0
#> 101          0                 DataRow      0        0         0         0
#> 102          0                 DataRow      0        0         0         0
#> 103          0                 DataRow      0        0         0         0
#> 104          0                 DataRow      0        0         0         0
#> 105          0                 DataRow      0        0         0         0
#> 106          0                 DataRow      0        0         0         0
#> 107          0                 DataRow      0        0         0         0
#> 108          0                 DataRow      0        0         0         0
#> 109          0                 DataRow      0        0         0         0
#> 110          0                 DataRow      0        0         0         0
#> 111          0                 DataRow      0        0         0         0
#> 112          0                 DataRow      0        0         0         0
#> 113          0                 DataRow      0        0         0         0
#> 114          0                 DataRow      0        0         0         0
#> 115          0                 DataRow      0        0         0         0
#> 116          0                 DataRow      0        0         0         0
#> 117          0                 DataRow      0        0         0         0
#> 118          0                 DataRow      0        0         0         0
#> 119          0                 DataRow      0        0         0         0
#> 120          0                 DataRow      0        0         0         0
#> 121          0                 DataRow      0        0         0         0
#> 122          0                 DataRow      0        0         0         0
#> 123          0                 DataRow      0        0         0         0
#> 124          0                 DataRow      0        0         0         0
#> 125          0                 DataRow      0        0         0         0
#> 126          0                 DataRow      0        0         0         0
#> 127          0                 DataRow      0        0         0         0
#> 128          0                 DataRow      0        0         0         0
#> 129          0                 DataRow      0        0         0         0
#> 130          0                 DataRow      0        0         0         0
#> 131          0                 DataRow      0        0         0         0
#> 132          0                 DataRow      0        0         0         0
#> 133          0                 DataRow      0        0         0         0
#> 134          0                 DataRow      0        0         0         0
#> 135          0                 DataRow      0        0         0         0
#> 136          0                 DataRow      0        0         0         0
#> 137          0                 DataRow      0        0         0         0
#> 138          0                 DataRow      0        0         0         0
#> 139          0                 DataRow      0        0         0         0
#> 140          0                 DataRow      0        0         0         0
#> 141          0                 DataRow      0        0         0         0
#> 142          0                 DataRow      0        0         0         0
#> 143          0                 DataRow      0        0         0         0
#> 144          0                 DataRow      0        0         0         0
#> 145          0                 DataRow      0        0         0         0
#> 146          0                 DataRow      0        0         0         0
#> 147          0                 DataRow      0        0         0         0
#> 148          0                 DataRow      0        0         0         0
#> 149          0                 DataRow      0        0         0         0
#> 150          0                 DataRow      0        0         0         0
#>     force_page page_title trailing_sep
#> 1        FALSE       <NA>         <NA>
#> 2        FALSE       <NA>         <NA>
#> 3        FALSE       <NA>         <NA>
#> 4        FALSE       <NA>         <NA>
#> 5        FALSE       <NA>         <NA>
#> 6        FALSE       <NA>         <NA>
#> 7        FALSE       <NA>         <NA>
#> 8        FALSE       <NA>         <NA>
#> 9        FALSE       <NA>         <NA>
#> 10       FALSE       <NA>         <NA>
#> 11       FALSE       <NA>         <NA>
#> 12       FALSE       <NA>         <NA>
#> 13       FALSE       <NA>         <NA>
#> 14       FALSE       <NA>         <NA>
#> 15       FALSE       <NA>         <NA>
#> 16       FALSE       <NA>         <NA>
#> 17       FALSE       <NA>         <NA>
#> 18       FALSE       <NA>         <NA>
#> 19       FALSE       <NA>         <NA>
#> 20       FALSE       <NA>         <NA>
#> 21       FALSE       <NA>         <NA>
#> 22       FALSE       <NA>         <NA>
#> 23       FALSE       <NA>         <NA>
#> 24       FALSE       <NA>         <NA>
#> 25       FALSE       <NA>         <NA>
#> 26       FALSE       <NA>         <NA>
#> 27       FALSE       <NA>         <NA>
#> 28       FALSE       <NA>         <NA>
#> 29       FALSE       <NA>         <NA>
#> 30       FALSE       <NA>         <NA>
#> 31       FALSE       <NA>         <NA>
#> 32       FALSE       <NA>         <NA>
#> 33       FALSE       <NA>         <NA>
#> 34       FALSE       <NA>         <NA>
#> 35       FALSE       <NA>         <NA>
#> 36       FALSE       <NA>         <NA>
#> 37       FALSE       <NA>         <NA>
#> 38       FALSE       <NA>         <NA>
#> 39       FALSE       <NA>         <NA>
#> 40       FALSE       <NA>         <NA>
#> 41       FALSE       <NA>         <NA>
#> 42       FALSE       <NA>         <NA>
#> 43       FALSE       <NA>         <NA>
#> 44       FALSE       <NA>         <NA>
#> 45       FALSE       <NA>         <NA>
#> 46       FALSE       <NA>         <NA>
#> 47       FALSE       <NA>         <NA>
#> 48       FALSE       <NA>         <NA>
#> 49       FALSE       <NA>         <NA>
#> 50       FALSE       <NA>         <NA>
#> 51       FALSE       <NA>         <NA>
#> 52       FALSE       <NA>         <NA>
#> 53       FALSE       <NA>         <NA>
#> 54       FALSE       <NA>         <NA>
#> 55       FALSE       <NA>         <NA>
#> 56       FALSE       <NA>         <NA>
#> 57       FALSE       <NA>         <NA>
#> 58       FALSE       <NA>         <NA>
#> 59       FALSE       <NA>         <NA>
#> 60       FALSE       <NA>         <NA>
#> 61       FALSE       <NA>         <NA>
#> 62       FALSE       <NA>         <NA>
#> 63       FALSE       <NA>         <NA>
#> 64       FALSE       <NA>         <NA>
#> 65       FALSE       <NA>         <NA>
#> 66       FALSE       <NA>         <NA>
#> 67       FALSE       <NA>         <NA>
#> 68       FALSE       <NA>         <NA>
#> 69       FALSE       <NA>         <NA>
#> 70       FALSE       <NA>         <NA>
#> 71       FALSE       <NA>         <NA>
#> 72       FALSE       <NA>         <NA>
#> 73       FALSE       <NA>         <NA>
#> 74       FALSE       <NA>         <NA>
#> 75       FALSE       <NA>         <NA>
#> 76       FALSE       <NA>         <NA>
#> 77       FALSE       <NA>         <NA>
#> 78       FALSE       <NA>         <NA>
#> 79       FALSE       <NA>         <NA>
#> 80       FALSE       <NA>         <NA>
#> 81       FALSE       <NA>         <NA>
#> 82       FALSE       <NA>         <NA>
#> 83       FALSE       <NA>         <NA>
#> 84       FALSE       <NA>         <NA>
#> 85       FALSE       <NA>         <NA>
#> 86       FALSE       <NA>         <NA>
#> 87       FALSE       <NA>         <NA>
#> 88       FALSE       <NA>         <NA>
#> 89       FALSE       <NA>         <NA>
#> 90       FALSE       <NA>         <NA>
#> 91       FALSE       <NA>         <NA>
#> 92       FALSE       <NA>         <NA>
#> 93       FALSE       <NA>         <NA>
#> 94       FALSE       <NA>         <NA>
#> 95       FALSE       <NA>         <NA>
#> 96       FALSE       <NA>         <NA>
#> 97       FALSE       <NA>         <NA>
#> 98       FALSE       <NA>         <NA>
#> 99       FALSE       <NA>         <NA>
#> 100      FALSE       <NA>         <NA>
#> 101      FALSE       <NA>         <NA>
#> 102      FALSE       <NA>         <NA>
#> 103      FALSE       <NA>         <NA>
#> 104      FALSE       <NA>         <NA>
#> 105      FALSE       <NA>         <NA>
#> 106      FALSE       <NA>         <NA>
#> 107      FALSE       <NA>         <NA>
#> 108      FALSE       <NA>         <NA>
#> 109      FALSE       <NA>         <NA>
#> 110      FALSE       <NA>         <NA>
#> 111      FALSE       <NA>         <NA>
#> 112      FALSE       <NA>         <NA>
#> 113      FALSE       <NA>         <NA>
#> 114      FALSE       <NA>         <NA>
#> 115      FALSE       <NA>         <NA>
#> 116      FALSE       <NA>         <NA>
#> 117      FALSE       <NA>         <NA>
#> 118      FALSE       <NA>         <NA>
#> 119      FALSE       <NA>         <NA>
#> 120      FALSE       <NA>         <NA>
#> 121      FALSE       <NA>         <NA>
#> 122      FALSE       <NA>         <NA>
#> 123      FALSE       <NA>         <NA>
#> 124      FALSE       <NA>         <NA>
#> 125      FALSE       <NA>         <NA>
#> 126      FALSE       <NA>         <NA>
#> 127      FALSE       <NA>         <NA>
#> 128      FALSE       <NA>         <NA>
#> 129      FALSE       <NA>         <NA>
#> 130      FALSE       <NA>         <NA>
#> 131      FALSE       <NA>         <NA>
#> 132      FALSE       <NA>         <NA>
#> 133      FALSE       <NA>         <NA>
#> 134      FALSE       <NA>         <NA>
#> 135      FALSE       <NA>         <NA>
#> 136      FALSE       <NA>         <NA>
#> 137      FALSE       <NA>         <NA>
#> 138      FALSE       <NA>         <NA>
#> 139      FALSE       <NA>         <NA>
#> 140      FALSE       <NA>         <NA>
#> 141      FALSE       <NA>         <NA>
#> 142      FALSE       <NA>         <NA>
#> 143      FALSE       <NA>         <NA>
#> 144      FALSE       <NA>         <NA>
#> 145      FALSE       <NA>         <NA>
#> 146      FALSE       <NA>         <NA>
#> 147      FALSE       <NA>         <NA>
#> 148      FALSE       <NA>         <NA>
#> 149      FALSE       <NA>         <NA>
#> 150      FALSE       <NA>         <NA>
#> 
#> $line_grouping
#>   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
#>  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
#>  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
#>  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
#>  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
#>  [91]  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
#> [109] 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
#> [127] 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
#> [145] 145 146 147 148 149 150 151
#> 
#> $ref_footnotes
#> list()
#> 
#> $main_title
#> [1] ""
#> 
#> $subtitles
#> character(0)
#> 
#> $page_titles
#> character(0)
#> 
#> $main_footer
#> [1] ""
#> 
#> $prov_footer
#> character(0)
#> 
#> $header_section_div
#> [1] NA
#> 
#> $horizontal_sep
#> [1] "—"
#> 
#> $col_gap
#> [1] 3
#> 
#> $listing_keycols
#> NULL
#> 
#> $table_inset
#> [1] 0
#> 
#> $has_topleft
#> [1] FALSE
#> 
#> $indent_size
#> [1] 2
#> 
#> $col_widths
#>         rnms Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
#>            3           12           11           12           11           10 
#> 
#> $fontspec
#> $family
#> [1] "Courier"
#> 
#> $size
#> [1] 8
#> 
#> $lineheight
#> [1] 1
#> 
#> attr(,"class")
#> [1] "font_spec" "list"     
#> 
#> $num_rep_cols
#> [1] 0
#> 
#> $ref_fnote_df
#> [1] row_path  col_path  row       col       symbol    ref_index msg      
#> [8] nlines   
#> <0 rows> (or 0-length row.names)
#> 
#> $col_info
#>   label name abs_rownumber path pos_in_siblings n_siblings self_extent
#> 1     1    1             1   NA               0          0          12
#> 2     2    2             2   NA               0          0          11
#> 3     3    3             3   NA               0          0          12
#> 4     4    4             4   NA               0          0          11
#> 5     5    5             5   NA               0          0          10
#>   par_extent reprint_inds node_class indent nrowrefs ncellrefs nreflines
#> 1          0                   stuff      0        0         0         0
#> 2          0                   stuff      0        0         0         0
#> 3          0                   stuff      0        0         0         0
#> 4          0                   stuff      0        0         0         0
#> 5          0                   stuff      0        0         0         0
#>   force_page page_title trailing_sep ref_info_df
#> 1      FALSE       <NA>         <NA>            
#> 2      FALSE       <NA>         <NA>            
#> 3      FALSE       <NA>         <NA>            
#> 4      FALSE       <NA>         <NA>            
#> 5      FALSE       <NA>         <NA>            
#> 
#> attr(,"nrow_header")
#> [1] 1
#> attr(,"ncols")
#> [1] 5
#> attr(,"class")
#> [1] "MatrixPrintForm" "list"           
```
