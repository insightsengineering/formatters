# Create spoof matrix form from a data frame

Useful functions for writing tests and examples, and a starting point
for more sophisticated custom `matrix_form` methods.

## Usage

``` r
basic_matrix_form(
  df,
  indent_rownames = FALSE,
  parent_path = NULL,
  ignore_rownames = FALSE,
  add_decoration = FALSE,
  fontspec = font_spec(),
  split_labels = NULL,
  data_labels = NULL,
  num_rep_cols = 0L,
  round_type = valid_round_type
)

basic_listing_mf(
  df,
  keycols = names(df)[1],
  add_decoration = TRUE,
  fontspec = font_spec(),
  round_type = valid_round_type
)
```

## Arguments

- df:

  (`data.frame`)  
  a data frame.

- indent_rownames:

  (`flag`)  
  whether row names should be indented. Being this used for testing
  purposes, it defaults to `FALSE`. If `TRUE`, it assigns label rows on
  even lines (also format is `"-"` and value strings are `""`).
  Indentation works only if split labels are used (see parameters
  `split_labels` and `data_labels`).

- parent_path:

  (`string`)  
  parent path that all rows should be "children of". Defaults to `NULL`,
  as usually this is not needed. It may be necessary to use `"root"`,
  for some specific scenarios.

- ignore_rownames:

  (`flag`)  
  whether row names should be ignored.

- add_decoration:

  (`flag`)  
  whether adds title and footer decorations should be added to the
  matrix form.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

- split_labels:

  (`string`)  
  indicates which column to use as split labels. If `NULL`, no split
  labels are used.

- data_labels:

  (`string`)  
  indicates which column to use as data labels. It is ignored if no
  `split_labels` is present and is automatically assigned to
  `"Analysis method"` when `split_labels` is present, but `data_labels`
  is `NULL`. Its direct column name is used as node name in `"DataRow"`
  pathing. See
  [`mf_rinfo()`](https://insightsengineering.github.io/formatters/reference/mpf_accessors.md)
  for more information.

- num_rep_cols:

  (`numeric(1)`)  
  Number of columns to be treated as repeating columns. Defaults to `0`
  for `basic_matrix_form` and `length(keycols)` for `basic_listing_mf`.
  Note repeating columns are separate from row labels if present.

- round_type:

  (`string`)  
  The type of rounding to perform. Allowed values: (`"iec"`, `"iec_mod"`
  or `"sas"`) See
  [`round_fmt()`](https://insightsengineering.github.io/formatters/reference/round_fmt.md)
  for details.

- keycols:

  (`character`)  
  a vector of `df` column names that are printed first and for which
  repeated values are assigned `""`. This format is characteristic of a
  listing matrix form.

## Value

A valid `MatrixPrintForm` object representing `df` that is ready for
ASCII rendering.

A valid `MatrixPrintForm` object representing `df` as a listing that is
ready for ASCII rendering.

## Details

If some of the column has a
[obj_format](https://insightsengineering.github.io/formatters/reference/lab_name.md)
assigned, it will be respected for all column values except for label
rows, if present (see parameter `split_labels`).

## Functions

- `basic_listing_mf()`: Create a `MatrixPrintForm` object from data
  frame `df` that respects the default formats for a listing object.

## Examples

``` r
mform <- basic_matrix_form(mtcars)
cat(toString(mform))
#>                       mpg    cyl   disp    hp    drat    wt     qsec    vs   am   gear   carb
#> ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#> Mazda RX4              21     6     160    110   3.9    2.62    16.46   0    1     4      4  
#> Mazda RX4 Wag          21     6     160    110   3.9    2.875   17.02   0    1     4      4  
#> Datsun 710            22.8    4     108    93    3.85   2.32    18.61   1    1     4      1  
#> Hornet 4 Drive        21.4    6     258    110   3.08   3.215   19.44   1    0     3      1  
#> Hornet Sportabout     18.7    8     360    175   3.15   3.44    17.02   0    0     3      2  
#> Valiant               18.1    6     225    105   2.76   3.46    20.22   1    0     3      1  
#> Duster 360            14.3    8     360    245   3.21   3.57    15.84   0    0     3      4  
#> Merc 240D             24.4    4    146.7   62    3.69   3.19     20     1    0     4      2  
#> Merc 230              22.8    4    140.8   95    3.92   3.15    22.9    1    0     4      2  
#> Merc 280              19.2    6    167.6   123   3.92   3.44    18.3    1    0     4      4  
#> Merc 280C             17.8    6    167.6   123   3.92   3.44    18.9    1    0     4      4  
#> Merc 450SE            16.4    8    275.8   180   3.07   4.07    17.4    0    0     3      3  
#> Merc 450SL            17.3    8    275.8   180   3.07   3.73    17.6    0    0     3      3  
#> Merc 450SLC           15.2    8    275.8   180   3.07   3.78     18     0    0     3      3  
#> Cadillac Fleetwood    10.4    8     472    205   2.93   5.25    17.98   0    0     3      4  
#> Lincoln Continental   10.4    8     460    215    3     5.424   17.82   0    0     3      4  
#> Chrysler Imperial     14.7    8     440    230   3.23   5.345   17.42   0    0     3      4  
#> Fiat 128              32.4    4    78.7    66    4.08    2.2    19.47   1    1     4      1  
#> Honda Civic           30.4    4    75.7    52    4.93   1.615   18.52   1    1     4      2  
#> Toyota Corolla        33.9    4    71.1    65    4.22   1.835   19.9    1    1     4      1  
#> Toyota Corona         21.5    4    120.1   97    3.7    2.465   20.01   1    0     3      1  
#> Dodge Challenger      15.5    8     318    150   2.76   3.52    16.87   0    0     3      2  
#> AMC Javelin           15.2    8     304    150   3.15   3.435   17.3    0    0     3      2  
#> Camaro Z28            13.3    8     350    245   3.73   3.84    15.41   0    0     3      4  
#> Pontiac Firebird      19.2    8     400    175   3.08   3.845   17.05   0    0     3      2  
#> Fiat X1-9             27.3    4     79     66    4.08   1.935   18.9    1    1     4      1  
#> Porsche 914-2          26     4    120.3   91    4.43   2.14    16.7    0    1     5      2  
#> Lotus Europa          30.4    4    95.1    113   3.77   1.513   16.9    1    1     5      2  
#> Ford Pantera L        15.8    8     351    264   4.22   3.17    14.5    0    1     5      4  
#> Ferrari Dino          19.7    6     145    175   3.62   2.77    15.5    0    1     5      6  
#> Maserati Bora          15     8     301    335   3.54   3.57    14.6    0    1     5      8  
#> Volvo 142E            21.4    4     121    109   4.11   2.78    18.6    1    1     4      2  

# Advanced test case with label rows
library(dplyr)
iris_output <- iris %>%
  group_by(Species) %>%
  summarize("all obs" = round(mean(Petal.Length), 2)) %>%
  mutate("DataRow_label" = "Mean")
mf <- basic_matrix_form(iris_output,
  indent_rownames = TRUE,
  split_labels = "Species", data_labels = "DataRow_label"
)
cat(toString(mf))
#>              all obs
#> oooooooooooooooooooo
#> setosa              
#>   Mean        1.46  
#> versicolor          
#>   Mean        4.26  
#> virginica           
#>   Mean        5.55  
mform <- basic_listing_mf(mtcars)
cat(toString(mform))
#> main title
#> sub
#> titles
#> 
#> ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#> mpg    cyl   disp    hp    drat    wt     qsec    vs   am   gear   carb
#> ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#> 10.4    8     472    205   2.93   5.25    17.98   0    0     3      4  
#>         8     460    215    3     5.424   17.82   0    0     3      4  
#> 13.3    8     350    245   3.73   3.84    15.41   0    0     3      4  
#> 14.3    8     360    245   3.21   3.57    15.84   0    0     3      4  
#> 14.7    8     440    230   3.23   5.345   17.42   0    0     3      4  
#>  15     8     301    335   3.54   3.57    14.6    0    1     5      8  
#> 15.2    8    275.8   180   3.07   3.78     18     0    0     3      3  
#>         8     304    150   3.15   3.435   17.3    0    0     3      2  
#> 15.5    8     318    150   2.76   3.52    16.87   0    0     3      2  
#> 15.8    8     351    264   4.22   3.17    14.5    0    1     5      4  
#> 16.4    8    275.8   180   3.07   4.07    17.4    0    0     3      3  
#> 17.3    8    275.8   180   3.07   3.73    17.6    0    0     3      3  
#> 17.8    6    167.6   123   3.92   3.44    18.9    1    0     4      4  
#> 18.1    6     225    105   2.76   3.46    20.22   1    0     3      1  
#> 18.7    8     360    175   3.15   3.44    17.02   0    0     3      2  
#> 19.2    6    167.6   123   3.92   3.44    18.3    1    0     4      4  
#>         8     400    175   3.08   3.845   17.05   0    0     3      2  
#> 19.7    6     145    175   3.62   2.77    15.5    0    1     5      6  
#>  21     6     160    110   3.9    2.62    16.46   0    1     4      4  
#>         6     160    110   3.9    2.875   17.02   0    1     4      4  
#> 21.4    6     258    110   3.08   3.215   19.44   1    0     3      1  
#>         4     121    109   4.11   2.78    18.6    1    1     4      2  
#> 21.5    4    120.1   97    3.7    2.465   20.01   1    0     3      1  
#> 22.8    4     108    93    3.85   2.32    18.61   1    1     4      1  
#>         4    140.8   95    3.92   3.15    22.9    1    0     4      2  
#> 24.4    4    146.7   62    3.69   3.19     20     1    0     4      2  
#>  26     4    120.3   91    4.43   2.14    16.7    0    1     5      2  
#> 27.3    4     79     66    4.08   1.935   18.9    1    1     4      1  
#> 30.4    4    75.7    52    4.93   1.615   18.52   1    1     4      2  
#>         4    95.1    113   3.77   1.513   16.9    1    1     5      2  
#> 32.4    4    78.7    66    4.08    2.2    19.47   1    1     4      1  
#> 33.9    4    71.1    65    4.22   1.835   19.9    1    1     4      1  
#> ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#> 
#> main
#>   footer
#> 
#> prov footer
```
