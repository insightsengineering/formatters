# Find column indices for vertical pagination

Find column indices for vertical pagination

## Usage

``` r
vert_pag_indices(
  mf,
  cpp = 40,
  colwidths = NULL,
  verbose = FALSE,
  rep_cols = 0L,
  fontspec,
  nosplitin = character(),
  round_type = obj_round_type(mf)
)
```

## Arguments

- mf:

  (`MatrixPrintForm`)  
  object to be paginated.

- cpp:

  (`numeric(1)`)  
  number of characters per page (width).

- colwidths:

  (`numeric`)  
  vector of column widths (in characters) for use in vertical
  pagination.

- verbose:

  (`flag`)  
  whether additional informative messages about the search for
  pagination breaks should be shown. Defaults to `FALSE`.

- rep_cols:

  (`numeric(1)`)  
  number of *columns* (not including row labels) to be repeated on every
  page. Defaults to 0.

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

- nosplitin:

  (`character`)  
  list of names of subtables where page breaks are not allowed,
  regardless of other considerations. Defaults to none.

- round_type:

  (`string`)  
  .  
  The type of rounding to perform. Allowed values: (`"iec"`, `"iec_mod"`
  or `"sas"`)  
  iec, the default, and iec_mod performs rounding compliant with IEC
  60559 (see notes in
  [`round_fmt()`](https://insightsengineering.github.io/formatters/reference/round_fmt.md)),
  while sas performs nearest-value rounding consistent with rounding
  within SAS.  
  In addition, the rounding of a negative number that rounds to zero
  will be presented as 0 (with the appropriate number of trailing zeros)
  for both `sas` and `iec_mod`, while for `iec`, it will be presented as
  -0 (with the appropriate number of trailing zeros).

## Value

A `list` partitioning the vector of column indices into subsets for 1 or
more horizontally paginated pages.

## Examples

``` r
mf <- basic_matrix_form(df = mtcars)
colpaginds <- vert_pag_indices(mf, fontspec = font_spec())
lapply(colpaginds, function(j) mtcars[, j, drop = FALSE])
#> [[1]]
#>                      mpg cyl  disp
#> Mazda RX4           21.0   6 160.0
#> Mazda RX4 Wag       21.0   6 160.0
#> Datsun 710          22.8   4 108.0
#> Hornet 4 Drive      21.4   6 258.0
#> Hornet Sportabout   18.7   8 360.0
#> Valiant             18.1   6 225.0
#> Duster 360          14.3   8 360.0
#> Merc 240D           24.4   4 146.7
#> Merc 230            22.8   4 140.8
#> Merc 280            19.2   6 167.6
#> Merc 280C           17.8   6 167.6
#> Merc 450SE          16.4   8 275.8
#> Merc 450SL          17.3   8 275.8
#> Merc 450SLC         15.2   8 275.8
#> Cadillac Fleetwood  10.4   8 472.0
#> Lincoln Continental 10.4   8 460.0
#> Chrysler Imperial   14.7   8 440.0
#> Fiat 128            32.4   4  78.7
#> Honda Civic         30.4   4  75.7
#> Toyota Corolla      33.9   4  71.1
#> Toyota Corona       21.5   4 120.1
#> Dodge Challenger    15.5   8 318.0
#> AMC Javelin         15.2   8 304.0
#> Camaro Z28          13.3   8 350.0
#> Pontiac Firebird    19.2   8 400.0
#> Fiat X1-9           27.3   4  79.0
#> Porsche 914-2       26.0   4 120.3
#> Lotus Europa        30.4   4  95.1
#> Ford Pantera L      15.8   8 351.0
#> Ferrari Dino        19.7   6 145.0
#> Maserati Bora       15.0   8 301.0
#> Volvo 142E          21.4   4 121.0
#> 
#> [[2]]
#>                      hp drat    wt
#> Mazda RX4           110 3.90 2.620
#> Mazda RX4 Wag       110 3.90 2.875
#> Datsun 710           93 3.85 2.320
#> Hornet 4 Drive      110 3.08 3.215
#> Hornet Sportabout   175 3.15 3.440
#> Valiant             105 2.76 3.460
#> Duster 360          245 3.21 3.570
#> Merc 240D            62 3.69 3.190
#> Merc 230             95 3.92 3.150
#> Merc 280            123 3.92 3.440
#> Merc 280C           123 3.92 3.440
#> Merc 450SE          180 3.07 4.070
#> Merc 450SL          180 3.07 3.730
#> Merc 450SLC         180 3.07 3.780
#> Cadillac Fleetwood  205 2.93 5.250
#> Lincoln Continental 215 3.00 5.424
#> Chrysler Imperial   230 3.23 5.345
#> Fiat 128             66 4.08 2.200
#> Honda Civic          52 4.93 1.615
#> Toyota Corolla       65 4.22 1.835
#> Toyota Corona        97 3.70 2.465
#> Dodge Challenger    150 2.76 3.520
#> AMC Javelin         150 3.15 3.435
#> Camaro Z28          245 3.73 3.840
#> Pontiac Firebird    175 3.08 3.845
#> Fiat X1-9            66 4.08 1.935
#> Porsche 914-2        91 4.43 2.140
#> Lotus Europa        113 3.77 1.513
#> Ford Pantera L      264 4.22 3.170
#> Ferrari Dino        175 3.62 2.770
#> Maserati Bora       335 3.54 3.570
#> Volvo 142E          109 4.11 2.780
#> 
#> [[3]]
#>                      qsec vs am
#> Mazda RX4           16.46  0  1
#> Mazda RX4 Wag       17.02  0  1
#> Datsun 710          18.61  1  1
#> Hornet 4 Drive      19.44  1  0
#> Hornet Sportabout   17.02  0  0
#> Valiant             20.22  1  0
#> Duster 360          15.84  0  0
#> Merc 240D           20.00  1  0
#> Merc 230            22.90  1  0
#> Merc 280            18.30  1  0
#> Merc 280C           18.90  1  0
#> Merc 450SE          17.40  0  0
#> Merc 450SL          17.60  0  0
#> Merc 450SLC         18.00  0  0
#> Cadillac Fleetwood  17.98  0  0
#> Lincoln Continental 17.82  0  0
#> Chrysler Imperial   17.42  0  0
#> Fiat 128            19.47  1  1
#> Honda Civic         18.52  1  1
#> Toyota Corolla      19.90  1  1
#> Toyota Corona       20.01  1  0
#> Dodge Challenger    16.87  0  0
#> AMC Javelin         17.30  0  0
#> Camaro Z28          15.41  0  0
#> Pontiac Firebird    17.05  0  0
#> Fiat X1-9           18.90  1  1
#> Porsche 914-2       16.70  0  1
#> Lotus Europa        16.90  1  1
#> Ford Pantera L      14.50  0  1
#> Ferrari Dino        15.50  0  1
#> Maserati Bora       14.60  0  1
#> Volvo 142E          18.60  1  1
#> 
#> [[4]]
#>                     gear carb
#> Mazda RX4              4    4
#> Mazda RX4 Wag          4    4
#> Datsun 710             4    1
#> Hornet 4 Drive         3    1
#> Hornet Sportabout      3    2
#> Valiant                3    1
#> Duster 360             3    4
#> Merc 240D              4    2
#> Merc 230               4    2
#> Merc 280               4    4
#> Merc 280C              4    4
#> Merc 450SE             3    3
#> Merc 450SL             3    3
#> Merc 450SLC            3    3
#> Cadillac Fleetwood     3    4
#> Lincoln Continental    3    4
#> Chrysler Imperial      3    4
#> Fiat 128               4    1
#> Honda Civic            4    2
#> Toyota Corolla         4    1
#> Toyota Corona          3    1
#> Dodge Challenger       3    2
#> AMC Javelin            3    2
#> Camaro Z28             3    4
#> Pontiac Firebird       3    2
#> Fiat X1-9              4    1
#> Porsche 914-2          5    2
#> Lotus Europa           5    2
#> Ford Pantera L         5    4
#> Ferrari Dino           5    6
#> Maserati Bora          5    8
#> Volvo 142E             4    2
#> 
```
