# formatters

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering//actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering//actions/workflows/check.yaml)
[![Docs 📚](https://github.com/insightsengineering//actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io//)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/coverage.xml)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/insightsengineering/?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering//main?color=purple\&label=package%20version)](https://github.com/insightsengineering//tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/?color=red\&label=open%20issues)](https://github.com/insightsengineering//issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

# formatters

<!-- start badges -->

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/formatters)](https://www.r-pkg.org/badges/version-last-release/formatters)
[![CRAN monthly downloads](https://cranlogs.r-pkg.org/badges/formatters)](https://cranlogs.r-pkg.org/badges/formatters)
[![Code Coverage](https://raw.githubusercontent.com/insightsengineering/formatters/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering/formatters/_xml_coverage_reports/data/main/coverage.xml)

<!-- end badges -->

The `formatters` package provides two core pieces of functionality, both
relatead to ASCII rendering:

1. `format_value` provides the ability to format single- and
   multi-valued elements into ASCII display-ready strings
1. the `matrix_form` framework provides generics for implementing ASCII
   rendering support for display tables

Both of these featuresets are used in the `rtables` package.

## Motivation

The core motivation for `formatters` is the rendering of reporting
tables into ASCII. In this context a ‘value’ is the raw content that to
appear in a single table cell. Most commonly this is a numeric vector of
length 1, 2 or – occasionally – 3.

## Format labels

`formatters` ships with a large number of pre-defined formats
appropriate for rendering values into ASCII strings. These existing
formats are specified by their labels. We can see the list of these by
calling the `list_valid_format_labels` function:

```R
list_valid_format_labels()

$`1d`
 [1] "xx"                 "xx."                "xx.x"
 [4] "xx.xx"              "xx.xxx"             "xx.xxxx"
 [7] "xx%"                "xx.%"               "xx.x%"
[10] "xx.xx%"             "xx.xxx%"            "(N=xx)"
[13] ">999.9"             ">999.99"            "x.xxxx | (<0.0001)"

$`2d`
 [1] "xx / xx"            "xx. / xx."          "xx.x / xx.x"
 [4] "xx.xx / xx.xx"      "xx.xxx / xx.xxx"    "xx (xx%)"
 [7] "xx (xx.%)"          "xx (xx.x%)"         "xx (xx.xx%)"
[10] "xx. (xx.%)"         "xx.x (xx.x%)"       "xx.xx (xx.xx%)"
[13] "(xx, xx)"           "(xx., xx.)"         "(xx.x, xx.x)"
[16] "(xx.xx, xx.xx)"     "(xx.xxx, xx.xxx)"   "(xx.xxxx, xx.xxxx)"
[19] "xx - xx"            "xx.x - xx.x"        "xx.xx - xx.xx"
[22] "xx (xx)"            "xx. (xx.)"          "xx.x (xx.x)"
[25] "xx.xx (xx.xx)"      "xx (xx.)"           "xx (xx.x)"
[28] "xx (xx.xx)"         "xx.x, xx.x"         "xx.x to xx.x"

$`3d`
[1] "xx.xx (xx.xx - xx.xx)"

attr(,"info")
[1] "xx does not modify the element, and xx. rounds a number to 0 digits"
```

Each of these labels desceribes how the incoming (possibly
multi-element) raw value will be formatted. `xx` indicates that an
element of the value will be printed as is, with no modification. `xx.`
indicates that a numeric value element will be rounded to 0 decimal
places, `xx.x` indicates rounding to 1 decimal place, etc.

## Formatting values

Values are formatted via calls to `format_value`, like so:

```R
format_value(5.1235, format = "xx.xx")

[1] "5.12"

format_value(c(1.2355, 2.6789), "(xx.xx, xx.xx)")

[1] "(1.24, 2.68)"
```

## Table Rendering Framework

__Advanced Usage Only__ These features are supported, and in fact are
used in `rtables` and the experimental `rlistings`. That said, the API
is currently very low-level and tailored to what `rtables` and
`rlistings` need. How useful this is to other table frameworks may vary.

The second major piece of functionality in `formatters` is the ability
to render tables into ASCII (and thus directly to the terminal) based on
a so-called `MatrixPrintForm` representation of the table.

To hook up `rtables`-style ASCII display for your tables, it suffices to
export a method for the exported `matrix_form` generic `formatters`
provides. This method must return a `MatrixPrintForm` object
representing your table.

We can build a baby example method for `data.frames` to illustrate this
process:

```R
## pagdfrow supports a large number of pieces of information regarding
## siblings and what information should be repeated after a pagination.
## we ignore all that here and just give the absolutely crucial info:
## nm (name), lab (label), rnum (absolute row position), pth ("path"),
## extent (how many lines it takes up), rclass ("class of row")
fake_pagdf_row <- function(i, rnms) {
    nm <- rnms[i]
    pagdfrow(nm = nm, lab = nm, rnum = i, pth = nm, extent = 1L,
             rclass = "NA")
}

matrix_form.data.frame <- function(df) {
    fmts <- lapply(df, function(x) if(is.null(obj_format(x))) "xx" else obj_format(x))

    bodystrs <- mapply(function(x, fmt) {
        sapply(x, format_value, format = fmt)
    }, x = df, fmt = fmts)

    rnms <- row.names(df)
    if(is.null(rnms))
        rnms <- as.character(seq_len(NROW(df)))

    cnms <- names(df)

    strings <- rbind(c("", cnms),
                     cbind(rnms, bodystrs))

    fnr <- nrow(strings)
    fnc <- ncol(strings)

    ## center alignment for column labels, left alignment for everything else
    aligns <- rbind("center",
                    matrix("left", nrow = NROW(df), ncol = fnc))


    ## build up fake pagination df,
    rowdf <- basic_pagdf(row.names(df))
    matrix_print_form(strings = strings,
                      aligns = aligns,
                      spans = matrix(1, nrow = fnr, ncol = fnc),
                      formats = NULL,
                      row_info = rowdf,
                      has_topleft = FALSE,
                      nlines_header = 1,
                      nrow_header = 1)
}


cat(toString(matrix_form.data.frame(mtcars)))

                      mpg    cyl   disp    hp    drat    wt     qsec    vs   am   gear   carb
—————————————————————————————————————————————————————————————————————————————————————————————
Mazda RX4             21     6     160     110   3.9    2.62    16.46   0    1    4      4
Mazda RX4 Wag         21     6     160     110   3.9    2.875   17.02   0    1    4      4
Datsun 710            22.8   4     108     93    3.85   2.32    18.61   1    1    4      1
Hornet 4 Drive        21.4   6     258     110   3.08   3.215   19.44   1    0    3      1
Hornet Sportabout     18.7   8     360     175   3.15   3.44    17.02   0    0    3      2
Valiant               18.1   6     225     105   2.76   3.46    20.22   1    0    3      1
Duster 360            14.3   8     360     245   3.21   3.57    15.84   0    0    3      4
Merc 240D             24.4   4     146.7   62    3.69   3.19    20      1    0    4      2
Merc 230              22.8   4     140.8   95    3.92   3.15    22.9    1    0    4      2
Merc 280              19.2   6     167.6   123   3.92   3.44    18.3    1    0    4      4
Merc 280C             17.8   6     167.6   123   3.92   3.44    18.9    1    0    4      4
Merc 450SE            16.4   8     275.8   180   3.07   4.07    17.4    0    0    3      3
Merc 450SL            17.3   8     275.8   180   3.07   3.73    17.6    0    0    3      3
Merc 450SLC           15.2   8     275.8   180   3.07   3.78    18      0    0    3      3
Cadillac Fleetwood    10.4   8     472     205   2.93   5.25    17.98   0    0    3      4
Lincoln Continental   10.4   8     460     215   3      5.424   17.82   0    0    3      4
Chrysler Imperial     14.7   8     440     230   3.23   5.345   17.42   0    0    3      4
Fiat 128              32.4   4     78.7    66    4.08   2.2     19.47   1    1    4      1
Honda Civic           30.4   4     75.7    52    4.93   1.615   18.52   1    1    4      2
Toyota Corolla        33.9   4     71.1    65    4.22   1.835   19.9    1    1    4      1
Toyota Corona         21.5   4     120.1   97    3.7    2.465   20.01   1    0    3      1
Dodge Challenger      15.5   8     318     150   2.76   3.52    16.87   0    0    3      2
AMC Javelin           15.2   8     304     150   3.15   3.435   17.3    0    0    3      2
Camaro Z28            13.3   8     350     245   3.73   3.84    15.41   0    0    3      4
Pontiac Firebird      19.2   8     400     175   3.08   3.845   17.05   0    0    3      2
Fiat X1-9             27.3   4     79      66    4.08   1.935   18.9    1    1    4      1
Porsche 914-2         26     4     120.3   91    4.43   2.14    16.7    0    1    5      2
Lotus Europa          30.4   4     95.1    113   3.77   1.513   16.9    1    1    5      2
Ford Pantera L        15.8   8     351     264   4.22   3.17    14.5    0    1    5      4
Ferrari Dino          19.7   6     145     175   3.62   2.77    15.5    0    1    5      6
Maserati Bora         15     8     301     335   3.54   3.57    14.6    0    1    5      8
Volvo 142E            21.4   4     121     109   4.11   2.78    18.6    1    1    4      2
```
