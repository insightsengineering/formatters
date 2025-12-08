# Default horizontal separator

The default horizontal separator character which can be displayed in the
current charset for use in rendering table-like objects.

The default horizontal separator character which can be displayed in the
current charset for use in rendering table-like objects.

## Usage

``` r
default_hsep()

set_default_hsep(hsep_char)

default_hsep()

set_default_hsep(hsep_char)
```

## Arguments

- hsep_char:

  (`string`)  
  character that will be set in the R environment options as the default
  horizontal separator. Must be a single character. Use
  `getOption("formatters_default_hsep")` to get its current value
  (`NULL` if not set).

## Value

unicode 2014 (long dash for generating solid horizontal line) if in a
locale that uses a UTF character set, otherwise an ASCII hyphen with a
once-per-session warning.

unicode 2014 (long dash for generating solid horizontal line) if in a
locale that uses a UTF character set, otherwise an ASCII hyphen with a
once-per-session warning.

## Examples

``` r
default_hsep()
#> [1] "—"
set_default_hsep("o")
default_hsep()
#> [1] "o"

default_hsep()
#> [1] "o"
set_default_hsep("o")
default_hsep()
#> [1] "o"
```
