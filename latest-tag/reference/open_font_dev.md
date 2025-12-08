# Activate font state

Activate font state

## Usage

``` r
open_font_dev(fontspec, silent = FALSE)

close_font_dev()

debug_font_dev()

undebug_font_dev()
```

## Arguments

- fontspec:

  (`font_spec`)  
  a font_spec object specifying the font information to use for
  calculating string widths and heights, as returned by
  [`font_spec()`](https://insightsengineering.github.io/formatters/reference/font_spec.md).

- silent:

  (`logical(1)`)  
  If `FALSE`, the default, a warning will be emitted if this function
  switches away from an active graphics device.

## Value

- `open_font_dev` returns a logical value indicating whether a *new* pdf
  device was opened.

- `close_font_dev`, `debug_font_dev` and `undebug_font_dev` return
  `NULL`.

In all cases the value is returned invisibly.

## Details

The font device state is an environment with four variables guaranteed
to be set:

- `open`:

  (`logical(1)`)  
  whether a device is already open with font info

- `fontspec`:

  (`font_spec`)  
  the font specification, if any, that is currently active
  ([`list()`](https://rdrr.io/r/base/list.html) if none is).

- `spacewidth`:

  (`numeric(1)`)  
  the width of the space character in the currently active font.

- `ismonospace`:

  (`logical(1)`)  
  whether the specified font is monospaced.

`open_font_dev` opens a pdf device with the specified font only if there
is not one currently open with the same font. If a new device is opened,
it caches `spacewidth` and `ismonospace` for use in `nchar_ttype`).

`close_font_dev` closes any open font state device and clears the cached
values.

`debug_font_dev` and `undebug_font_dev` activate and deactivate,
respectively, logging of where in the call stack font devices are being
opened.

## Examples

``` r
open_font_dev(font_spec("Times"))
nchar_ttype("Hiya there", font_spec("Times"))
#> [1] 17
close_font_dev()
```
