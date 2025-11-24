# General title and footer accessors

General title and footer accessors

## Usage

``` r
main_title(obj)

# S4 method for class 'MatrixPrintForm'
main_title(obj)

main_title(obj) <- value

# S4 method for class 'MatrixPrintForm'
main_title(obj) <- value

subtitles(obj)

# S4 method for class 'MatrixPrintForm'
subtitles(obj)

subtitles(obj) <- value

# S4 method for class 'MatrixPrintForm'
subtitles(obj) <- value

page_titles(obj)

# S4 method for class 'MatrixPrintForm'
page_titles(obj)

# S4 method for class 'ANY'
page_titles(obj)

page_titles(obj) <- value

# S4 method for class 'MatrixPrintForm'
page_titles(obj) <- value

main_footer(obj)

# S4 method for class 'MatrixPrintForm'
main_footer(obj)

main_footer(obj) <- value

# S4 method for class 'MatrixPrintForm'
main_footer(obj) <- value

prov_footer(obj)

# S4 method for class 'MatrixPrintForm'
prov_footer(obj)

prov_footer(obj) <- value

# S4 method for class 'MatrixPrintForm'
prov_footer(obj) <- value

all_footers(obj)

all_titles(obj)
```

## Arguments

- obj:

  (`ANY`)  
  object to extract information from.

- value:

  character. New value.

## Value

A character scalar (`main_title`), character vector (`main_footer`), or
vector of length zero or more (`subtitles`, `page_titles`,
`prov_footer`) containing the relevant title/footer contents.
