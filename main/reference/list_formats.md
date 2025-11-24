# List of currently supported formats and vertical alignments

We support `xx` style format labels grouped by 1d, 2d, and 3d. Currently
valid format labels cannot be added dynamically. Format functions must
be used for special cases.

## Usage

``` r
list_valid_format_labels()

list_valid_aligns()
```

## Value

- `list_valid_format_labels()` returns a nested list, with elements
  listing the supported 1d, 2d, and 3d format strings.

&nbsp;

- `list_valid_aligns()` returns a character vector of valid vertical
  alignments.

## Examples

``` r
list_valid_format_labels()
#> $`1d`
#>  [1] "xx"                 "xx."                "xx.x"              
#>  [4] "xx.xx"              "xx.xxx"             "xx.xxxx"           
#>  [7] "xx%"                "xx.%"               "xx.x%"             
#> [10] "xx.xx%"             "xx.xxx%"            "(N=xx)"            
#> [13] "N=xx"               ">999.9"             ">999.99"           
#> [16] "x.xxxx | (<0.0001)"
#> 
#> $`2d`
#>  [1] "xx / xx"            "xx. / xx."          "xx.x / xx.x"       
#>  [4] "xx.xx / xx.xx"      "xx.xxx / xx.xxx"    "N=xx (xx%)"        
#>  [7] "xx (xx%)"           "xx (xx.%)"          "xx (xx.x%)"        
#> [10] "xx (xx.xx%)"        "xx. (xx.%)"         "xx.x (xx.x%)"      
#> [13] "xx.xx (xx.xx%)"     "(xx, xx)"           "(xx., xx.)"        
#> [16] "(xx.x, xx.x)"       "(xx.xx, xx.xx)"     "(xx.xxx, xx.xxx)"  
#> [19] "(xx.xxxx, xx.xxxx)" "xx - xx"            "xx.x - xx.x"       
#> [22] "xx.xx - xx.xx"      "xx (xx)"            "xx. (xx.)"         
#> [25] "xx.x (xx.x)"        "xx.xx (xx.xx)"      "xx (xx.)"          
#> [28] "xx (xx.x)"          "xx (xx.xx)"         "xx. (xx.x)"        
#> [31] "xx.x (xx.xx)"       "xx.xx (xx.xxx)"     "xx, xx"            
#> [34] "xx.x, xx.x"         "xx.xx, xx.xx"       "xx.x to xx.x"      
#> 
#> $`3d`
#> [1] "xx. (xx. - xx.)"          "xx.x (xx.x - xx.x)"      
#> [3] "xx.xx (xx.xx - xx.xx)"    "xx.xxx (xx.xxx - xx.xxx)"
#> [5] "xx / xx (xx.%)"           "xx / xx (xx.x%)"         
#> [7] "xx / xx (xx.xx%)"        
#> 
#> attr(,"info")
#> [1] "xx does not modify the element, and xx. rounds a number to 0 digits"

list_valid_aligns()
#> [1] "left"      "right"     "center"    "decimal"   "dec_right" "dec_left" 
```
