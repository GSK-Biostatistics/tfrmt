# Set custom parameter-level significant digits rounding

Set custom parameter-level significant digits rounding

## Usage

``` r
param_set(...)
```

## Arguments

- ...:

  Series of name-value pairs, optionally formatted using
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  syntax (note `glue` syntax is required for combined parameters).The
  name represents the parameter and the value represents the number of
  places to round the parameter to. For combined parameters (e.g.,
  `"{min}, {max}"`), value should be a vector of the same length (e.g.,
  c(1,1)).

## Value

list of default parameter-level significant digits rounding

## Details

Type `param_set()` in console to view package defaults. Use of the
function will add to the defaults and/or override included defaults of
the same name. For values that are integers, use `NA` so no decimal
places will be added.

## Examples

``` r
# View included defaults
param_set()
#> $min
#> [1] 1
#> 
#> $max
#> [1] 1
#> 
#> $median
#> [1] 1
#> 
#> $`{mean} ({sd})`
#> [1] 1 2
#> 
#> $n
#> [1] NA
#> 

# Update the defaults
param_set("{mean} ({sd})" = c(2,3), "pct" = 1)
#> $min
#> [1] 1
#> 
#> $max
#> [1] 1
#> 
#> $median
#> [1] 1
#> 
#> $n
#> [1] NA
#> 
#> $`{mean} ({sd})`
#> [1] 2 3
#> 
#> $pct
#> [1] 1
#> 

# Separate mean and SD to different lines
param_set("mean" = 2, "sd" = 3)
#> $min
#> [1] 1
#> 
#> $max
#> [1] 1
#> 
#> $median
#> [1] 1
#> 
#> $n
#> [1] NA
#> 
#> $mean
#> [1] 2
#> 
#> $sd
#> [1] 3
#> 

# Add formatting using the glue syntax
param_set("{pct} %" = 1)
#> $min
#> [1] 1
#> 
#> $max
#> [1] 1
#> 
#> $median
#> [1] 1
#> 
#> $`{mean} ({sd})`
#> [1] 1 2
#> 
#> $n
#> [1] NA
#> 
#> $`{pct} %`
#> [1] 1
#> 
```
