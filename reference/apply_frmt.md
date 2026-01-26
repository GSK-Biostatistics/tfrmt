# Apply formatting

Apply formatting

## Usage

``` r
apply_frmt(frmt_def, .data, value, mock = FALSE, ...)

# S3 method for class 'frmt'
apply_frmt(frmt_def, .data, value, mock = FALSE, ...)

# S3 method for class 'frmt_combine'
apply_frmt(
  frmt_def,
  .data,
  value,
  mock = FALSE,
  param,
  column,
  label,
  group,
  ...
)

# S3 method for class 'frmt_when'
apply_frmt(frmt_def, .data, value, mock = FALSE, ...)
```

## Arguments

- frmt_def:

  formatting to be applied

- .data:

  data, but only what is getting changed

- value:

  value symbol should only be one

- mock:

  Logical value is this is for a mock or not. By default `FALSE`

- ...:

  additional arguments for methods

- param:

  param column as a quosure

- column:

  column columns as a list of quosures

- label:

  label column as a quosure

- group:

  group column as a list of quosures

## Value

formatted dataset

## Examples

``` r
library(tibble)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
# Set up data
df <- tibble(x = c(20.12,34.54,12.34))

apply_frmt(
 frmt_def = frmt("XX.X"),
 .data=df,
 value=quo(x))
#> # A tibble: 3 × 1
#>   x    
#>   <chr>
#> 1 20.1 
#> 2 34.5 
#> 3 12.3 
```
