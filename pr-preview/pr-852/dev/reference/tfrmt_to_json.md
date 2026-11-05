# Print to JSON

Print to JSON

## Usage

``` r
tfrmt_to_json(tfrmt, path = NULL)
```

## Arguments

- tfrmt:

  tfrmt to print

- path:

  file path to save JSON to. If not provided the JSON will just print to
  the console

## Value

JSON

## Examples

``` r
tfrmt(
  label = label,
  column = column,
  param = param,
  value=value) |>
  tfrmt_to_json()
#> {
#>   "group": [],
#>   "label": ["label"],
#>   "param": ["param"],
#>   "value": ["value"],
#>   "column": ["column"]
#> } 
```
