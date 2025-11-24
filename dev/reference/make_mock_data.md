# Make mock data for display shells

Make mock data for display shells

## Usage

``` r
make_mock_data(tfrmt, .default = 1:3, n_cols = NULL)
```

## Arguments

- tfrmt:

  tfrmt object

- .default:

  Number of unique levels to create for group/label values set to
  ".default"

- n_cols:

  Number of columns in the output table (not including group/label
  variables). If not supplied it will default to using the `col_plan`
  from the `tfrmt`. If neither are available it will use 3.

## Value

tibble containing mock data

## Examples

``` r
tfrmt_spec <- tfrmt(
  label = label,
  column = column,
  param = param,
  value=value,
  body_plan = body_plan(
    frmt_structure(group_val=".default", label_val=".default", frmt("xx.x"))
    )
  )

make_mock_data(tfrmt_spec)
#> # A tibble: 9 × 3
#>   label   param   column 
#>   <chr>   <chr>   <chr>  
#> 1 label_1 param_1 column1
#> 2 label_1 param_1 column2
#> 3 label_1 param_1 column3
#> 4 label_2 param_1 column1
#> 5 label_2 param_1 column2
#> 6 label_2 param_1 column3
#> 7 label_3 param_1 column1
#> 8 label_3 param_1 column2
#> 9 label_3 param_1 column3
```
