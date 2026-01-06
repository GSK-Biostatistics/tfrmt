# Remap group values in a tfrmt

Remap group values in a tfrmt

## Usage

``` r
update_group(tfrmt, ...)
```

## Arguments

- tfrmt:

  a `tfrmt`

- ...:

  Use new_name = old_name to rename selected variables

## Value

A `tfrmt` with the `group` variables updated in all places

tfrmt object with updated groups#'

## Examples

``` r
tfrmt_spec <- tfrmt(
    group = c(group1, group2),
    body_plan  = body_plan(
      frmt_structure(
         group_val = list(group2 = "value"),
         label_val = ".default",
         frmt("XXX")
         ),
     frmt_structure(
         group_val = list(group1 = "value", group2 = "value"),
         label_val = ".default",
         frmt("XXX")
       )
    ))

tfrmt_spec %>%
  update_group(New_Group = group1)
#> $group
#> <list_of<quosure>>
#> 
#> [[1]]
#> <quosure>
#> expr: ^New_Group
#> env:  0x558d943f8d78
#> 
#> [[2]]
#> <quosure>
#> expr: ^group2
#> env:  0x558d943f7c68
#> 
#> 
#> $label
#> <quosure>
#> expr: ^
#> env:  empty
#> 
#> $param
#> <quosure>
#> expr: ^
#> env:  empty
#> 
#> $value
#> <quosure>
#> expr: ^
#> env:  empty
#> 
#> $column
#> <list_of<quosure>>
#> 
#> named list()
#> 
#> $body_plan
#> [[1]]
#> Format Structure
#>   Group Values: `group2` - "value"
#>   Label Values: ".default"
#>   Format: < frmt | Expression: `XXX` >
#> 
#> [[2]]
#> Format Structure
#>   Group Values: `New_Group` - "value"; `group2` - "value"
#>   Label Values: ".default"
#>   Format: < frmt | Expression: `XXX` >
#> 
#> attr(,"class")
#> [1] "body_plan"  "frmt_table"
#> 
#> attr(,"class")
#> [1] "tfrmt"
```
