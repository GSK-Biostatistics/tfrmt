# Page structure

Page structure

## Usage

``` r
page_structure(group_val = NULL, label_val = NULL)
```

## Arguments

- group_val:

  string or a named list of strings which represent the value of group
  to split after. Set to ".default" if the split should occur after
  every unique value of the variable.

- label_val:

  string which represents the value of label to split after. Set to
  ".default" if the split should occur after every unique value of the
  variable.

## Value

page structure object

## Examples

``` r
# split page after every unique level of the grouping variable
 page_structure(group_val = ".default", label_val = NULL)
#> $group_val
#> [1] ".default"
#> 
#> $label_val
#> NULL
#> 
#> attr(,"class")
#> [1] "page_structure" "structure"     

 # split page after specific levels
 page_structure(group_val = "grp1", label_val = "lbl3")
#> $group_val
#> [1] "grp1"
#> 
#> $label_val
#> [1] "lbl3"
#> 
#> attr(,"class")
#> [1] "page_structure" "structure"     
```
