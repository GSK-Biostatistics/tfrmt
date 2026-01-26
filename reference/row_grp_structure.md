# Row Group Structure Object

Function needed to create a row_grp_structure object, which is a
building block of
[`row_grp_plan()`](https://gsk-biostatistics.github.io/tfrmt/reference/row_grp_plan.md)

## Usage

``` r
row_grp_structure(group_val = ".default", element_block)
```

## Arguments

- group_val:

  A string or a named list of strings which represent the value of group
  should be when the given frmt is implemented

- element_block:

  element_block() object to define the block styling

## Value

row_grp_structure object

## See also

[`row_grp_plan()`](https://gsk-biostatistics.github.io/tfrmt/reference/row_grp_plan.md)
for more details on how to group row group structures,
[`element_block()`](https://gsk-biostatistics.github.io/tfrmt/reference/element_block.md)
for more details on how to specify spacing between each group.

[Link to related
article](https://gsk-biostatistics.github.io/tfrmt/articles/row_grp_plan.html)

## Examples

``` r
## single grouping variable example
row_grp_structure(group_val = c("A","C"), element_block(post_space = "---"))
#> $group_val
#> [1] "A" "C"
#> 
#> $block_to_apply
#> $post_space
#> [1] "---"
#> 
#> $fill
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "element_block" "element"      
#> 
#> attr(,"class")
#> [1] "row_grp_structure" "frmt_table"       

## example with multiple grouping variables
row_grp_structure(group_val = list(grp1 = "A", grp2 = "b"), element_block(post_space = " "))
#> $group_val
#> $group_val$grp1
#> [1] "A"
#> 
#> $group_val$grp2
#> [1] "b"
#> 
#> 
#> $block_to_apply
#> $post_space
#> [1] " "
#> 
#> $fill
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "element_block" "element"      
#> 
#> attr(,"class")
#> [1] "row_grp_structure" "frmt_table"       
```
