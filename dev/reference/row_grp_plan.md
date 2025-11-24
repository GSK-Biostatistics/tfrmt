# Row Group Plan

Define the look of the table groups on the output. This function allows
you to add spaces after blocks and allows you to control how the groups
are viewed whether they span the entire table or are nested as a column.

## Usage

``` r
row_grp_plan(..., label_loc = element_row_grp_loc(location = "indented"))
```

## Arguments

- ...:

  Row group structure objects separated by commas

- label_loc:

  [`element_row_grp_loc()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/element_row_grp_loc.md)
  object specifying location

## Value

row_grp_plan object

## See also

[`row_grp_structure()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/row_grp_structure.md)
for more details on how to specify row group structures,
[`element_block()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/element_block.md)
for more details on how to specify spacing between each group,
[`element_row_grp_loc()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/element_row_grp_loc.md)
for more details on how to specify whether row group titles span the
entire table or collapse.

[Link to related
article](https://gsk-biostatistics.github.io/tfrmt/articles/row_grp_plan.html)

## Examples

``` r

  ## single grouping variable example
  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = c("A","C"), element_block(post_space = "---")),
    row_grp_structure(group_val = c("B"), element_block(post_space = " ")),
    label_loc = element_row_grp_loc(location = "column")
  )

  ## example with multiple grouping variables
  sample_grp_plan <- row_grp_plan(
     row_grp_structure(group_val = list(grp1 = "A", grp2 = "b"), element_block(post_space = " ")),
     label_loc = element_row_grp_loc(location = "spanning")
     )
```
