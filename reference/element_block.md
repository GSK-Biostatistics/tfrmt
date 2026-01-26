# Element block

Element block

## Usage

``` r
element_block(post_space = c(NULL, " ", "-"), fill = TRUE)
```

## Arguments

- post_space:

  Values to show in a new line created after the group block

- fill:

  Whether to recycle the value of `post_space` to match width of the
  data. Defaults to `TRUE`

## Value

element block object

## See also

[`row_grp_plan()`](https://gsk-biostatistics.github.io/tfrmt/reference/row_grp_plan.md)
for more details on how to group row group structures,
[`row_grp_structure()`](https://gsk-biostatistics.github.io/tfrmt/reference/row_grp_structure.md)
for more details on how to specify row group structures,
[`element_row_grp_loc()`](https://gsk-biostatistics.github.io/tfrmt/reference/element_row_grp_loc.md)
for more details on how to specify whether row group titles span the
entire table or collapse.

## Examples

``` r
tfrmt_spec <- tfrmt(
  group = grp1,
  label = label,
  param = param,
  value = value,
  column = column,
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = "   "))
  ),
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt("xx"))
  )
)
```
