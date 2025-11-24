# Element Row Group Location

Element Row Group Location

## Usage

``` r
element_row_grp_loc(
  location = c("indented", "spanning", "column", "noprint", "gtdefault"),
  indent = "  "
)
```

## Arguments

- location:

  Location of the row group labels. Specifying 'indented' combines all
  group and label variables into a single column with each sub-group
  indented under its parent. 'spanning' and 'column' retain the highest
  level group variable in its own column and combine all remaining group
  and label variables into a single column with sub-groups indented. The
  highest level group column will either be printed as a spanning header
  or in its own column in the gt. The 'noprint' option allows the user
  to suppress group values from being printed. Finally, the 'gtdefault'
  option allows users to use the 'gt' defaults for styling multiple
  group columns.

- indent:

  A string of the number of spaces you want to indent

## Value

element_row_grp_loc object

## Images

Here are some example outputs:

![Examples showing the difference between the row group
locations](https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/main/images/tfrmt-row_group_plan-cropped.jpg)

## See also

[`row_grp_plan()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/row_grp_plan.md)
for more details on how to group row group structures,
[`row_grp_structure()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/row_grp_structure.md)
for more details on how to specify row group structures,
[`element_block()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/element_block.md)
for more details on how to specify spacing between each group.

[Link to related
article](https://gsk-biostatistics.github.io/tfrmt/articles/row_grp_plan.html)

## Examples

``` r
tfrmt_spec <- tfrmt(
  group = c(grp1, grp2),
  label = label,
  param = param,
  value = value,
  column = column,
  row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "noprint")),
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt("xx"))
  )
)
```
