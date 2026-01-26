# Format Structure Object

Function needed to create a frmt_structure object, which is a building
block of
[`body_plan()`](https://gsk-biostatistics.github.io/tfrmt/reference/body_plan.md).
This specifies the rows the format will be applied to.

## Usage

``` r
frmt_structure(group_val = ".default", label_val = ".default", ...)
```

## Arguments

- group_val:

  A string or a named list of strings which represent the value of group
  should be when the given frmt is implemented

- label_val:

  A string which represent the value of label should be when the given
  frmt is implemented

- ...:

  either a
  [`frmt()`](https://gsk-biostatistics.github.io/tfrmt/reference/frmt.md),
  [`frmt_combine()`](https://gsk-biostatistics.github.io/tfrmt/reference/frmt.md),
  or a
  [`frmt_when()`](https://gsk-biostatistics.github.io/tfrmt/reference/frmt.md)
  object. This can be named to also specify the parameter value

## Value

frmt_structure object

## Images

Here are some example outputs: ![Example comparing fmt, frmt_combine,
and
frmt_when](https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/main/images/tfrmt-frmts.jpg)

## See also

[`body_plan()`](https://gsk-biostatistics.github.io/tfrmt/reference/body_plan.md)
combines the frmt_structures to be applied to the table body, and
[`frmt()`](https://gsk-biostatistics.github.io/tfrmt/reference/frmt.md),
[`frmt_combine()`](https://gsk-biostatistics.github.io/tfrmt/reference/frmt.md),
and
[`frmt_when()`](https://gsk-biostatistics.github.io/tfrmt/reference/frmt.md)
define the format semantics.

[Link to related
article](https://gsk-biostatistics.github.io/tfrmt/articles/body_plan.html)

## Examples

``` r
sample_structure <- frmt_structure(
          group_val = c("group1"),
          label_val = ".default",
          frmt("XXX")
        )
## multiple group columns
sample_structure <- frmt_structure(
          group_val = list(grp_col1 = "group1", grp_col2 = "subgroup3"),
          label_val = ".default",
          frmt("XXX")
        )
```
