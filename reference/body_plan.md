# Table Body Plan

Define the formatting of the body contents of the table through a series
of frmt_structures. Structures get applied in order from bottom up, so
the last added structure is the first applied.

## Usage

``` r
body_plan(...)
```

## Arguments

- ...:

  list of frmt_structures defining the body formatting

## Value

body_plan object

## See also

[`frmt_structure()`](https://gsk-biostatistics.github.io/tfrmt/reference/frmt_structure.md)
defines which rows the formats will be applied to, and
[`frmt()`](https://gsk-biostatistics.github.io/tfrmt/reference/frmt.md),
[`frmt_combine()`](https://gsk-biostatistics.github.io/tfrmt/reference/frmt.md),
and
[`frmt_when()`](https://gsk-biostatistics.github.io/tfrmt/reference/frmt.md)
define the format semantics.

[Link to related
article](https://gsk-biostatistics.github.io/tfrmt/articles/body_plan.html)

## Examples

``` r
  tfrmt_spec<- tfrmt(
      title = "Table Title",
      body_plan = body_plan(
        frmt_structure(
          group_val = c("group1"),
          label_val = ".default",
          frmt("XXX")
        )
      )
     )
```
