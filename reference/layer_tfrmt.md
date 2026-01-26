# Layer tfrmt objects together

Provide utility for layering tfrmt objects together. If both tfrmt's
have values, it will preferentially choose the second tfrmt by default.
This is an alternative to piping together tfrmt's

## Usage

``` r
layer_tfrmt(x, y, ..., join_body_plans = TRUE)
```

## Arguments

- x, y:

  tfrmt objects that need to be combined

- ...:

  arguments passed to layer_tfrmt_arg functions for combining different
  tfrmt elements

- join_body_plans:

  should the `body_plans` be combined, or just keep styling in y. See
  details: join_body_plans for more details.

## Value

tfrmt object

## Details

### join_body_plan

When combining two body_plans, the body plans will stack together, first
the body plan from x tfrmt then y tfrmt. This means that frmt_structures
in y will take priority over those in x.

Combining two tfrmt with large body_plans can lead to slow table
evaluation. Consider setting `join_body_plan` to `FALSE`. Only the y
`body_plan` will be preserved.

## Examples

``` r
tfrmt_1 <- tfrmt(title = "title1")

tfrmt_2 <- tfrmt(title = "title2",subtitle = "subtitle2")

layered_table_format <- layer_tfrmt(tfrmt_1, tfrmt_2)
```
