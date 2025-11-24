# Column Style Plan

Define how the columns of the table body should be aligned, whether
left, right or on a specific character(s).

## Usage

``` r
col_style_plan(...)
```

## Arguments

- ...:

  series of col_style_structure objects

## Value

col_style_plan object

## See also

[`col_style_structure()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/theme_element.md)
for more information on how to specify how to and which columns to
align.

[Link to related
article](https://gsk-biostatistics.github.io/tfrmt/articles/col_style_plan.html)

## Examples

``` r
 plan <- col_style_plan(
    col_style_structure(col = "my_var", align = "left", width = 100),
    col_style_structure(col = vars(four), align = "right"),
    col_style_structure(col = vars(two, three), align = c(".", ",", " "))
   )


```
