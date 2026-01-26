# Fill missing values in hierarchical variables

**\[experimental\]**

Replace `NA` values in one column conditional on the same row having a
non-NA value in a different column.

The user supplies a vector of columns from which the pairs will be
extracted with a rolling window. For example `vars <- c("A", "B", "C")`
will generate 2 pairs `("A", "B")` and `("B", "C")`. Therefore the order
of the variables matters.

In each pair the second column `B` will be filled if `A` is not missing.
One can choose the value to fill with:

- `"Any {colname}"`, in this case evaluating to `"Any B"` is the
  default.

- Any other value. For example `"Any event"` for an adverse effects
  table.

- the value of pair's first column. In this case, the value of `A`.

## Usage

``` r
prep_hierarchical_fill(
  df,
  vars,
  fill = "Any {colname}",
  fill_from_left = FALSE
)
```

## Arguments

- df:

  (data.frame)

- vars:

  (character) a vector of variables to generate pairs from.

- fill:

  (character) value to replace with. Defaults to `"Any {colname}"`, in
  which case `colname` will be replaced with the name of the column.

- fill_from_left:

  (logical) indicating whether to fill from the left (first) column in
  the pair. Defaults to `FALSE`. If `TRUE` it takes precedence over
  `fill`.

## Value

a data.frame with the same columns as the input, but in which some the
desired columns have been filled pairwise.

## Examples

``` r
df <- data.frame(
  x = c(1, 2, NA),
  y = c("a", NA, "b"),
  z = rep(NA, 3)
)

prep_hierarchical_fill(
  df,
  vars = c("x", "y")
)
#>    x     y  z
#> 1  1     a NA
#> 2  2 Any y NA
#> 3 NA     b NA

prep_hierarchical_fill(
  df,
  vars = c("x", "y"),
  fill = "foo"
)
#>    x   y  z
#> 1  1   a NA
#> 2  2 foo NA
#> 3 NA   b NA

prep_hierarchical_fill(
  df,
  vars = c("x", "y", "z"),
  fill_from_left = TRUE
)
#>    x y z
#> 1  1 a a
#> 2  2 2 2
#> 3 NA b b
```
