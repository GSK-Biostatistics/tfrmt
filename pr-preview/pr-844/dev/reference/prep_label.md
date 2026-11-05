# Prepare label

**\[experimental\]**

Adds a `label` column which is a combination of `stat_label` (for
continuous variables) and `variable_level` (for categorical ones) if
these 2 columns are present in the input data frame.

## Usage

``` r
prep_label(df)
```

## Arguments

- df:

  (data.frame)

## Value

a data.frame with a `label` column (if the input has the required
columns) or the input unchanged.

## Examples

``` r
df <- data.frame(
  variable_level = c("d", "e", "f"),
  stat_label = c("a", "b", "c"),
  stat_name = c("n", "N", "n"),
  context = c("categorical", "continuous", "hierarchical")
)

prep_label(df)
#>   variable_level stat_label stat_name      context label
#> 1              d          a         n  categorical     d
#> 2              e          b         N   continuous     b
#> 3              f          c         n hierarchical     f
```
