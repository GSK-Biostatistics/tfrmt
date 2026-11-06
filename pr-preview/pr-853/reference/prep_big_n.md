# Prepare `bigN` stat variables

**\[experimental\]**

`prep_big_n()`:

- recodes the `"n"` `stat_name` into `bigN` for the desired variables,
  and

- drops all other `stat_names` for the same variables.

If your `tfrmt` contains a
[`big_n_structure()`](https://gsk-biostatistics.github.io/tfrmt/reference/big_n_structure.md)
you pass the tfrmt `column` to `prep_big_n()` via `vars`.

## Usage

``` r
prep_big_n(df, vars)
```

## Arguments

- df:

  (data.frame)

- vars:

  (character) a vector of variables to prepare `bigN` for.

## Value

a data.frame with the same columns as the input. The `stat_name` column
is modified.

## Examples

``` r
df <- data.frame(
  stat_name = c("n", "max", "min", rep(c("n", "N", "p"), times = 2)),
  context = rep(c("continuous", "hierarchical", "categorical"), each = 3),
  stat_variable = rep(c("a", "b", "c"), each = 3)
) |>
  dplyr::bind_rows(
    data.frame(
      stat_name = "n",
      context = "total_n",
      stat_variable = "d"
    )
  )

prep_big_n(
  df,
  vars = c("b", "c")
)
#>   stat_name      context stat_variable
#> 1         n   continuous             a
#> 2       max   continuous             a
#> 3       min   continuous             a
#> 4      bigN hierarchical             b
#> 5      bigN  categorical             c
#> 6      bigN      total_n             d
```
