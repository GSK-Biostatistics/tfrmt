# Combine variables

**\[experimental\]**

A wrapper around
[`tidyr::unite()`](https://tidyr.tidyverse.org/reference/unite.html)
which pastes several columns into one. In addition it checks the output
is identical to
[`dplyr::coalesce()`](https://dplyr.tidyverse.org/reference/coalesce.html).
If not identical, the input data.frame is returned unchanged. Useful for
uniting sparsely populated columns, for example when processing an ard
that was created with
[`cards::ard_stack()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_stack.html)
then shuffled with `[shuffle_card()]`.

If the data is the result of a hierarchical ard stack (with
[`cards::ard_stack_hierarchical()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_stack_hierarchical.html)
or
[`cards::ard_stack_hierarchical_count()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_stack_hierarchical.html)),
the input is returned unchanged. This is assessed from the information
in the `context` column which needs to be present. If the input data
does not have a `context` column, the input will be returned unmodified.

## Usage

``` r
prep_combine_vars(df, vars, remove = TRUE)
```

## Arguments

- df:

  (data.frame)

- vars:

  (character) a vector of variables to unite. If a single variable is
  supplied, the input is returned unchanged.

- remove:

  If `TRUE`, remove input columns from output data frame.

## Value

a data.frame with an additional column, called `variable_level` or the
input unchanged.

## Examples

``` r
df <- data.frame(
  a = 1:6,
  context = rep("categorical", 6),
  b = c("a", rep(NA, 5)),
  c = c(NA, "b", rep(NA, 4)),
  d = c(NA, NA, "c", rep(NA, 3)),
  e = c(NA, NA, NA, "d", rep(NA, 2)),
  f = c(NA, NA, NA, NA, "e", NA),
  g = c(rep(NA, 5), "f")
)

prep_combine_vars(
  df,
  vars = c("b", "c", "d", "e", "f", "g")
)
#>   a     context variable_level
#> 1 1 categorical              a
#> 2 2 categorical              b
#> 3 3 categorical              c
#> 4 4 categorical              d
#> 5 5 categorical              e
#> 6 6 categorical              f
```
