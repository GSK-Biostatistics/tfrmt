# Shuffle `cards`

**\[experimental\]**

This function ingests an ARD object of class `card` and shuffles the
information to prepare for analysis. Helpful for streamlining across
multiple ARDs.

## Usage

``` r
shuffle_card(
  x,
  by = NULL,
  trim = TRUE,
  order_rows = TRUE,
  fill_overall = "Overall {colname}",
  fill_hierarchical_overall = "Any {colname}"
)
```

## Arguments

- x:

  an ARD data frame of class 'card'

- by:

  Grouping variable(s) used in calculations. Defaults to `NULL`. If
  available (i.e. if `x` comes from a stacking function),
  `attributes(x)$by` will be used instead of `by`.

- trim:

  logical representing whether or not to trim away `fmt_fun`, `error`,
  and `warning` columns

- order_rows:

  logical representing whether or not to apply
  [`cards::tidy_ard_row_order()`](https://insightsengineering.github.io/cards/latest-tag/reference/tidy_ard_order.html)
  to sort the rows

- fill_overall:

  scalar to fill missing grouping or variable levels. If a character is
  passed, then it is processed with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) where
  the colname element is available to inject into the string, e.g.
  `Overall {colname}` may resolve to `"Overall AGE"` for an `AGE`
  column. Default is `Overall {colname}`. If `NA` then no fill will
  occur.

- fill_hierarchical_overall:

  scalar to fill variable levels for overall hierarchical calculations.
  If a character is passed, then it is processed with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) where
  the colname element is available to inject into the string, e.g.
  `Any {colname}` may resolve to `"Any AESOC"` for an `AESOC` column.
  Default is `Any {colname}`. If `NA` then no fill will occur.

## Value

a tibble

## Examples

``` r
if (FALSE) { # \dontrun{
cards::bind_ard(
  cards::ard_categorical(cards::ADSL, by = "ARM", variables = "AGEGR1"),
  cards::ard_categorical(cards::ADSL, variables = "ARM")
) |>
  shuffle_card()
} # }
```
