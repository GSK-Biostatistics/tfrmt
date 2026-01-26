# Unusual Tables

``` r
library(tfrmt)
```

## Multiple columns of Row Labels

*Note: {tfrmt} version 0.3.0 brings support for multiple row label
columns! See the Row Group Plan vignette for details*

It is not all that unusual for listings (and some tables) to have
multiple row label columns. When this happens, it is often easier to
avoid using gt’s out-of-the box stub functions/formatting. An example of
a table like this is the “Summary of Number of Subjects by Site” from
the CDISC pilot. ![Image of the Summary of Number of Subjects By Site
table from the CDISC
pilot](../reference/figures/summary_subj_by_site.png)

To make this table the values will be long with “Pooled Id” and “Site
Id” in their own columns, as if they were group or label variables. We
also will need a column for the parameters even though they are all the
same.

``` r
data <- tibble::tribble(
  ~`Pooled Id` , ~`Site Id` ,
  "701"        , "701"      ,
  "703"        , "703"      ,
  "704"        , "704"      ,
  "705"        , "705"      ,
  "708"        , "708"      ,
  "709"        , "709"      ,
  "710"        , "710"      ,
  "713"        , "713"      ,
  "716"        , "716"      ,
  "718"        , "718"      ,
  "900"        , "702"      ,
  "900"        , "706"      ,
  "900"        , "707"      ,
  "900"        , "711"      ,
  "900"        , "714"      ,
  "900"        , "715"      ,
  "900"        , "717"      ,
  "Total"      , " "
) |>
  tidyr::crossing(
    col1 = c(
      "Placebo (N=86)",
      "Xanomeline Low Dose (N=84)",
      "Xanomeline High Dose (N=84)",
      "Total (N=254)"
    ),
    col2 = factor(c("ITT", "Eff", "Com"), levels = c("ITT", "Eff", "Com"))
  ) |>
  dplyr::mutate(
    val = rpois(216, 15), # Here I am just faking the data for display purposes
    param = "val"
  )
```

Once we have the data in the standard ARD format we can make the
`tfrmt`. What makes this `tfrmt` different is we won’t include group or
label, and our two ID columns will be displayed as regular columns. This
also means that all columns of the table, including the ID columns, can
be ordered via the
[`col_plan()`](https://gsk-biostatistics.github.io/tfrmt/reference/col_plan.md).
Because the
[`col_plan()`](https://gsk-biostatistics.github.io/tfrmt/reference/col_plan.md)
follows the conventions of
[`select()`](https://dplyr.tidyverse.org/reference/select.html) we can’t
specify the order of the highest level spanning columns and the lower
level columns. But, `tfrmt` respects the order things are put in, which
is why we used a factor for the populations.

``` r
tfrmt(
  param = "param",
  value = "val",
  column = vars(col1, col2),
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt("XX"))
  ),
  row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc("column")),
  col_plan = col_plan(
    `Pooled Id`,
    `Site Id`,
    contains("Placebo"),
    contains("High Dose"),
    contains("Low Dose"),
    everything()
  )
) |>
  print_to_gt(data)
```

[TABLE]
