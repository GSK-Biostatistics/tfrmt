# Page Plan

``` r
library(tfrmt)
library(dplyr)
library(gt)
library(tidyr)
library(purrr)
```

The purpose of the page plan is to provide the option to split tables
onto multiple pages. Currently, the page plan splits tables horizontally
(i.e. row-wise), but in the future it is also planned to allow vertical
(i.e. column-wise) splitting.

## Page Plan rules

The rules for how to split the table fall into 2 categories:

1.  **Values-driven**:

This feature involves splitting by the values of `group` and/or `label`
variables. It is specified via one or more `page_structure` objects.
Within the `page_structure`, there are two possible methods for defining
the splits:

- Split by every unique value of a `group` or `label` variable. This can
  be done by passing the value “.default” as such:
  `page_structure(group_val = ".default")`.

- Split by a specific value of a `group` or `label` variable. This can
  be done by passing the specific value as such:
  `page_structure(label_val = "n")`. This will split after rows where
  label_val = “n”. If there are consecutive rows with label_val = “n”,
  it will split after the final row of that section of rows. This logic
  extends to instances where there are multiple sections with
  consecutive rows with label_val = “n”; there will be a split at the
  bottom of each section. Note: There can only be one `page_structure`
  that includes this level of specificity.

These methods can also be combined with a `page_structure` or across
multiple `page_structure`s. For example,
`page_structure(group_val = ".default", label_val = "n")` will split on
every unique group_val, as well as every instance where label_val = “n”.

If any of the `page_structure`s contain a `".default"`, it may be
desired to print a note indicating the grouping value for the given page
at the time of rendering. The location of this note can be specified via
the `note_loc` parameter in `page_plan`. The functionality for
`note_loc` may be limited by the desired output type; for example,
“preheader” is only available for RTF outputs, while “source_note” and
“subtitle” are available for all output types. The content of this note
can be modified by supplying a function via the `transform` argument.

2.  **Max Rows-driven**:

This feature, available via the `max_rows` argument in `page_plan`,
involves splitting based on the maximum number of rows per table. Rows
dedicated to group labels (without data) are included in the row counts.
If a set of rows within a single `group` value are split apart, the
group label will be repeated for each page.

**NOTE**: If both `max_rows` and `page_structure` are provided to the
`page_plan`, the table will first be split according to the
`page_structure`, follwed by the `max_rows`.

Examples of each of these approaches are below. To reduce the amount of
code displayed in the examples, following the initial table, `tfrmt`’s
layering functionality will be used to add the `page_plan`. For more
information about this, see the “Layering tfrmts” vignette.

### Values-driven splitting

Let’s take a subset of our example demography data.

Expand for the code used to produce this subset

``` r
data_demog2 <- data_demog |>
  filter(rowlbl1 %in% unique(rowlbl1)[1:3])
```

``` r
head(data_demog2)
#> # A tibble: 6 × 8
#> # Groups:   rowlbl1 [1]
#>   rowlbl1 rowlbl2 param grp    ord1  ord2 column                 value
#>   <chr>   <chr>   <chr> <chr> <dbl> <dbl> <chr>                  <dbl>
#> 1 Age (y) n       n     cont      1     1 Placebo               86    
#> 2 Age (y) n       n     cont      1     1 Xanomeline Low Dose   84    
#> 3 Age (y) n       n     cont      1     1 Xanomeline High Dose  84    
#> 4 Age (y) n       n     cont      1     1 Total                254    
#> 5 Age (y) n       p     cont      1     1 p-value                0.593
#> 6 Age (y) Mean    Mean  cont      1     2 Placebo               75.2
```

The formatted table as a single page is as follows:

``` r
base_tfrmt <- tfrmt(
  # specify columns in the data
  group = c(rowlbl1, grp),
  label = rowlbl2,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      frmt_combine(
        "{n} {pct}",
        n = frmt("xxx"),
        pct = frmt_when(
          "==100" ~ "",
          "==0" ~ "",
          TRUE ~ frmt("(xx.x %)")
        )
      )
    ),
    frmt_structure(
      group_val = ".default",
      label_val = "n",
      frmt("xxx")
    ),
    frmt_structure(
      group_val = ".default",
      label_val = c("Mean", "Median", "Min", "Max"),
      frmt("xxx.x")
    ),
    frmt_structure(
      group_val = ".default",
      label_val = "SD",
      frmt("xxx.xx")
    ),
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      p = frmt("")
    ),
    frmt_structure(
      group_val = ".default",
      label_val = c("n", "<65 yrs", "<12 months", "<25"),
      p = frmt_when(
        ">0.99" ~ ">0.99",
        "<0.001" ~ "<0.001",
        TRUE ~ frmt("x.xxx", missing = "")
      )
    )
  ),
  # remove extra cols
  col_plan = col_plan(
    -grp,
    -starts_with("ord")
  ),
  # Specify column styling plan
  col_style_plan = col_style_plan(
    col_style_structure(
      align = c(".", ",", " "),
      col = vars(Placebo, contains("Dose"), "Total", "p-value")
    )
  ),
  # Specify row group plan
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = ".default",
      element_block(post_space = " ")
    ),
    label_loc = element_row_grp_loc(
      location = "column"
    )
  )
)

base_tfrmt |>
  print_to_gt(data_demog2)
```

|                      |                 |   Placebo    | Xanomeline Low Dose | Xanomeline High Dose |    Total     | p-value |
|:---------------------|:----------------|:------------:|:-------------------:|:--------------------:|:------------:|:-------:|
| Age (y)              | n               |  86          |     84              |      84              | 254          |  0.593  |
|                      | Mean            |  75.2        |     75.7            |      74.4            |  75.1        |         |
|                      | SD              |   8.59       |      8.29           |       7.89           |   8.25       |         |
|                      | Median          |  76.0        |     77.5            |      76.0            |  77.0        |         |
|                      | Min             |  52.0        |     51.0            |      56.0            |  51.0        |         |
|                      | Max             |  89.0        |     88.0            |      88.0            |  89.0        |         |
|                      |                 |              |                     |                      |              |         |
|                      | \<65 yrs        |  14 (16.3 %) |      8 ( 9.5 %)     |      11 (13.1 %)     |  33 (13.0 %) |  0.144  |
|                      | 65-80 yrs       |  42 (48.8 %) |     47 (56.0 %)     |      55 (65.5 %)     | 144 (56.7 %) |         |
|                      | \>80 yrs        |  30 (34.9 %) |     29 (34.5 %)     |      18 (21.4 %)     |  77 (30.3 %) |         |
|                      |                 |              |                     |                      |              |         |
| Sex                  | n               |  86          |     84              |      84              | 254          |  0.141  |
|                      | Male            |  33 (38.4 %) |     34 (40.5 %)     |      44 (52.4 %)     | 111 (43.7 %) |         |
|                      | Female          |  53 (61.6 %) |     50 (59.5 %)     |      40 (47.6 %)     | 143 (56.3 %) |         |
|                      |                 |              |                     |                      |              |         |
| Race (Origin)        | n               |  86          |     84              |      84              | 254          |  0.648  |
|                      | Caucasian       |  75 (87.2 %) |     72 (85.7 %)     |      71 (84.5 %)     | 218 (85.8 %) |         |
|                      | African Descent |   8 ( 9.3 %) |      6 ( 7.1 %)     |       9 (10.7 %)     |  23 ( 9.1 %) |         |
|                      | Hispanic        |   3 ( 3.5 %) |      6 ( 7.1 %)     |       3 ( 3.6 %)     |  12 ( 4.7 %) |         |
|                      | Other           |              |                     |       1 ( 1.2 %)     |   1 ( 0.4 %) |         |
|                      |                 |              |                     |                      |              |         |
| MMSE                 | n               |  86          |     84              |      84              | 254          |  0.595  |
|                      | Mean            |  18.0        |     17.9            |      18.5            |  18.1        |         |
|                      | SD              |   4.27       |      4.22           |       4.16           |   4.21       |         |
|                      | Median          |  19.5        |     18.0            |      20.0            |  19.0        |         |
|                      | Min             |  10.0        |     10.0            |      10.0            |  10.0        |         |
|                      | Max             |  23.0        |     24.0            |      24.0            |  24.0        |         |
|                      |                 |              |                     |                      |              |         |
| Duration of disease  | n               |  86          |     84              |      84              | 254          |  0.153  |
|                      | Mean            |  42.6        |     48.7            |      40.5            |  43.9        |         |
|                      | SD              |  30.24       |     29.58           |      24.69           |  28.40       |         |
|                      | Median          |  35.3        |     40.2            |      36.0            |  36.2        |         |
|                      | Min             |   7.2        |      7.8            |       2.2            |   2.2        |         |
|                      | Max             | 183.1        |    130.8            |     135.0            | 183.1        |         |
|                      |                 |              |                     |                      |              |         |
|                      | \<12 months     |   5 ( 5.8 %) |      3 ( 3.6 %)     |       4 ( 4.8 %)     |  12 ( 4.7 %) |  0.789  |
|                      | \>=12 months    |  81 (94.2 %) |     81 (96.4 %)     |      80 (95.2 %)     | 242 (95.3 %) |         |
|                      |                 |              |                     |                      |              |         |
| Years of education   | n               |  86          |     84              |      84              | 254          |  0.388  |
|                      | Mean            |  12.6        |     13.2            |      12.5            |  12.8        |         |
|                      | SD              |   2.95       |      4.15           |       2.92           |   3.38       |         |
|                      | Median          |  12.0        |     12.0            |      12.0            |  12.0        |         |
|                      | Min             |   6.0        |      3.0            |       6.0            |   3.0        |         |
|                      | Max             |  21.0        |     24.0            |      20.0            |  24.0        |         |
|                      |                 |              |                     |                      |              |         |
| Baseline weight(kg)  | n               |  86          |     83              |      84              | 253          |  0.003  |
|                      | Mean            |  62.8        |     67.3            |      70.0            |  66.6        |         |
|                      | SD              |  12.77       |     14.12           |      14.65           |  14.13       |         |
|                      | Median          |  60.5        |     64.9            |      69.2            |  66.7        |         |
|                      | Min             |  34.0        |     45.4            |      41.7            |  34.0        |         |
|                      | Max             |  86.2        |    106.1            |     108.0            | 108.0        |         |
|                      |                 |              |                     |                      |              |         |
| Baseline height(cm)  | n               |  86          |     84              |      84              | 254          |  0.126  |
|                      | Mean            | 162.6        |    163.4            |     165.8            | 163.9        |         |
|                      | SD              |  11.52       |     10.42           |      10.13           |  10.76       |         |
|                      | Median          | 162.6        |    162.6            |     165.1            | 162.8        |         |
|                      | Min             | 137.2        |    135.9            |     146.1            | 135.9        |         |
|                      | Max             | 185.4        |    195.6            |     190.5            | 195.6        |         |
|                      |                 |              |                     |                      |              |         |
| Baseline BMI         | n               |  86          |     83              |      84              | 253          |  0.013  |
|                      | Mean            |  23.6        |     25.1            |      25.3            |  24.7        |         |
|                      | SD              |   3.67       |      4.27           |       4.16           |   4.09       |         |
|                      | Median          |  23.4        |     24.3            |      24.8            |  24.2        |         |
|                      | Min             |  15.1        |     17.7            |      13.7            |  13.7        |         |
|                      | Max             |  33.3        |     40.1            |      34.5            |  40.1        |         |
|                      |                 |              |                     |                      |              |         |
|                      | \<25            |  59 (68.6 %) |     47 (56.0 %)     |      44 (52.4 %)     | 150 (59.1 %) |  0.233  |
|                      | 25-\<30         |  21 (24.4 %) |     27 (32.1 %)     |      28 (33.3 %)     |  76 (29.9 %) |         |
|                      | \>=30           |   6 ( 7.0 %) |     10 (11.9 %)     |      12 (14.3 %)     |  28 (11.0 %) |         |
|                      |                 |              |                     |                      |              |         |

#### Every unique value of 1 variable

Suppose we want to split the table by every unique value of `rowlbl1`
and add a footnote indicating the table grouping. We can drop `rowlbl1`
from the tables since its value will be printed in the note. We will use
the [`gt::grp_pull()`](https://gt.rstudio.com/reference/grp_pull.html)
function to print the individual tables nicely in the vignette.

``` r
gts <- base_tfrmt |>
  layer_tfrmt(
    tfrmt(
      # page plan
      page_plan = page_plan(
        page_structure(
          group_val = list(
            rowlbl1 = ".default"
          )
        ),
        note_loc = "source_note"
      )
    )
  ) |>
  print_to_gt(data_demog2)
```

``` r
gts |> gt::grp_pull(1)
```

|                  |           |   Placebo   | Xanomeline Low Dose | Xanomeline High Dose |    Total     | p-value |
|:-----------------|:----------|:-----------:|:-------------------:|:--------------------:|:------------:|:-------:|
| Age (y)          | n         | 86          |     84              |     84               | 254          |  0.593  |
|                  | Mean      | 75.2        |     75.7            |     74.4             |  75.1        |         |
|                  | SD        |  8.59       |      8.29           |      7.89            |   8.25       |         |
|                  | Median    | 76.0        |     77.5            |     76.0             |  77.0        |         |
|                  | Min       | 52.0        |     51.0            |     56.0             |  51.0        |         |
|                  | Max       | 89.0        |     88.0            |     88.0             |  89.0        |         |
|                  |           |             |                     |                      |              |         |
|                  | \<65 yrs  | 14 (16.3 %) |      8 ( 9.5 %)     |     11 (13.1 %)      |  33 (13.0 %) |  0.144  |
|                  | 65-80 yrs | 42 (48.8 %) |     47 (56.0 %)     |     55 (65.5 %)      | 144 (56.7 %) |         |
|                  | \>80 yrs  | 30 (34.9 %) |     29 (34.5 %)     |     18 (21.4 %)      |  77 (30.3 %) |         |
|                  |           |             |                     |                      |              |         |
| rowlbl1: Age (y) |           |             |                     |                      |              |         |

``` r
gts |> gt::grp_pull(2)
```

|              |        |   Placebo   | Xanomeline Low Dose | Xanomeline High Dose |    Total     | p-value |
|:-------------|:-------|:-----------:|:-------------------:|:--------------------:|:------------:|:-------:|
| Sex          | n      | 86          |     84              |     84               | 254          |  0.141  |
|              | Male   | 33 (38.4 %) |     34 (40.5 %)     |     44 (52.4 %)      | 111 (43.7 %) |         |
|              | Female | 53 (61.6 %) |     50 (59.5 %)     |     40 (47.6 %)      | 143 (56.3 %) |         |
|              |        |             |                     |                      |              |         |
| rowlbl1: Sex |        |             |                     |                      |              |         |

``` r
gts |> gt::grp_pull(3)
```

|                        |                 |   Placebo   | Xanomeline Low Dose | Xanomeline High Dose |    Total     | p-value |
|:-----------------------|:----------------|:-----------:|:-------------------:|:--------------------:|:------------:|:-------:|
| Race (Origin)          | n               | 86          |     84              |     84               | 254          |  0.648  |
|                        | Caucasian       | 75 (87.2 %) |     72 (85.7 %)     |     71 (84.5 %)      | 218 (85.8 %) |         |
|                        | African Descent |  8 ( 9.3 %) |      6 ( 7.1 %)     |      9 (10.7 %)      |  23 ( 9.1 %) |         |
|                        | Hispanic        |  3 ( 3.5 %) |      6 ( 7.1 %)     |      3 ( 3.6 %)      |  12 ( 4.7 %) |         |
|                        | Other           |             |                     |      1 ( 1.2 %)      |   1 ( 0.4 %) |         |
|                        |                 |             |                     |                      |              |         |
| rowlbl1: Race (Origin) |                 |             |                     |                      |              |         |

#### Every unique value of 2 variables

We could also choose to split on both grouping variables as such
(showing first 3 tables only):

``` r
gts <- base_tfrmt |>
  layer_tfrmt(
    tfrmt(
      # page plan
      page_plan = page_plan(
        page_structure(group_val = ".default"),
        note_loc = "source_note"
      )
    )
  ) |>
  print_to_gt(data_demog2)
```

``` r
gts |> gt::grp_pull(1)
```

|                             |        | Placebo | Xanomeline Low Dose | Xanomeline High Dose | Total  | p-value |
|:----------------------------|:-------|:-------:|:-------------------:|:--------------------:|:------:|:-------:|
| Age (y)                     | n      |  86     |        84           |        84            | 254    |  0.593  |
|                             | Mean   |  75.2   |        75.7         |        74.4          |  75.1  |         |
|                             | SD     |   8.59  |         8.29        |         7.89         |   8.25 |         |
|                             | Median |  76.0   |        77.5         |        76.0          |  77.0  |         |
|                             | Min    |  52.0   |        51.0         |        56.0          |  51.0  |         |
|                             | Max    |  89.0   |        88.0         |        88.0          |  89.0  |         |
|                             |        |         |                     |                      |        |         |
| rowlbl1: Age (y), grp: cont |        |         |                     |                      |        |         |

``` r
gts |> gt::grp_pull(2)
```

|                            |           |   Placebo   | Xanomeline Low Dose | Xanomeline High Dose |    Total     | p-value |
|:---------------------------|:----------|:-----------:|:-------------------:|:--------------------:|:------------:|:-------:|
| Age (y)                    | \<65 yrs  | 14 (16.3 %) |      8 ( 9.5 %)     |     11 (13.1 %)      |  33 (13.0 %) |  0.144  |
|                            | 65-80 yrs | 42 (48.8 %) |     47 (56.0 %)     |     55 (65.5 %)      | 144 (56.7 %) |         |
|                            | \>80 yrs  | 30 (34.9 %) |     29 (34.5 %)     |     18 (21.4 %)      |  77 (30.3 %) |         |
|                            |           |             |                     |                      |              |         |
| rowlbl1: Age (y), grp: cat |           |             |                     |                      |              |         |

``` r
gts |> gt::grp_pull(3)
```

|                        |        |   Placebo   | Xanomeline Low Dose | Xanomeline High Dose |    Total     | p-value |
|:-----------------------|:-------|:-----------:|:-------------------:|:--------------------:|:------------:|:-------:|
| Sex                    | n      | 86          |     84              |     84               | 254          |  0.141  |
|                        | Male   | 33 (38.4 %) |     34 (40.5 %)     |     44 (52.4 %)      | 111 (43.7 %) |         |
|                        | Female | 53 (61.6 %) |     50 (59.5 %)     |     40 (47.6 %)      | 143 (56.3 %) |         |
|                        |        |             |                     |                      |              |         |
| rowlbl1: Sex, grp: cat |        |             |                     |                      |              |         |

Notes can sometimes end up not being very human readable. We could
change them (let’s say from `"rowlbl1: Age (y), grp: cat"` to
`"Row label 1 - Age (y), Group - cat"`) by supplying a transformation
function to
[`page_plan()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/page_plan.md) -
i.e. the `transform` argument.

``` r
transform_note <- function(x) {
  x |>
  stringr::str_replace("rowlbl", "Row label ") |>
  stringr::str_replace("grp", "Group") |>
  stringr::str_replace_all(":", " -")
}

gts <- base_tfrmt |>
  layer_tfrmt(
    tfrmt(
      # page plan
      page_plan = page_plan(
        page_structure(group_val = ".default"),
        note_loc = "source_note",
        transform = transform_note
      )
    )
  ) |>
  print_to_gt(data_demog2)

gts |> gt::grp_pull(1)
```

|                                     |        | Placebo | Xanomeline Low Dose | Xanomeline High Dose | Total  | p-value |
|:------------------------------------|:-------|:-------:|:-------------------:|:--------------------:|:------:|:-------:|
| Age (y)                             | n      |  86     |        84           |        84            | 254    |  0.593  |
|                                     | Mean   |  75.2   |        75.7         |        74.4          |  75.1  |         |
|                                     | SD     |   8.59  |         8.29        |         7.89         |   8.25 |         |
|                                     | Median |  76.0   |        77.5         |        76.0          |  77.0  |         |
|                                     | Min    |  52.0   |        51.0         |        56.0          |  51.0  |         |
|                                     | Max    |  89.0   |        88.0         |        88.0          |  89.0  |         |
|                                     |        |         |                     |                      |        |         |
| Row label 1 - Age (y), Group - cont |        |         |                     |                      |        |         |

#### Specific value of a variable

Finally, we could split on a specific value observed in the data. For
example, `rowlbl1 = "Age (y)"`.

``` r
gts <- base_tfrmt |>
  layer_tfrmt(
    tfrmt(
      # page plan
      page_plan = page_plan(
        page_structure(
          group_val = list(
            rowlbl1 = "Age (y)"
          )
        ),
        note_loc = "source_note"
      )
    )
  ) |>
  print_to_gt(data_demog2)
```

``` r
gts |> gt::grp_pull(1)
```

|         |           |   Placebo   | Xanomeline Low Dose | Xanomeline High Dose |    Total     | p-value |
|:--------|:----------|:-----------:|:-------------------:|:--------------------:|:------------:|:-------:|
| Age (y) | n         | 86          |     84              |     84               | 254          |  0.593  |
|         | Mean      | 75.2        |     75.7            |     74.4             |  75.1        |         |
|         | SD        |  8.59       |      8.29           |      7.89            |   8.25       |         |
|         | Median    | 76.0        |     77.5            |     76.0             |  77.0        |         |
|         | Min       | 52.0        |     51.0            |     56.0             |  51.0        |         |
|         | Max       | 89.0        |     88.0            |     88.0             |  89.0        |         |
|         |           |             |                     |                      |              |         |
|         | \<65 yrs  | 14 (16.3 %) |      8 ( 9.5 %)     |     11 (13.1 %)      |  33 (13.0 %) |  0.144  |
|         | 65-80 yrs | 42 (48.8 %) |     47 (56.0 %)     |     55 (65.5 %)      | 144 (56.7 %) |         |
|         | \>80 yrs  | 30 (34.9 %) |     29 (34.5 %)     |     18 (21.4 %)      |  77 (30.3 %) |         |
|         |           |             |                     |                      |              |         |

``` r
gts |> gt::grp_pull(2)
```

|                      |                 |   Placebo    | Xanomeline Low Dose | Xanomeline High Dose |    Total     | p-value |
|:---------------------|:----------------|:------------:|:-------------------:|:--------------------:|:------------:|:-------:|
| Sex                  | n               |  86          |     84              |      84              | 254          |  0.141  |
|                      | Male            |  33 (38.4 %) |     34 (40.5 %)     |      44 (52.4 %)     | 111 (43.7 %) |         |
|                      | Female          |  53 (61.6 %) |     50 (59.5 %)     |      40 (47.6 %)     | 143 (56.3 %) |         |
|                      |                 |              |                     |                      |              |         |
| Race (Origin)        | n               |  86          |     84              |      84              | 254          |  0.648  |
|                      | Caucasian       |  75 (87.2 %) |     72 (85.7 %)     |      71 (84.5 %)     | 218 (85.8 %) |         |
|                      | African Descent |   8 ( 9.3 %) |      6 ( 7.1 %)     |       9 (10.7 %)     |  23 ( 9.1 %) |         |
|                      | Hispanic        |   3 ( 3.5 %) |      6 ( 7.1 %)     |       3 ( 3.6 %)     |  12 ( 4.7 %) |         |
|                      | Other           |              |                     |       1 ( 1.2 %)     |   1 ( 0.4 %) |         |
|                      |                 |              |                     |                      |              |         |
| MMSE                 | n               |  86          |     84              |      84              | 254          |  0.595  |
|                      | Mean            |  18.0        |     17.9            |      18.5            |  18.1        |         |
|                      | SD              |   4.27       |      4.22           |       4.16           |   4.21       |         |
|                      | Median          |  19.5        |     18.0            |      20.0            |  19.0        |         |
|                      | Min             |  10.0        |     10.0            |      10.0            |  10.0        |         |
|                      | Max             |  23.0        |     24.0            |      24.0            |  24.0        |         |
|                      |                 |              |                     |                      |              |         |
| Duration of disease  | n               |  86          |     84              |      84              | 254          |  0.153  |
|                      | Mean            |  42.6        |     48.7            |      40.5            |  43.9        |         |
|                      | SD              |  30.24       |     29.58           |      24.69           |  28.40       |         |
|                      | Median          |  35.3        |     40.2            |      36.0            |  36.2        |         |
|                      | Min             |   7.2        |      7.8            |       2.2            |   2.2        |         |
|                      | Max             | 183.1        |    130.8            |     135.0            | 183.1        |         |
|                      |                 |              |                     |                      |              |         |
|                      | \<12 months     |   5 ( 5.8 %) |      3 ( 3.6 %)     |       4 ( 4.8 %)     |  12 ( 4.7 %) |  0.789  |
|                      | \>=12 months    |  81 (94.2 %) |     81 (96.4 %)     |      80 (95.2 %)     | 242 (95.3 %) |         |
|                      |                 |              |                     |                      |              |         |
| Years of education   | n               |  86          |     84              |      84              | 254          |  0.388  |
|                      | Mean            |  12.6        |     13.2            |      12.5            |  12.8        |         |
|                      | SD              |   2.95       |      4.15           |       2.92           |   3.38       |         |
|                      | Median          |  12.0        |     12.0            |      12.0            |  12.0        |         |
|                      | Min             |   6.0        |      3.0            |       6.0            |   3.0        |         |
|                      | Max             |  21.0        |     24.0            |      20.0            |  24.0        |         |
|                      |                 |              |                     |                      |              |         |
| Baseline weight(kg)  | n               |  86          |     83              |      84              | 253          |  0.003  |
|                      | Mean            |  62.8        |     67.3            |      70.0            |  66.6        |         |
|                      | SD              |  12.77       |     14.12           |      14.65           |  14.13       |         |
|                      | Median          |  60.5        |     64.9            |      69.2            |  66.7        |         |
|                      | Min             |  34.0        |     45.4            |      41.7            |  34.0        |         |
|                      | Max             |  86.2        |    106.1            |     108.0            | 108.0        |         |
|                      |                 |              |                     |                      |              |         |
| Baseline height(cm)  | n               |  86          |     84              |      84              | 254          |  0.126  |
|                      | Mean            | 162.6        |    163.4            |     165.8            | 163.9        |         |
|                      | SD              |  11.52       |     10.42           |      10.13           |  10.76       |         |
|                      | Median          | 162.6        |    162.6            |     165.1            | 162.8        |         |
|                      | Min             | 137.2        |    135.9            |     146.1            | 135.9        |         |
|                      | Max             | 185.4        |    195.6            |     190.5            | 195.6        |         |
|                      |                 |              |                     |                      |              |         |
| Baseline BMI         | n               |  86          |     83              |      84              | 253          |  0.013  |
|                      | Mean            |  23.6        |     25.1            |      25.3            |  24.7        |         |
|                      | SD              |   3.67       |      4.27           |       4.16           |   4.09       |         |
|                      | Median          |  23.4        |     24.3            |      24.8            |  24.2        |         |
|                      | Min             |  15.1        |     17.7            |      13.7            |  13.7        |         |
|                      | Max             |  33.3        |     40.1            |      34.5            |  40.1        |         |
|                      |                 |              |                     |                      |              |         |
|                      | \<25            |  59 (68.6 %) |     47 (56.0 %)     |      44 (52.4 %)     | 150 (59.1 %) |  0.233  |
|                      | 25-\<30         |  21 (24.4 %) |     27 (32.1 %)     |      28 (33.3 %)     |  76 (29.9 %) |         |
|                      | \>=30           |   6 ( 7.0 %) |     10 (11.9 %)     |      12 (14.3 %)     |  28 (11.0 %) |         |
|                      |                 |              |                     |                      |              |         |

### Splitting based on maximum rows

To instead limit the number of rows per page, we can set the `max_rows`
argument as such (showing first 3 tables only):

``` r
gts <- base_tfrmt |>
  layer_tfrmt(
    tfrmt(
      # page plan
      page_plan = page_plan(
        max_rows = 20
      )
    )
  ) |>
  print_to_gt(data_demog2)
```

``` r
gts |> gt::grp_pull(1)
```

|               |                 |   Placebo   | Xanomeline Low Dose | Xanomeline High Dose |    Total     | p-value |
|:--------------|:----------------|:-----------:|:-------------------:|:--------------------:|:------------:|:-------:|
| Age (y)       | n               | 86          |     84              |     84               | 254          |  0.593  |
|               | Mean            | 75.2        |     75.7            |     74.4             |  75.1        |         |
|               | SD              |  8.59       |      8.29           |      7.89            |   8.25       |         |
|               | Median          | 76.0        |     77.5            |     76.0             |  77.0        |         |
|               | Min             | 52.0        |     51.0            |     56.0             |  51.0        |         |
|               | Max             | 89.0        |     88.0            |     88.0             |  89.0        |         |
|               |                 |             |                     |                      |              |         |
|               | \<65 yrs        | 14 (16.3 %) |      8 ( 9.5 %)     |     11 (13.1 %)      |  33 (13.0 %) |  0.144  |
|               | 65-80 yrs       | 42 (48.8 %) |     47 (56.0 %)     |     55 (65.5 %)      | 144 (56.7 %) |         |
|               | \>80 yrs        | 30 (34.9 %) |     29 (34.5 %)     |     18 (21.4 %)      |  77 (30.3 %) |         |
|               |                 |             |                     |                      |              |         |
| Sex           | n               | 86          |     84              |     84               | 254          |  0.141  |
|               | Male            | 33 (38.4 %) |     34 (40.5 %)     |     44 (52.4 %)      | 111 (43.7 %) |         |
|               | Female          | 53 (61.6 %) |     50 (59.5 %)     |     40 (47.6 %)      | 143 (56.3 %) |         |
|               |                 |             |                     |                      |              |         |
| Race (Origin) | n               | 86          |     84              |     84               | 254          |  0.648  |
|               | Caucasian       | 75 (87.2 %) |     72 (85.7 %)     |     71 (84.5 %)      | 218 (85.8 %) |         |
|               | African Descent |  8 ( 9.3 %) |      6 ( 7.1 %)     |      9 (10.7 %)      |  23 ( 9.1 %) |         |
|               | Hispanic        |  3 ( 3.5 %) |      6 ( 7.1 %)     |      3 ( 3.6 %)      |  12 ( 4.7 %) |         |
|               | Other           |             |                     |      1 ( 1.2 %)      |   1 ( 0.4 %) |         |

``` r
gts |> gt::grp_pull(2)
```

|                      |              |   Placebo    | Xanomeline Low Dose | Xanomeline High Dose |    Total     | p-value |
|:---------------------|:-------------|:------------:|:-------------------:|:--------------------:|:------------:|:-------:|
| Race (Origin)        |              |              |                     |                      |              |         |
| MMSE                 | n            |  86          |     84              |      84              | 254          |  0.595  |
|                      | Mean         |  18.0        |     17.9            |      18.5            |  18.1        |         |
|                      | SD           |   4.27       |      4.22           |       4.16           |   4.21       |         |
|                      | Median       |  19.5        |     18.0            |      20.0            |  19.0        |         |
|                      | Min          |  10.0        |     10.0            |      10.0            |  10.0        |         |
|                      | Max          |  23.0        |     24.0            |      24.0            |  24.0        |         |
|                      |              |              |                     |                      |              |         |
| Duration of disease  | n            |  86          |     84              |      84              | 254          |  0.153  |
|                      | Mean         |  42.6        |     48.7            |      40.5            |  43.9        |         |
|                      | SD           |  30.24       |     29.58           |      24.69           |  28.40       |         |
|                      | Median       |  35.3        |     40.2            |      36.0            |  36.2        |         |
|                      | Min          |   7.2        |      7.8            |       2.2            |   2.2        |         |
|                      | Max          | 183.1        |    130.8            |     135.0            | 183.1        |         |
|                      |              |              |                     |                      |              |         |
|                      | \<12 months  |   5 ( 5.8 %) |      3 ( 3.6 %)     |       4 ( 4.8 %)     |  12 ( 4.7 %) |  0.789  |
|                      | \>=12 months |  81 (94.2 %) |     81 (96.4 %)     |      80 (95.2 %)     | 242 (95.3 %) |         |
|                      |              |              |                     |                      |              |         |
| Years of education   | n            |  86          |     84              |      84              | 254          |  0.388  |
|                      | Mean         |  12.6        |     13.2            |      12.5            |  12.8        |         |

``` r
gts |> gt::grp_pull(3)
```

|                     |        | Placebo | Xanomeline Low Dose | Xanomeline High Dose | Total  | p-value |
|:--------------------|:-------|:-------:|:-------------------:|:--------------------:|:------:|:-------:|
| Years of education  | SD     |   2.95  |         4.15        |          2.92        |   3.38 |         |
|                     | Median |  12.0   |        12.0         |         12.0         |  12.0  |         |
|                     | Min    |   6.0   |         3.0         |          6.0         |   3.0  |         |
|                     | Max    |  21.0   |        24.0         |         20.0         |  24.0  |         |
|                     |        |         |                     |                      |        |         |
| Baseline weight(kg) | n      |  86     |        83           |         84           | 253    |  0.003  |
|                     | Mean   |  62.8   |        67.3         |         70.0         |  66.6  |         |
|                     | SD     |  12.77  |        14.12        |         14.65        |  14.13 |         |
|                     | Median |  60.5   |        64.9         |         69.2         |  66.7  |         |
|                     | Min    |  34.0   |        45.4         |         41.7         |  34.0  |         |
|                     | Max    |  86.2   |       106.1         |        108.0         | 108.0  |         |
|                     |        |         |                     |                      |        |         |
| Baseline height(cm) | n      |  86     |        84           |         84           | 254    |  0.126  |
|                     | Mean   | 162.6   |       163.4         |        165.8         | 163.9  |         |
|                     | SD     |  11.52  |        10.42        |         10.13        |  10.76 |         |
|                     | Median | 162.6   |       162.6         |        165.1         | 162.8  |         |
|                     | Min    | 137.2   |       135.9         |        146.1         | 135.9  |         |
|                     | Max    | 185.4   |       195.6         |        190.5         | 195.6  |         |
|                     |        |         |                     |                      |        |         |
| Baseline BMI        | n      |  86     |        83           |         84           | 253    |  0.013  |

Notice that for groups (defined by the `rowlbl1` variable) that are
split up, the group header is repeated for each table.

## Page Plan outputs

When Page Plan is applied, the result of the
[`print_to_gt()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/print_to_gt.md)
or
[`print_mock_gt()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/print_mock_gt.md)
functions is a `gt_group` object (collection of individual `gt`s).
Passing this result to the
[`gt::gtsave()`](https://gt.rstudio.com/reference/gtsave.html) function
will result in a multi-paged output.
