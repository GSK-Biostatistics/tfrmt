# Examples

``` r
library(tfrmt)
```

## Demography Table

For this demography table we are going to use `data_demog`, an example
analysis results dataset found in the package, which is based on the
CDISC pilot data. This dataset has two different row label columns,
`rowlbl1` and `rowlbl2` because we are building a table with group and
row labels. There are also two order columns which will be used to set
the row order of the output. There is a single column to define our
table’s columns (multiple column columns are used when there is column
spanning). Finally there is a param column, a value column and an
additional grouping column, `grp`, which we can use for more complex
formatting.

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

The mock we are going to match looks like this:

|                     |                  | Placebo      | Xanomeline Low Dose | Xanomeline High Dose | Total        | p-value |
|:--------------------|:-----------------|:-------------|:--------------------|:---------------------|:-------------|---------|
| Age (y)             | n                | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                     | Mean             | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | SD               | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                     | Median           | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Min              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Max              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     |                  |              |                     |                      |              |         |
|                     | \<65 yrs         | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) | x.xxx   |
|                     | 65-80 yrs        | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                     | \>80 yrs         | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                     |                  |              |                     |                      |              |         |
| Sex                 | n                | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                     | Male             | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                     | Female           | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                     |                  |              |                     |                      |              |         |
| Race (Origin)       | n                | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                     | Caucasian        | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                     | African Descent  | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                     | Hispanic         | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                     | Other            | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                     |                  |              |                     |                      |              |         |
| MMSE                | n                | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                     | Mean             | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | SD               | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                     | Median           | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Min              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Max              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     |                  |              |                     |                      |              |         |
| Duration of disease | n                | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                     | Mean             | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | SD               | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                     | Median           | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Min              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Max              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     |                  |              |                     |                      |              |         |
|                     | \<12 months      | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) | x.xxx   |
|                     | \>=12 months     | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                     |                  |              |                     |                      |              |         |
| Years of education  | n                | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                     | Mean             | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | SD               | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                     | Median           | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Min              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Max              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     |                  |              |                     |                      |              |         |
| Baseline weight(kg) | n                | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                     | Mean             | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | SD               | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                     | Median           | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Min              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Max              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     |                  |              |                     |                      |              |         |
| Baseline height(cm) | n                | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                     | Mean             | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | SD               | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                     | Median           | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Min              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Max              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     |                  |              |                     |                      |              |         |
| Baseline BMI        | n                | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                     | Mean             | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | SD               | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                     | Median           | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Min              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     | Max              | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                     |                  |              |                     |                      |              |         |
|                     | \<25             | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) | x.xxx   |
|                     | 25-\<30          | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                     | \>=30            | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                     |                  |              |                     |                      |              |         |

For this table, we have three columns for each of the treatment groups,
a total column for all groups combined, and a p-value column. The table
also contains a mix of categorical and continuous analysis.

The first thing we are going to do when building out the `tfrmt` is
specify all our columns

``` r
tfrmt(
  # specify columns in the data
  group = c(rowlbl1, grp),
  label = rowlbl2,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2)
) |>
  print_to_gt(data_demog) |>
  gt::tab_options(
    container.width = 900
  )
```

[TABLE]

While this makes a table, it isn’t a very nice table and definitely
doesn’t match the mock. So let’s start with formatting all the numbers.
To do this we are going to build a `body_plan` to add to our `tfrmt`.
This will be a fairly quick explanation of `body_plan`s but if you would
like more information see `vignettes("Body Plan")`

Body plans are made up of a series of `frmt_stucture`s where each
`frmt_stucture` represents the formatting of a cell within the table.
The order of the `frmt_structure`s matter; they are always applied
latest to oldest. This means the first `frmt_stucture` in the
`body_plan` should be the most generic. You can use the groups, labels
and parameters to specify which formatting applies to which values.

To start, we are going to use all the rows that are “n (%)” as the
default. This way we don’t need to list out every row that is an “n (%)”
row. These rows are made up of two different values, so we will need to
use `frmt_combine`. Next, we can format the continuous variables, which
is just a straightforward one value per row so we can just use the label
to filter and `frmt` to define the look. Finally, we want to format the
p-values. This is a bit more complicated, since the p-value sits in the
same row as other parameters; therefore the group and label value are
not specific enough and we need something more granular. As such, we
will need to specify the parameter in the `frmt_structure` like so:
`frmt_structure(group_val = ".default", label_val = ".default", p = frmt("x.xx")`.
Further, we also need to make sure it never displays a rounded p-value
of 0 or 1. So we can use `frmt_when` to specify the formatting based on
the value.

``` r
tfrmt(
  # specify columns in the data
  group = c(rowlbl1, grp),
  label = rowlbl2,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} ({pct} %)",
      n = frmt("xxx"),
      pct = frmt("xx.x")
    )),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
    frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min", "Max"), frmt("xxx.x")),
    frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx")),
    frmt_structure(group_val = ".default", label_val = ".default", p = frmt_when(
      ">0.99" ~ ">0.99",
      "<0.001" ~ "<0.001",
      TRUE ~ frmt("x.xxx", missing = "")
    ))
  )
) |>
  print_to_gt(data_demog) |>
  gt::tab_options(
    container.width = 900
  )
```

[TABLE]

Now that all the numbers look correct, we can drop the order columns and
the `grp` column (note that while we do not want to display the `grp`
column, it plays a role behind the scenes, which will be addressed in
the next step). To do this we use a `col_plan` which uses `tidy-select`
nomenclature to drop/move columns.

``` r
tfrmt(
  # specify columns in the data
  group = c(rowlbl1, grp),
  label = rowlbl2,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
      n = frmt("xxx"),
      pct = frmt_when(
        "==100" ~ "",
        "==0" ~ "",
        TRUE ~ frmt("(xx.x %)")
      )
    )),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
    frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min", "Max"), frmt("xxx.x")),
    frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx")),
    frmt_structure(group_val = ".default", label_val = ".default", p = frmt("")),
    frmt_structure(group_val = ".default", label_val = c("n", "<65 yrs", "<12 months", "<25"), p = frmt_when(
      ">0.99" ~ ">0.99",
      "<0.001" ~ "<0.001",
      TRUE ~ frmt("x.xxx", missing = "")
    ))
  ),
  # remove extra cols
  col_plan = col_plan(
    -grp,
    -starts_with("ord")
  )
) |>
  print_to_gt(data_demog) |>
  gt::tab_options(
    container.width = 900
  )
```

[TABLE]

Now this table looks just about right. There are two problems, (1)
alignment and (2) spacing between the continuous and categorical values.
To take care of the alignment we are going to add a `col_style_plan`
which accepts a series of `col_style_structure`s. This allows columns to
be aligned differently if needed. For this table, we want all the
columns to align on either “.”, “,” or ” ” so our `col_style_structure`
looks like
`col_style_structure(align = c(".",","," "), col = vars(everything()))`.
After the alignment is sorted we can move on to the spacing. In order to
match the spacing of the mock we need to use the extra `grp` column from
our data. If we look at our data, we can see we want a space any time
either of the groups change.

``` r
data_demog |>
  dplyr::distinct(rowlbl1, grp)
#> # A tibble: 12 × 2
#> # Groups:   rowlbl1 [9]
#>    rowlbl1                grp  
#>    <chr>                  <chr>
#>  1 "Age (y)"              cont 
#>  2 "Age (y)"              cat  
#>  3 "Sex"                  cat  
#>  4 "Race (Origin)"        cat  
#>  5 "MMSE"                 cont 
#>  6 "Duration of disease " cont 
#>  7 "Duration of disease " cat  
#>  8 "Years of education"   cont 
#>  9 "Baseline weight(kg)"  cont 
#> 10 "Baseline height(cm)"  cont 
#> 11 "Baseline BMI"         cont 
#> 12 "Baseline BMI"         cat
```

This means that we can use a `row_grp_plan` with just a `".default"` as
the group value and it should handle all of the spacing. In addition to
the spacing, `row_grp_plan` will let us move the spanning group labels
to a separate column by changing the `label_loc` to “column”.

``` r
tfrmt(
  # specify columns in the data
  group = c(rowlbl1, grp),
  label = rowlbl2,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
      n = frmt("xxx"),
      pct = frmt_when(
        "==100" ~ "",
        "==0" ~ "",
        TRUE ~ frmt("(xx.x %)")
      )
    )),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
    frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min", "Max"), frmt("xxx.x")),
    frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx")),
    frmt_structure(group_val = ".default", label_val = ".default", p = frmt("")),
    frmt_structure(group_val = ".default", label_val = c("n", "<65 yrs", "<12 months", "<25"), p = frmt_when(
      ">0.99" ~ ">0.99",
      "<0.001" ~ "<0.001",
      TRUE ~ frmt("x.xxx", missing = "")
    ))
  ),
  # remove extra cols
  col_plan = col_plan(
    -grp,
    -starts_with("ord")
  ),
  # Specify column styling plan
  col_style_plan = col_style_plan(
    col_style_structure(align = c(".", ",", " "), col = c(
      "Placebo", "Xanomeline Low Dose",
      "Xanomeline High Dose", "Total", "p-value"
    )),
    col_style_structure(align = "left", col = c("rowlbl1", "rowlbl2"))
  ),

  # Specify row group plan
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = " ")),
    label_loc = element_row_grp_loc(location = "column")
  )
) |>
  print_to_gt(data_demog) |>
  gt::tab_options(
    container.width = 900
  )
```

|                     |                  | Placebo     | Xanomeline Low Dose | Xanomeline High Dose | Total        | p-value |
|:--------------------|:-----------------|:------------|:--------------------|:---------------------|:-------------|---------|
| Age (y)             | n                | 86          | 84                  | 84                   | 254          | 0.593   |
|                     | Mean             | 75.2        | 75.7                | 74.4                 | 75.1         |         |
|                     | SD               |  8.59       |  8.29               |  7.89                |  8.25        |         |
|                     | Median           | 76.0        | 77.5                | 76.0                 | 77.0         |         |
|                     | Min              | 52.0        | 51.0                | 56.0                 | 51.0         |         |
|                     | Max              | 89.0        | 88.0                | 88.0                 | 89.0         |         |
|                     |                  |             |                     |                      |              |         |
|                     | \<65 yrs         | 14 (16.3 %) |  8 ( 9.5 %)         | 11 (13.1 %)          | 33 (13.0 %)  | 0.144   |
|                     | 65-80 yrs        | 42 (48.8 %) | 47 (56.0 %)         | 55 (65.5 %)          | 144 (56.7 %) |         |
|                     | \>80 yrs         | 30 (34.9 %) | 29 (34.5 %)         | 18 (21.4 %)          | 77 (30.3 %)  |         |
|                     |                  |             |                     |                      |              |         |
| Sex                 | n                | 86          | 84                  | 84                   | 254          | 0.141   |
|                     | Male             | 33 (38.4 %) | 34 (40.5 %)         | 44 (52.4 %)          | 111 (43.7 %) |         |
|                     | Female           | 53 (61.6 %) | 50 (59.5 %)         | 40 (47.6 %)          | 143 (56.3 %) |         |
|                     |                  |             |                     |                      |              |         |
| Race (Origin)       | n                | 86          | 84                  | 84                   | 254          | 0.648   |
|                     | Caucasian        | 75 (87.2 %) | 72 (85.7 %)         | 71 (84.5 %)          | 218 (85.8 %) |         |
|                     | African Descent  |  8 ( 9.3 %) |  6 ( 7.1 %)         |  9 (10.7 %)          | 23 ( 9.1 %)  |         |
|                     | Hispanic         |  3 ( 3.5 %) |  6 ( 7.1 %)         |  3 ( 3.6 %)          | 12 ( 4.7 %)  |         |
|                     | Other            |             |                     |  1 ( 1.2 %)          |  1 ( 0.4 %)  |         |
|                     |                  |             |                     |                      |              |         |
| MMSE                | n                | 86          | 84                  | 84                   | 254          | 0.595   |
|                     | Mean             | 18.0        | 17.9                | 18.5                 | 18.1         |         |
|                     | SD               |  4.27       |  4.22               |  4.16                |  4.21        |         |
|                     | Median           | 19.5        | 18.0                | 20.0                 | 19.0         |         |
|                     | Min              | 10.0        | 10.0                | 10.0                 | 10.0         |         |
|                     | Max              | 23.0        | 24.0                | 24.0                 | 24.0         |         |
|                     |                  |             |                     |                      |              |         |
| Duration of disease | n                | 86          | 84                  | 84                   | 254          | 0.153   |
|                     | Mean             | 42.6        | 48.7                | 40.5                 | 43.9         |         |
|                     | SD               | 30.24       | 29.58               | 24.69                | 28.40        |         |
|                     | Median           | 35.3        | 40.2                | 36.0                 | 36.2         |         |
|                     | Min              |  7.2        |  7.8                |  2.2                 |  2.2         |         |
|                     | Max              | 183.1       | 130.8               | 135.0                | 183.1        |         |
|                     |                  |             |                     |                      |              |         |
|                     | \<12 months      |  5 ( 5.8 %) |  3 ( 3.6 %)         |  4 ( 4.8 %)          | 12 ( 4.7 %)  | 0.789   |
|                     | \>=12 months     | 81 (94.2 %) | 81 (96.4 %)         | 80 (95.2 %)          | 242 (95.3 %) |         |
|                     |                  |             |                     |                      |              |         |
| Years of education  | n                | 86          | 84                  | 84                   | 254          | 0.388   |
|                     | Mean             | 12.6        | 13.2                | 12.5                 | 12.8         |         |
|                     | SD               |  2.95       |  4.15               |  2.92                |  3.38        |         |
|                     | Median           | 12.0        | 12.0                | 12.0                 | 12.0         |         |
|                     | Min              |  6.0        |  3.0                |  6.0                 |  3.0         |         |
|                     | Max              | 21.0        | 24.0                | 20.0                 | 24.0         |         |
|                     |                  |             |                     |                      |              |         |
| Baseline weight(kg) | n                | 86          | 83                  | 84                   | 253          | 0.003   |
|                     | Mean             | 62.8        | 67.3                | 70.0                 | 66.6         |         |
|                     | SD               | 12.77       | 14.12               | 14.65                | 14.13        |         |
|                     | Median           | 60.5        | 64.9                | 69.2                 | 66.7         |         |
|                     | Min              | 34.0        | 45.4                | 41.7                 | 34.0         |         |
|                     | Max              | 86.2        | 106.1               | 108.0                | 108.0        |         |
|                     |                  |             |                     |                      |              |         |
| Baseline height(cm) | n                | 86          | 84                  | 84                   | 254          | 0.126   |
|                     | Mean             | 162.6       | 163.4               | 165.8                | 163.9        |         |
|                     | SD               | 11.52       | 10.42               | 10.13                | 10.76        |         |
|                     | Median           | 162.6       | 162.6               | 165.1                | 162.8        |         |
|                     | Min              | 137.2       | 135.9               | 146.1                | 135.9        |         |
|                     | Max              | 185.4       | 195.6               | 190.5                | 195.6        |         |
|                     |                  |             |                     |                      |              |         |
| Baseline BMI        | n                | 86          | 83                  | 84                   | 253          | 0.013   |
|                     | Mean             | 23.6        | 25.1                | 25.3                 | 24.7         |         |
|                     | SD               |  3.67       |  4.27               |  4.16                |  4.09        |         |
|                     | Median           | 23.4        | 24.3                | 24.8                 | 24.2         |         |
|                     | Min              | 15.1        | 17.7                | 13.7                 | 13.7         |         |
|                     | Max              | 33.3        | 40.1                | 34.5                 | 40.1         |         |
|                     |                  |             |                     |                      |              |         |
|                     | \<25             | 59 (68.6 %) | 47 (56.0 %)         | 44 (52.4 %)          | 150 (59.1 %) | 0.233   |
|                     | 25-\<30          | 21 (24.4 %) | 27 (32.1 %)         | 28 (33.3 %)          | 76 (29.9 %)  |         |
|                     | \>=30            |  6 ( 7.0 %) | 10 (11.9 %)         | 12 (14.3 %)          | 28 (11.0 %)  |         |
|                     |                  |             |                     |                      |              |         |

## AE table

For the adverse events (AE) table, we will use the `data_ae` analysis
results data, which is also based on the CDISC pilot data. This dataset
has two different row label columns, `AEBODSYS` and `AETERM`, for system
organ class and preferred term, respectively. There are also two order
columns which will be used to set the row order of the output. Because
this table has column spanners, we have two column variables, `col2` and
`col1` to define the hierarchy of columns. Finally, there is a param
column and a value column. For brevity, we will subset to AEs with \>10%
prevalence in the High Dose group.

Expand for the code used to produce this subset

``` r
data_ae2 <- data_ae |>
  dplyr::group_by(AEBODSYS, AETERM) |>
  dplyr::mutate(pct_high = value[col2 == "Xanomeline High Dose" & param == "pct"]) |>
  dplyr::ungroup() |>
  dplyr::filter(pct_high > 10) |>
  dplyr::select(-pct_high)
```

    #> # A tibble: 6 × 8
    #>   AEBODSYS        AETERM          col2             col1  param value  ord1  ord2
    #>   <chr>           <chr>           <chr>            <chr> <chr> <dbl> <dbl> <dbl>
    #> 1 ANY BODY SYSTEM ANY BODY SYSTEM Placebo          n_pct n      65       0     0
    #> 2 ANY BODY SYSTEM ANY BODY SYSTEM Placebo          n_pct pct    75.6     0     0
    #> 3 ANY BODY SYSTEM ANY BODY SYSTEM Placebo          AEs   AEs   281       0     0
    #> 4 ANY BODY SYSTEM ANY BODY SYSTEM Xanomeline Low … n_pct n      77       0     0
    #> 5 ANY BODY SYSTEM ANY BODY SYSTEM Xanomeline Low … n_pct pct    91.7     0     0
    #> 6 ANY BODY SYSTEM ANY BODY SYSTEM Xanomeline Low … AEs   AEs   412       0     0

The mock we are going to match looks like this:

[TABLE]

For this table we have three treatment group columns (Placebo, Low, and
High Dose) which each have the following values reported: \# of subjects
with at least one AE (n), percent of subjects with at least one AE
(pct), and \# of AEs (AEs). We also have two p-value columns (Low Dose
vs. Placebo, High Dose vs. Placebo).

Like the demography example, the first thing we are going to do when
building out the `tfrmt` is specify all our columns. Note that `col2`
contains our spanning labels and `col1` contains our lower level column
headers:

``` r
tfrmt(
  # specify columns in the data
  group = AEBODSYS,
  label = AETERM,
  column = c(col2, col1),
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2)
) |>
  print_to_gt(data_ae2) |>
  gt::tab_options(
    container.width = 1000
  )
#> The following rows of the given dataset have no format applied to them 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165
#> Multiple param listed for the same group/label values.
#> The following frmt_structures may be missing from the body_plan
#> or the order may need to be changed:
#> - `frmt_structure(group_val = "ANY BODY SYSTEM", label_val = "ANY BODY SYSTEM", frmt_combine("{n}, {pct}",n = frmt("xx"), pct = frmt("xx")))`
#> - `frmt_structure(group_val = "CARDIAC DISORDERS", label_val = "CARDIAC DISORDERS", frmt_combine("{n}, {pct}",n = frmt("xx"), pct = frmt("xx")))`
#> - `frmt_structure(group_val = "GASTROINTESTINAL DISORDERS", label_val = "GASTROINTESTINAL DISORDERS", frmt_combine("{n}, {pct}",n = frmt("xx"), pct = frmt("xx")))`
#> - `frmt_structure(group_val = "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS", label_val = c("APPLICATION SITE ERYTHEMA","APPLICATION SITE IRRITATION","APPLICATION SITE PRURITUS","GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS"), frmt_combine("{n}, {pct}",n = frmt("xx"), pct = frmt("xx")))`
#> - `frmt_structure(group_val = "INFECTIONS AND INFESTATIONS", label_val = "INFECTIONS AND INFESTATIONS", frmt_combine("{n}, {pct}",n = frmt("xx"), pct = frmt("xx")))`
#> - `frmt_structure(group_val = "NERVOUS SYSTEM DISORDERS", label_val = c("DIZZINESS","NERVOUS SYSTEM DISORDERS"), frmt_combine("{n}, {pct}",n = frmt("xx"), pct = frmt("xx")))`
#> - `frmt_structure(group_val = "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS", label_val = c("RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS"), frmt_combine("{n}, {pct}",n = frmt("xx"), pct = frmt("xx")))`
#> - `frmt_structure(group_val = "SKIN AND SUBCUTANEOUS TISSUE DISORDERS", label_val = c("ERYTHEMA","PRURITUS","RASH","SKIN AND SUBCUTANEOUS TISSUE DISORDERS"), frmt_combine("{n}, {pct}",n = frmt("xx"), pct = frmt("xx")))`
```

[TABLE]

Next, we need to format the values using the `body_plan`. Recall that
our body plan will be made up of a series of `frmt_stucture`s where each
`frmt_stucture` represents the formatting of a cell within the table.
Our AE table boils down to the following values: \# of subjects with at
least one AE (n), percent of subjects with at least one AE (pct), \# of
AEs (AEs), and p-value (pval). Because our n and pct will be combined
using `frmt_combine`, we will have 3 `frmt_structure` objects. Note the
use of `frmt_when` to format the p-values.

``` r
tfrmt(
  # specify columns in the data
  group = AEBODSYS,
  label = AETERM,
  column = c(col2, col1),
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{n} {pct}",
        n = frmt("XXX"),
        pct = frmt_when(
          "==100" ~ "",
          "==0" ~ "",
          TRUE ~ frmt("(xx.x %)")
        )
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      AEs = frmt("[XXX]")
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      pval = frmt_when(
        ">0.99" ~ ">0.99",
        "<0.001" ~ "<0.001",
        "<0.05" ~ frmt("x.xxx*"),
        TRUE ~ frmt("x.xxx", missing = "--")
      )
    )
  )
) |>
  print_to_gt(tfrmt=_, data_ae2) |>
  gt::tab_options(
    container.width = 1000
  )
```

[TABLE]

Almost there! Our AE table contains data for both Preferred Terms and
System Organ Classes. Therefore, we do not want a typical group-level
header. Instead, we want to display the System Organ Class label inline
with its data, and nest the Preferred Term data underneath. Fortunately,
we are able to achieve this formatting with a `row_grp_plan`:

``` r
tfrmt(
  # specify columns in the data
  group = AEBODSYS,
  label = AETERM,
  column = c(col2, col1),
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{n} {pct}",
        n = frmt("XXX"),
        pct = frmt_when(
          "==100" ~ "",
          "==0" ~ "",
          TRUE ~ frmt("(xx.x %)")
        )
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      AEs = frmt("[XXX]")
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      pval = frmt_when(
        ">0.99" ~ ">0.99",
        "<0.001" ~ "<0.001",
        "<0.05" ~ frmt("x.xxx*"),
        TRUE ~ frmt("x.xxx", missing = "--")
      )
    )
  ),
  # Nest Preferred terms under SOC
  row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented"))
) |>
  print_to_gt(data_ae2) |>
  gt::tab_options(
    container.width = 1000
  )
```

[TABLE]

Our column alignment looks good as-is, except for the p-values. We can
use the `col_style_plan` to tweak those.

``` r
tfrmt(
  # specify columns in the data
  group = AEBODSYS,
  label = AETERM,
  column = c(col2, col1),
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{n} {pct}",
        n = frmt("XXX"),
        pct = frmt_when(
          "==100" ~ "",
          "==0" ~ "",
          TRUE ~ frmt("(xx.x %)")
        )
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      AEs = frmt("[XXX]")
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      pval = frmt_when(
        ">0.99" ~ ">0.99",
        "<0.001" ~ "<0.001",
        "<0.05" ~ frmt("x.xxx*"),
        TRUE ~ frmt("x.xxx", missing = "--")
      )
    )
  ),
  # Nest Preferred terms under SOC
  row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented")),
  # alignment

  # Specify column styling plan
  col_style_plan = col_style_plan(
    col_style_structure(align = c(".", ",", " "), col = vars(starts_with("p_")))
  )
) |>
  print_to_gt(data_ae2) |>
  gt::tab_options(
    container.width = 1000
  )
```

[TABLE]

Notice that we still have our order columns and the column labels could
benefit from some renaming. We can add a `col_plan` to help with the
ordering:

``` r
tfrmt(
  # specify columns in the data
  group = AEBODSYS,
  label = AETERM,
  column = c(col2, col1),
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{n} {pct}",
        n = frmt("XXX"),
        pct = frmt_when(
          "==100" ~ "",
          "==0" ~ "",
          TRUE ~ frmt("(xx.x %)")
        )
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      AEs = frmt("[XXX]")
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      pval = frmt_when(
        ">0.99" ~ ">0.99",
        "<0.001" ~ "<0.001",
        "<0.05" ~ frmt("x.xxx*"),
        TRUE ~ frmt("x.xxx", missing = "--")
      )
    )
  ),
  # Nest Preferred terms under SOC
  row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented")),

  # Specify column styling plan
  col_style_plan = col_style_plan(
    col_style_structure(align = c(".", ",", " "), col = vars(p_low, p_high))
  ),
  # columns
  col_plan = col_plan(
    -starts_with("ord")
  )
) |>
  print_to_gt(data_ae2) |>
  gt::tab_options(
    container.width = 1000
  )
```

[TABLE]

For better control over our column labels, we can make use of
`col_plan`’s `span_structure`s to define the column labels and spanners
order and names:

``` r
tfrmt(
  # specify columns in the data
  group = AEBODSYS,
  label = AETERM,
  column = c(col2, col1),
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{n} {pct}",
        n = frmt("XXX"),
        pct = frmt_when(
          "==100" ~ "",
          "==0" ~ "",
          TRUE ~ frmt("(xx.x %)")
        )
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      AEs = frmt("[XXX]")
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      pval = frmt_when(
        ">0.99" ~ ">0.99",
        "<0.001" ~ "<0.001",
        "<0.05" ~ frmt("x.xxx*"),
        TRUE ~ frmt("x.xxx", missing = "--")
      )
    )
  ),
  # Nest Preferred terms under SOC
  row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented")),

  # Specify column styling plan
  col_style_plan = col_style_plan(
    col_style_structure(align = c(".", ",", " "), col = c(p_low, p_high))
  ),

  # columns
  col_plan = col_plan(
    ## defines the spanning column order, and then beneath them the order of their contents
    -starts_with("ord"),
    span_structure(
      col2 = c(
        "Xanomeline High Dose (N=84)" = `Xanomeline High Dose`,
        "Xanomeline Low Dose (N=84)" = `Xanomeline Low Dose`,
        "Placebo (N=86)" = Placebo
      ),
      col1 = c(
        `n (%)` = `n_pct`,
        `[AEs]` = `AEs`
      )
    ),
    span_structure(
      col2 = c("Fisher's Exact p-values" = fisher_pval),
      col1 = c(
        # add a line break to help with table formatting
        `Placebo vs.\n Low Dose` = `p_low`,
        `Placebo vs.\n High Dose` = `p_high`
      )
    )
  )
) |>
  print_to_gt(data_ae2) |>
  gt::tab_options(
    container.width = 1000
  )
```

[TABLE]

Our AE table is now complete!

## Efficacy

For this example, we will use the `data_efficacy` dataset, an example
analysis results dataset found in the package, which is based on the
CDISC pilot data for the ADAS-Cog(11) score. The goal is to recreate
table 14-3.01 from the CDISC pilot.

This data is relatively simple in that it contains only 1 `group` column
and `column` column, but it adds complexity in that multiple analyses
are stacked together - summary statistics of different values at
different time points, and the results of several different ANCOVA
models. Multiple treatment groups as well as the contrasts between
groups are included.

*NOTE:* Pay attention that `label` in the `data_efficacy` must be
matching the `label_val` in the `label_val` for the
[`frmt_combine()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/frmt.md)
to work. For example, for `Mean (SD)`, each variable has different
`param` (`mean` and `sd` respectively), but they have the same `label`
(`Mean (SD`)).

    #> # A tibble: 6 × 7
    #>   group    label     column               param value  ord1  ord2
    #>   <chr>    <chr>     <chr>                <chr> <dbl> <dbl> <dbl>
    #> 1 Baseline n         Placebo              n      79       1     1
    #> 2 Baseline n         Xanomeline Low Dose  n      81       1     1
    #> 3 Baseline n         Xanomeline High Dose n      74       1     1
    #> 4 Baseline Mean (SD) Placebo              mean   24.1     1     2
    #> 5 Baseline Mean (SD) Xanomeline Low Dose  mean   24.4     1     2
    #> 6 Baseline Mean (SD) Xanomeline High Dose mean   21.3     1     2

The mock we are going to match looks like this:

[TABLE]

Let’s first see how the table looks without any special formatting.

``` r
tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  value = value
) |>
  print_to_gt(data_efficacy) |>
  gt::tab_options(
    container.width = 800
  )
#> The following rows of the given dataset have no format applied to them 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70
#> Multiple param listed for the same group/label values.
#> The following frmt_structures may be missing from the body_plan
#> or the order may need to be changed:
#> - `frmt_structure(group_val = "Baseline", label_val = "Median (Range)", frmt_combine("{median}, {min}, {max}",median = frmt("xx"), min = frmt("xx"), max = frmt("xx")))`
#> - `frmt_structure(group_val = "Change from Baseline", label_val = "Median (Range)", frmt_combine("{median}, {min}, {max}",median = frmt("xx"), min = frmt("xx"), max = frmt("xx")))`
#> - `frmt_structure(group_val = "Week 24", label_val = "Median (Range)", frmt_combine("{median}, {min}, {max}",median = frmt("xx"), min = frmt("xx"), max = frmt("xx")))`
#> - `frmt_structure(group_val = "p-value (Xan - Placebo)", label_val = "95% CI", frmt_combine("{diff_lcl}, {diff_ucl}",diff_lcl = frmt("xx"), diff_ucl = frmt("xx")))`
#> - `frmt_structure(group_val = "p-value (Xan - Placebo)", label_val = "Diff of LS Means (SE)", frmt_combine("{diff}, {diff_se}",diff = frmt("xx"), diff_se = frmt("xx")))`
#> - `frmt_structure(group_val = "p-value (Xan High - Xan Low)", label_val = "95% CI", frmt_combine("{diff_lcl}, {diff_ucl}",diff_lcl = frmt("xx"), diff_ucl = frmt("xx")))`
#> - `frmt_structure(group_val = "p-value (Xan High - Xan Low)", label_val = "Diff of LS Means (SE)", frmt_combine("{diff}, {diff_se}",diff = frmt("xx"), diff_se = frmt("xx")))`
```

[TABLE]

Through judicious use of `body_plan`’s `frmt`, `frmt_combine`, and
`frmt_when`, we can conditionally format each of the different pieces of
results. For the summary statistics, we have the number of observations
(n), the mean and standard deviation (mean, sd), and the median and
range (median, min, max). For the models, we have the p-value (p.value)
and the least squares mean difference (diff) as well as its associated
standard error (diff_se) and 95% confidence interval (diff_lcl,
diff_ucl).

The `label` column indicates which row the various measures belong on.
First, let’s format the stand-alone values: n and p-value. Notice that n
always sits on a row labelled “n”; therefore we can reference it by
label_val or param name in the `frmt_structure`. Our p-values have
several different label values so it is more convenient to format them
according to their param name in the `frmt_structure`.

``` r
tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  value = value,
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = "n",
      frmt("xx")
    ), # we could also do: label_val = ".default", n = frmt("xx")
    frmt_structure(
      group_val = ".default", label_val = ".default",
      p.value = frmt_when(
        "<0.001" ~ "<0.001",
        ">0.99" ~ ">0.99",
        TRUE ~ frmt("x.xxx", missing = " ")
      )
    )
  )
) |>
  print_to_gt(data_efficacy) |>
  gt::tab_options(
    container.width = 800
  )
#> The following rows of the given dataset have no format applied to them 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 58, 59, 60, 61, 62, 63, 64, 65, 67, 68, 69, 70
#> Multiple param listed for the same group/label values.
#> The following frmt_structures may be missing from the body_plan
#> or the order may need to be changed:
#> - `frmt_structure(group_val = "Baseline", label_val = "Median (Range)", frmt_combine("{median}, {min}, {max}",median = frmt("xx"), min = frmt("xx"), max = frmt("xx")))`
#> - `frmt_structure(group_val = "Change from Baseline", label_val = "Median (Range)", frmt_combine("{median}, {min}, {max}",median = frmt("xx"), min = frmt("xx"), max = frmt("xx")))`
#> - `frmt_structure(group_val = "Week 24", label_val = "Median (Range)", frmt_combine("{median}, {min}, {max}",median = frmt("xx"), min = frmt("xx"), max = frmt("xx")))`
#> - `frmt_structure(group_val = "p-value (Xan - Placebo)", label_val = "95% CI", frmt_combine("{diff_lcl}, {diff_ucl}",diff_lcl = frmt("xx"), diff_ucl = frmt("xx")))`
#> - `frmt_structure(group_val = "p-value (Xan - Placebo)", label_val = "Diff of LS Means (SE)", frmt_combine("{diff}, {diff_se}",diff = frmt("xx"), diff_se = frmt("xx")))`
#> - `frmt_structure(group_val = "p-value (Xan High - Xan Low)", label_val = "95% CI", frmt_combine("{diff_lcl}, {diff_ucl}",diff_lcl = frmt("xx"), diff_ucl = frmt("xx")))`
#> - `frmt_structure(group_val = "p-value (Xan High - Xan Low)", label_val = "Diff of LS Means (SE)", frmt_combine("{diff}, {diff_se}",diff = frmt("xx"), diff_se = frmt("xx")))`
```

[TABLE]

Next, the remaining param values are combined in twos or threes.
Therefore, we use the `frmt_combine` utility to achieve desired
formatting:

``` r
tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  value = value,
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = "n",
      frmt("xx")
    ), # we could also do: label_val = ".default", n = frmt("xx")
    frmt_structure(
      group_val = ".default", label_val = ".default",
      p.value = frmt_when(
        "<0.001" ~ "<0.001",
        ">0.99" ~ ">0.99",
        TRUE ~ frmt("x.xxx", missing = " ")
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "Median (Range)",
      frmt_combine("{median} ({min};{max})",
        median = frmt("xx.x"),
        min = frmt("xx"),
        max = frmt("xx"), missing = " "
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "Mean (SD)",
      frmt_combine("{mean} ({sd})",
        mean = frmt("xx.x"),
        sd = frmt("xx.xx"), missing = " "
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "Diff of LS Means (SE)",
      frmt_combine("{diff} ({diff_se})",
        diff = frmt("xx.x"),
        diff_se = frmt("xx.xx"), missing = " "
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "95% CI",
      frmt_combine("({diff_lcl};{diff_ucl})",
        diff_lcl = frmt("xx.x"),
        diff_ucl = frmt("xx.x"), missing = " "
      )
    )
  )
) |>
  print_to_gt(data_efficacy) |>
  gt::tab_options(
    container.width = 800
  )
```

[TABLE]

Now that our values are all formatted correctly, we can make sure the
table is sorted appropriately by passing our order columns to
`sorting_cols`. We can also drop these order columns from the final
display using `col_plan`.

``` r
tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = "n",
      frmt("xx")
    ), # we could also do: label_val = ".default", n = frmt("xx")
    frmt_structure(
      group_val = ".default", label_val = ".default",
      p.value = frmt_when(
        "<0.001" ~ "<0.001",
        ">0.99" ~ ">0.99",
        TRUE ~ frmt("x.xxx", missing = " ")
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "Median (Range)",
      frmt_combine("{median} ({min};{max})",
        median = frmt("xx.x"),
        min = frmt("xx"),
        max = frmt("xx"), missing = " "
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "Mean (SD)",
      frmt_combine("{mean} ({sd})",
        mean = frmt("xx.x"),
        sd = frmt("xx.xx"), missing = " "
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "Diff of LS Means (SE)",
      frmt_combine("{diff} ({diff_se})",
        diff = frmt("xx.x"),
        diff_se = frmt("xx.xx"), missing = " "
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "95% CI",
      frmt_combine("({diff_lcl};{diff_ucl})",
        diff_lcl = frmt("xx.x"),
        diff_ucl = frmt("xx.x"), missing = " "
      )
    )
  ),
  col_plan = col_plan(
    group, label, Placebo, contains("Low"), contains("High"), -starts_with("ord")
  )
) |>
  print_to_gt(data_efficacy) |>
  gt::tab_options(
    container.width = 800
  )
```

[TABLE]

Notice that our row labels are not quite right. First, we have a bit of
a hierarchy with the `label` values nested under the `group` values, and
it would be nice to add some indentation to make the nesting more
obvious. Also, in some cases, the `group` values also contain summary
data, which means the ARD contains a matching `group` and `label` value.
For summary rows, we want to suppress the printing of the extra
group-level header, and display the summary data in-line. The
`row_grp_plan` can help us with both via the `row_grp_loc` argument:

``` r
tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = "n",
      frmt("xx")
    ), # we could also do: label_val = ".default", n = frmt("xx")
    frmt_structure(
      group_val = ".default", label_val = ".default",
      p.value = frmt_when(
        "<0.001" ~ "<0.001",
        ">0.99" ~ ">0.99",
        TRUE ~ frmt("x.xxx", missing = " ")
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "Median (Range)",
      frmt_combine("{median} ({min};{max})",
        median = frmt("xx.x"),
        min = frmt("xx"),
        max = frmt("xx"), missing = " "
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "Mean (SD)",
      frmt_combine("{mean} ({sd})",
        mean = frmt("xx.x"),
        sd = frmt("xx.xx"), missing = " "
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "Diff of LS Means (SE)",
      frmt_combine("{diff} ({diff_se})",
        diff = frmt("xx.x"),
        diff_se = frmt("xx.xx"), missing = " "
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "95% CI",
      frmt_combine("({diff_lcl};{diff_ucl})",
        diff_lcl = frmt("xx.x"),
        diff_ucl = frmt("xx.x"), missing = " "
      )
    )
  ),
  col_plan = col_plan(
    group, label, Placebo, contains("Low"), contains("High"), -starts_with("ord")
  ),
  row_grp_plan = row_grp_plan(
    label_loc = element_row_grp_loc(location = "indented")
  )
) |>
  print_to_gt(data_efficacy) |>
  gt::tab_options(
    container.width = 800
  )
```

[TABLE]

Almost done! Notice that the spec also contains empty rows after
different groups of data. We can mimic this behavior by passing
`row_grp_structure` objects in our `row_grp_plan`. These objects define
“blocks” of rows and describe how to format them. In this case, we want
to add a post space after specific blocks of data. We can reference the
locations of each block based on the values of the `group` variable.

``` r
tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = "n",
      frmt("xx")
    ), # we could also do: label_val = ".default", n = frmt("xx")
    frmt_structure(
      group_val = ".default", label_val = ".default",
      p.value = frmt_when(
        "<0.001" ~ "<0.001",
        ">0.99" ~ ">0.99",
        TRUE ~ frmt("x.xxx", missing = " ")
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "Median (Range)",
      frmt_combine("{median} ({min};{max})",
        median = frmt("xx.x"),
        min = frmt("xx"),
        max = frmt("xx"), missing = " "
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "Mean (SD)",
      frmt_combine("{mean} ({sd})",
        mean = frmt("xx.x"),
        sd = frmt("xx.xx"), missing = " "
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "Diff of LS Means (SE)",
      frmt_combine("{diff} ({diff_se})",
        diff = frmt("xx.x"),
        diff_se = frmt("xx.xx"), missing = " "
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = "95% CI",
      frmt_combine("({diff_lcl};{diff_ucl})",
        diff_lcl = frmt("xx.x"),
        diff_ucl = frmt("xx.x"), missing = " "
      )
    )
  ),
  col_plan = col_plan(
    group, label, Placebo, contains("Low"), contains("High"), -starts_with("ord")
  ),
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = list(group = "Change from Baseline"),
      element_block(post_space = " ")
    ),
    row_grp_structure(
      group_val = list(group = "p-value (Dose Response)"),
      element_block(post_space = " ")
    ),
    row_grp_structure(
      group_val = list(group = "p-value (Xan - Placebo)"),
      element_block(post_space = " ")
    ),
    label_loc = element_row_grp_loc(location = "indented")
  )
) |>
  print_to_gt(data_efficacy) |>
  gt::tab_options(
    container.width = 800
  )
```

[TABLE]

There we have it, our efficacy table is complete!
