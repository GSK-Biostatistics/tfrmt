# Extract Data from a tfrmt

``` r

library(tfrmt)
library(dplyr)
library(gt)
```

The [tfrmt](https://GSK-Biostatistics.github.io/tfrmt/) package is
designed to create highly formatted Tables by layering formatting logic
onto data. While the primary output of
[`print_to_gt()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/print_to_gt.md)
is a `gt` object ready for reporting, there are times when you may need
to access the “final” processed data frame—for example, to perform
secondary validations, to export the data, or use in another reporting
tool.

The
[`extract_data()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/extract_data.md)
function provides a clean way to pull this underlying data back out of a
`gt_tbl` or `gt_group` object.
[`extract_data()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/extract_data.md)
handles the cleanup of the data, removing internal columns and ensuring
that column names reflect the labels seen in the final table. The
extracted data frame contains the formatted character strings as seen in
the table.

## Basic Usage

In this example, we take a demographic dataset, apply a tfrmt, and then
extract the resulting data frame.

``` r

# Subset data for a simple example
data_demog_test <- data_demog |>
  filter(
    rowlbl1 %in% c("Age (y)", "Sex"),
    column != "p-value"
  )

# Define and print the tfrmt
tfrmt_obj <- tfrmt(
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
    frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx"))
  ),
  # remove extra cols
  col_plan = col_plan(
    -grp,
    -ord1,
    -ord2
  )
)

gt_table <- print_to_gt(tfrmt_obj, data_demog_test)

gt_table
```

[TABLE]

``` r


# Extract the data
extracted_df <- extract_data(gt_table)

extracted_df
#> # A tibble: 14 × 5
#>    rowlbl2       Placebo      `Xanomeline Low Dose` `Xanomeline High Dose` Total
#>    <chr>         <chr>        <chr>                 <chr>                  <chr>
#>  1 "Age (y)"      NA           NA                    NA                     NA  
#>  2 "  n"         " 86"        " 84"                 " 84"                  "254"
#>  3 "  Mean"      " 75.2"      " 75.7"               " 74.4"                " 75…
#>  4 "  SD"        "  8.59"     "  8.29"              "  7.89"               "  8…
#>  5 "  Median"    " 76.0"      " 77.5"               " 76.0"                " 77…
#>  6 "  Min"       " 52.0"      " 51.0"               " 56.0"                " 51…
#>  7 "  Max"       " 89.0"      " 88.0"               " 88.0"                " 89…
#>  8 "  <65 yrs"   " 14 (16.3 … "  8 ( 9.5 %)"        " 11 (13.1 %)"         " 33…
#>  9 "  65-80 yrs" " 42 (48.8 … " 47 (56.0 %)"        " 55 (65.5 %)"         "144…
#> 10 "  >80 yrs"   " 30 (34.9 … " 29 (34.5 %)"        " 18 (21.4 %)"         " 77…
#> 11 "Sex"          NA           NA                    NA                     NA  
#> 12 "  n"         " 86"        " 84"                 " 84"                  "254"
#> 13 "  Male"      " 33 (38.4 … " 34 (40.5 %)"        " 44 (52.4 %)"         "111…
#> 14 "  Female"    " 53 (61.6 … " 50 (59.5 %)"        " 40 (47.6 %)"         "143…
```

## Extracting from Paged Tables (`gt_group`)

When using a `page_plan`,
[`print_to_gt()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/print_to_gt.md)
returns a `gt_group` object containing multiple tables.
[`extract_data()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/extract_data.md)
automatically detects this and returns a list of data frames, one for
each page.

In this example we add pagination to our table using
[`page_plan()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/page_plan.md)
and extract the data from the `gt_group` object using
[`extract_data()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/extract_data.md).

``` r

tfrmt_paged <- tfrmt_obj |>
  tfrmt(
    page_plan = page_plan(
      page_structure(
        group_val = list(
          rowlbl1 = ".default"
        )
      ),
      note_loc = "source_note"
    )
  )

gt_paged <- print_to_gt(tfrmt_paged, data_demog_test)

gt_paged
```

[TABLE]

[TABLE]

``` r


# This returns a list of data frames
data_list <- extract_data(gt_paged)

data_list
#> [[1]]
#> # A tibble: 10 × 5
#>    rowlbl2       Placebo      `Xanomeline Low Dose` `Xanomeline High Dose` Total
#>    <chr>         <chr>        <chr>                 <chr>                  <chr>
#>  1 "Age (y)"      NA           NA                    NA                     NA  
#>  2 "  n"         " 86"        " 84"                 " 84"                  "254"
#>  3 "  Mean"      " 75.2"      " 75.7"               " 74.4"                " 75…
#>  4 "  SD"        "  8.59"     "  8.29"              "  7.89"               "  8…
#>  5 "  Median"    " 76.0"      " 77.5"               " 76.0"                " 77…
#>  6 "  Min"       " 52.0"      " 51.0"               " 56.0"                " 51…
#>  7 "  Max"       " 89.0"      " 88.0"               " 88.0"                " 89…
#>  8 "  <65 yrs"   " 14 (16.3 … "  8 ( 9.5 %)"        " 11 (13.1 %)"         " 33…
#>  9 "  65-80 yrs" " 42 (48.8 … " 47 (56.0 %)"        " 55 (65.5 %)"         "144…
#> 10 "  >80 yrs"   " 30 (34.9 … " 29 (34.5 %)"        " 18 (21.4 %)"         " 77…
#> 
#> [[2]]
#> # A tibble: 4 × 5
#>   rowlbl2    Placebo        `Xanomeline Low Dose` `Xanomeline High Dose` Total  
#>   <chr>      <chr>          <chr>                 <chr>                  <chr>  
#> 1 "Sex"       NA             NA                    NA                    NA     
#> 2 "  n"      " 86"          " 84"                 " 84"                  254    
#> 3 "  Male"   " 33 (38.4 %)" " 34 (40.5 %)"        " 44 (52.4 %)"         111 (4…
#> 4 "  Female" " 53 (61.6 %)" " 50 (59.5 %)"        " 40 (47.6 %)"         143 (5…
```

## Integration with Big N

[`extract_data()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/extract_data.md)
is also compatible with `big_n_structure`. If your table includes Big N
values in the column headers, these formatted strings will be preserved
as the column names in the extracted data frame.

Example showing big Ns preserved in the column names after using
[`extract_data()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/extract_data.md).

``` r

data <- tibble::tibble(
  Group = c("N", "N", "N", rep(c("Age (y)", "Sex", "Age (y)", "Sex"), c(3, 3, 6, 12))),
  Label = c("N", "N", "N", rep(c("n", "Mean (SD)", "Male", "Female"), c(6, 6, 6, 6))),
  Column = c("Placebo", "Treatment", "Total", rep(c("Placebo", "Treatment", "Total"), times = 8)),
  Param = c("bigN", "bigN", "bigN", rep(c("n", "mean", "sd", "n", "pct", "n", "pct"), c(6, 3, 3, 3, 3, 3, 3))),
  Value = c(
    30, 40, 60, 15, 13, 28, 14, 13, 27, 73.56, 74.231, 71.84, 9.347, 7.234, 8.293,
    8, 7, 15, 8 / 14, 7 / 13, 15 / 27, 6, 6, 12, 6 / 14, 6 / 13, 12 / 27
  )
) |>
  dplyr::mutate(
    Value = dplyr::case_when(
      Param == "pct" ~ Value * 100,
      TRUE ~ Value
    ),
    ord1 = dplyr::if_else(Param == "bigN", 0, 1),
    ord2 = dplyr::if_else(Param == "bigN", 0, 1)
  )


bign <- tfrmt(
  group = Group,
  label = Label,
  column = Column,
  value = Value,
  param = Param,
  sorting_cols = c(ord1, ord2),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      frmt_combine("{n} {pct}",
        n = frmt("X"),
        pct = frmt("(xx.x%)", missing = " ")
      )
    ),
    frmt_structure(
      group_val = "Age (y)", label_val = "Mean (SD)",
      frmt_combine("{mean} ({sd})",
        mean = frmt("XX.X"),
        sd = frmt("x.xx")
      )
    ),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xx"))
  ),
  col_plan = col_plan(everything(), -starts_with("ord"), "Total"),
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = " "))
  ),
  big_n = big_n_structure(param_val = "bigN", n_frmt = frmt("\nN = xx"))
) |>
  print_to_gt(data)

bign
```

[TABLE]

``` r


extracted <- extract_data(bign)

extracted
#> # A tibble: 10 × 4
#>    Label         `Placebo\nN = 30` `Treatment\nN = 40` `Total\nN = 60`
#>    <chr>         <chr>             <chr>               <chr>          
#>  1 "Age (y)"      NA                NA                  NA            
#>  2 "  n"         "15"              "13"                "28"           
#>  3 "Sex"          NA                NA                  NA            
#>  4 "  n"         "14"              "13"                "27"           
#>  5 "Age (y)"      NA                NA                  NA            
#>  6 "  Mean (SD)" "73.6 (9.35)"     "74.2 (7.23)"       "71.8 (8.29)"  
#>  7 "   "         " "               " "                 " "            
#>  8 "Sex"          NA                NA                  NA            
#>  9 "  Male"      "8 (57.1%)"       "7 (53.8%)"         "15 (55.6%)"   
#> 10 "  Female"    "6 (42.9%)"       "6 (46.2%)"         "12 (44.4%)"
```

## Handling Spanning Headers

If your table has multiple levels of column headers, `gt` stores these
internally using a delimiter.

By default,
[`extract_data()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/extract_data.md)
will collapse these levels using an underscore (`_`), but you can
specify a custom delimiter using the `col_delim` argument.

Example showing how spanning headers are combined using `col_delim`
argument in
[`extract_data()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/extract_data.md).

``` r

data <- tibble::tribble(
  ~group, ~label, ~span2, ~span1, ~my_col, ~parm, ~val,
  "g1", "rowlabel1", "column cols", "cols 1,2", "col1", "value", 1,
  "g1", "rowlabel1", "column cols", "cols 1,2", "col2", "value", 1,
  "g1", "rowlabel1", NA, NA, "mycol3", "value", 1,
  "g1", "rowlabel1", "column cols", "col 4", "col4", "value", 1,
  "g1", "rowlabel1", NA, NA, "mycol5", "value", 1,
  "g1", "rowlabel2", "column cols", "cols 1,2", "col1", "value", 2,
  "g1", "rowlabel2", "column cols", "cols 1,2", "col2", "value", 2,
  "g1", "rowlabel2", NA, NA, "mycol3", "value", 2,
  "g1", "rowlabel2", "column cols", "col 4", "col4", "value", 2,
  "g1", "rowlabel2", NA, NA, "mycol5", "value", 2,
  "g2", "rowlabel3", "column cols", "cols 1,2", "col1", "value", 3,
  "g2", "rowlabel3", "column cols", "cols 1,2", "col2", "value", 3,
  "g2", "rowlabel3", NA, NA, "mycol3", "value", 3,
  "g2", "rowlabel3", "column cols", "col 4", "col4", "value", 3,
  "g2", "rowlabel3", NA, NA, "mycol5", "value", 3,
)

# 2 layers of spanning headers
spanning_tfrmt <- tfrmt(
  group = group,
  label = label,
  param = parm,
  value = val,
  column = c(span2, span1, my_col),
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt("x"))
  ),
  col_plan = col_plan(
    group,
    label,
    starts_with("col")
  )
) |> print_to_gt(data)

spanning_tfrmt
```

[TABLE]

``` r


res_layer <- extract_data(spanning_tfrmt, col_delim = "/")
res_layer
#> # A tibble: 5 × 6
#>   label     column cols/cols 1,2…¹ column cols/cols 1,2…² column cols/col 4/co…³
#>   <chr>     <chr>                  <chr>                  <chr>                 
#> 1 "g1"      NA                     NA                     NA                    
#> 2 "  rowla… 1                      1                      1                     
#> 3 "  rowla… 2                      2                      2                     
#> 4 "g2"      NA                     NA                     NA                    
#> 5 "  rowla… 3                      3                      3                     
#> # ℹ abbreviated names: ¹​`column cols/cols 1,2/col1`,
#> #   ²​`column cols/cols 1,2/col2`, ³​`column cols/col 4/col4`
#> # ℹ 2 more variables: mycol3 <chr>, mycol5 <chr>
```
