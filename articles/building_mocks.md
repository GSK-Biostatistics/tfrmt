# Building Mocks

``` r
library(tfrmt)
```

Mock tables are useful for visualising how future generated data may be
presented in its final form.

The [tfrmt](https://GSK-Biostatistics.github.io/tfrmt/) package offers
two methods for creating mock tables:

generating mocks from a tfrmt object with no data

generating mocks from a tfrmt object plus “mock” data (i.e. data that is
representative of your final ARD, except without the `value` column)

While the former is simpler and quicker, the latter offers greater
control over the end result. For either option, placeholders
(e.g. `xx.x`, `XXX`) are used rather than numerical values. Note that
using the lower or upper case is up to your preference.

Tables are generally built by creating a tfrmt specification tailored to
your data, and then feeding that into one of the available print
functions. In this article we will only explore the use of the
[`print_mock_gt()`](https://gsk-biostatistics.github.io/tfrmt/reference/print_mock_gt.md)
function, which is specialised for printing mock tables to
[gt](https://gt.rstudio.com). For information on
[`print_to_gt()`](https://gsk-biostatistics.github.io/tfrmt/reference/print_to_gt.md)
please refer to its documentation.

All tfrmt specifications are created using the
[`tfrmt()`](https://gsk-biostatistics.github.io/tfrmt/reference/tfrmt.md)
function. The foundation of this function are the `label`, `column`,
`param` and `value` parameters. From here you can specify a `body_plan`
where custom `frmt_structures` can be added and built upon.

## Making a simple mock table

In the following example only one format structure has been specified,
where the desired outcome is:

1.  the count value and percent value are combined into the same cell
2.  the count value is formatted to be maximum three digits
3.  the percent value is formatted to be two digits rounded to one
    decimal place

``` r
tfrmt_spec <- tfrmt(
  # Specify columns in the data
  label = label,
  column = c(treatment, column),
  param = param,

  # Specify body plan
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine(
        "{count} {percent}",
        count = frmt("xxx"),
        percent = frmt_when(
          "==100" ~ frmt(""),
          "==0" ~ "",
          "TRUE" ~ frmt("(xx.x%)")
        )
      )
    )
  )
)
```

First, we will explore how to create a mock table using this tfrmt
object alone. The
[`print_mock_gt()`](https://gsk-biostatistics.github.io/tfrmt/reference/print_mock_gt.md)
function has parameters `.default` and `n_cols`; these options inform
the number of rows and columns of the mock table. In this example, we
will use their default values (3) to print three row labels and three
columns.

You may enter different arguments into these parameters to alter the
number of rows and columns shown.

You may notice that a message appears in the console - this is not an
error, it is a reminder to include a `value` parameter to the tfrmt
specification when using the
[`print_to_gt()`](https://gsk-biostatistics.github.io/tfrmt/reference/print_to_gt.md)
function.

``` r
# Print table
print_mock_gt(tfrmt_spec)
#> Message: `tfrmt` will need `value` value to `print_to_gt` when data is avaliable
```

[TABLE]

If you would like to add a little more customisation by supplying your
own mock data you can utilise the
[`crossing()`](https://tidyr.tidyverse.org/reference/expand.html)
function from the [tidyr](https://tidyr.tidyverse.org) package. You may
also wish to add an integer column specifying order layer for complex
tables.

Here is an example:

``` r
df1 <- tidyr::crossing(
  label = c("label 1", "label 2", "label 3"),
  treatment = c("Treatment", "Treatment", "Treatment", "Placebo"),
  column = c("trt1", "trt2", "trt1&trt2", "pl"),
  param = c("count", "percent")
) 
df <- df1 |>
  # Assign numerical order (optional - keep if using `sorting_cols` parameter)
  dplyr::mutate(ord1 = rep(seq(1:length(unique(df1$label))), each = nrow(df1) / length(unique(df1$label))))

df
#> # A tibble: 48 × 5
#>    label   treatment column    param    ord1
#>    <chr>   <chr>     <chr>     <chr>   <int>
#>  1 label 1 Placebo   pl        count       1
#>  2 label 1 Placebo   pl        percent     1
#>  3 label 1 Placebo   trt1      count       1
#>  4 label 1 Placebo   trt1      percent     1
#>  5 label 1 Placebo   trt1&trt2 count       1
#>  6 label 1 Placebo   trt1&trt2 percent     1
#>  7 label 1 Placebo   trt2      count       1
#>  8 label 1 Placebo   trt2      percent     1
#>  9 label 1 Treatment pl        count       1
#> 10 label 1 Treatment pl        percent     1
#> # ℹ 38 more rows
```

This time the `df` data has been added to the
[`print_mock_gt()`](https://gsk-biostatistics.github.io/tfrmt/reference/print_mock_gt.md)
function, so now you can see that the label and column names have
changed.

``` r
# Print table
print_mock_gt(tfrmt_spec, df)
#> Message: `tfrmt` will need `value` value to `print_to_gt` when data is avaliable
```

[TABLE]

You can adjust column names and structure using the `col_plan`
parameter. It accepts selection helpers from the
[tidyselect](https://tidyselect.r-lib.org) package. For example, you can
select
[`everything()`](https://tidyselect.r-lib.org/reference/everything.html)
and remove columns that `-starts_with("ord")`.

You can also use
[`span_structure()`](https://gsk-biostatistics.github.io/tfrmt/reference/col_plan.md)
provide specific information when trying to order across multiple
columns.

``` r
tfrmt_spec <- tfrmt_n_pct(n = "count", pct = "percent") |>
  tfrmt(
    # Specify columns in the data
    label = label,
    column = c(treatment, column),
    param = param,
    value = value,

    # Specify column structure
    col_plan = col_plan(
      -starts_with("ord"),
      span_structure(
        treatment = "Treatment",
        column = c(
          T1 = trt1,
          T2 = trt2,
          `T1&T2` = `trt1&trt2`
        )
      ),
      span_structure(
        treatment = "Placebo",
        column = c(PL = pl)
      )
    )
  )

print_mock_gt(tfrmt_spec, df)
```

[TABLE]

*Note:*
[`tfrmt_n_pct()`](https://gsk-biostatistics.github.io/tfrmt/reference/tfrmt_n_pct.md)
function has been used in place of defining the
[`body_plan()`](https://gsk-biostatistics.github.io/tfrmt/reference/body_plan.md).
This function returns a `tfrmt` with the same the formatting as the
previous example, for more information about layering `tfrmt`s see the
[vignette](https://gsk-biostatistics.github.io/tfrmt/articles/layer.md).

To add a title or subtitle, pass arguments to their respective
parameters.

``` r
tfrmt_spec <- tfrmt_n_pct(n = "count", pct = "percent") |>
  tfrmt(
    # Specify title, subtitle
    title = "Table Name",
    subtitle = "Study ID: GSK12345",

    # Specify columns in the data
    label = label,
    column = c(treatment, column),
    param = param,
    value = value,
    # Specify column structure
    col_plan = col_plan(
      -starts_with("ord"),
      span_structure(
        treatment = "Treatment",
        column = c(
          T1 = trt1,
          T2 = trt2,
          `T1&T2` = `trt1&trt2`
        )
      ),
      span_structure(
        treatment = "Placebo",
        column = c(PL = pl)
      )
    )
  )

print_mock_gt(tfrmt_spec, df)
```

[TABLE]

## Making more complex mock tables

Now let’s make a slightly more complex table where we customise the
value formats by groups, labels and columns. Extra features such as
titles, and arranging column structure will be avoided to keep the code
as simple as possible.

### Different value formatting between columns

Here we have used the
[`crossing()`](https://tidyr.tidyverse.org/reference/expand.html)
function twice with differing columns and bound them using
[`bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).

``` r
df <- dplyr::bind_rows(
  tidyr::crossing(
    label = c("label 1", "label 2", "label 3"),
    column = c("T1", "T2", "PL"),
    param = c("count", "percent")
  ),
  tidyr::crossing(
    label = c("label 1", "label 2", "label 3"),
    column = c("Risk Diff T1-PL", "Risk Diff T2-PL"),
    param = c("num", "lower", "upper")
  )
) |>
  dplyr::arrange_all()

df
#> # A tibble: 36 × 3
#>    label   column          param  
#>    <chr>   <chr>           <chr>  
#>  1 label 1 PL              count  
#>  2 label 1 PL              percent
#>  3 label 1 Risk Diff T1-PL lower  
#>  4 label 1 Risk Diff T1-PL num    
#>  5 label 1 Risk Diff T1-PL upper  
#>  6 label 1 Risk Diff T2-PL lower  
#>  7 label 1 Risk Diff T2-PL num    
#>  8 label 1 Risk Diff T2-PL upper  
#>  9 label 1 T1              count  
#> 10 label 1 T1              percent
#> # ℹ 26 more rows
```

This tfrmt specification is the same as the previous example but has one
additional `frmt_structure` which accounts for the value formatting we’d
like to apply to the the new columns.

``` r
tfrmt_n_pct(n = "count", pct = "percent") |>
  tfrmt(
    # Specify columns in the data
    label = "label",
    param = "param",
    column = "column",
    value = value,

    # Specify body plan
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default", label_val = ".default",
        frmt_combine(
          "{num} ({lower}, {upper})",
          num = frmt("xx.x"),
          lower = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("xx.x%")
          ),
          upper = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("xx.x%")
          )
        )
      )
    ),
    col_plan = col_plan(
      -starts_with("ord")
    )
  ) |>
  print_mock_gt(df)
```

|         |    PL     |   Risk Diff T1-PL   |   Risk Diff T2-PL   |    T1     |    T2     |
|---------|:---------:|:-------------------:|:-------------------:|:---------:|:---------:|
| label 1 | x (xx.x%) | xx.x (xx.x%, xx.x%) | xx.x (xx.x%, xx.x%) | x (xx.x%) | x (xx.x%) |
| label 2 | x (xx.x%) | xx.x (xx.x%, xx.x%) | xx.x (xx.x%, xx.x%) | x (xx.x%) | x (xx.x%) |
| label 3 | x (xx.x%) | xx.x (xx.x%, xx.x%) | xx.x (xx.x%, xx.x%) | x (xx.x%) | x (xx.x%) |

Note that if you’d like to print a select few columns from your dataset
you can simply filter `df` as you normally would within the
[`print_mock_gt()`](https://gsk-biostatistics.github.io/tfrmt/reference/print_mock_gt.md)
function. For example,
`df |> filter(!stringr::str_detect(column, "^Risk Diff")` could be used
to remove values that start with “Risk Diff”.

Alternatively, you can make use of
[`col_plan()`](https://gsk-biostatistics.github.io/tfrmt/reference/col_plan.md)
and directly specify the columns you would and wouldn’t like to display.
Here we have decided to select only the placebo and treatment columns.

``` r
tfrmt_n_pct(n = "count", pct = "percent") |>
  tfrmt(
    # Specify columns in the data
    label = "label",
    param = "param",
    column = "column",
    value = value,

    # Specify body plan
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default", label_val = ".default",
        frmt_combine(
          "{num} ({lower}, {upper})",
          num = frmt("xx.x"),
          lower = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("xx.x%")
          ),
          upper = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("xx.x%")
          )
        )
      )
    ),
    col_plan = col_plan(
      label, PL, T1, T2
    )
  ) |>
  print_mock_gt(df)
```

|         |    PL     |    T1     |    T2     |   Risk Diff T1-PL   |   Risk Diff T2-PL   |
|---------|:---------:|:---------:|:---------:|:-------------------:|:-------------------:|
| label 1 | x (xx.x%) | x (xx.x%) | x (xx.x%) | xx.x (xx.x%, xx.x%) | xx.x (xx.x%, xx.x%) |
| label 2 | x (xx.x%) | x (xx.x%) | x (xx.x%) | xx.x (xx.x%, xx.x%) | xx.x (xx.x%, xx.x%) |
| label 3 | x (xx.x%) | x (xx.x%) | x (xx.x%) | xx.x (xx.x%, xx.x%) | xx.x (xx.x%, xx.x%) |

### Same value formatting but 1 grouping level

In this example you can see that the dataset contains one group variable
(inside
[`crossing()`](https://tidyr.tidyverse.org/reference/expand.html)).
Hence, we account for this by adding the name of the group column into
the `group` parameter for
[`tfrmt()`](https://gsk-biostatistics.github.io/tfrmt/reference/tfrmt.md).

``` r
df <- tidyr::crossing(
  group = c("group 1", "group 2"),
  label = c("label 1", "label 2"),
  column = c("PL", "T1", "T2"),
  param = c("count", "percent")
) |>
  dplyr::arrange_all()

tfrmt_n_pct(n = "count", pct = "percent") |>
  tfrmt(
    group = group,
    label = label,
    column = column,
    param = param,
    value = value,
    row_grp_plan = row_grp_plan(
      row_grp_structure(
        group_val = ".default",
        element_block(post_space = "   ")
      )
    )
  ) |>
  print_mock_gt(df)
```

[TABLE]

### Different value formatting but 1 grouping level (using label_val)

This time we want to be more specific and change the formatting of
values by row label. Given that each label possesses different parameter
values in this example, we bind together multiple instances of
[`crossing()`](https://tidyr.tidyverse.org/reference/expand.html) with
these different label-parameter combinations, while keeping the rest the
same.

In the tfrmt specification we leverage the `label_val` parameter in
`frmt_structure` to specify the particular formatting by label. Usually,
if the default argument `.default` is kept then the function will
attempt to logically structure the table as best as it can.

You may notice “n”, “Mean (SD)” and “Median (Range)” has been used
instead of generic “label \#” names - this is just an illustration of
what these row labels could be.

``` r
df <- dplyr::bind_rows(
  tidyr::crossing(
    group = c("group 1", "group 2"),
    label = c("n"),
    column = c("PL", "T1", "T2"),
    param = c("count")
  ),
  tidyr::crossing(
    group = c("group 1", "group 2"),
    label = c("Mean (SD)"),
    column = c("PL", "T1", "T2"),
    param = c("mean", "sd")
  ),
  tidyr::crossing(
    group = c("group 1", "group 2"),
    label = c("Median (Range)"),
    column = c("PL", "T1", "T2"),
    param = c("min", "max", "median")
  )
) |>
  dplyr::arrange_all()

tfrmt(
  # Specify columns in the data
  group = group,
  label = label,
  column = column,
  param = param,
  value = value,
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = ".default",
      element_block(post_space = "   ")
    )
  ),

  # Specify body plan
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = "n", frmt("xx")),
    frmt_structure(
      group_val = ".default", label_val = "Median (Range)",
      frmt_combine("{median} ({min},{max})",
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
    )
  )
) |>
  print_mock_gt(df)
```

[TABLE]

### Different value formatting but 1 grouping level (using group_val)

Similarly, if we want to change the value formatting by group we can use
the `group_val` parameter in `frmt_structure`.

Note that the `row_group_plan` parameter has been used to detail how
we’d like the groups to be indented. For more information on this please
see the “Row group plan” article.

``` r
df <- dplyr::bind_rows(
  tidyr::crossing(
    group = c("group 1"),
    label = c("label 1", "label 2"),
    column = c("PL", "T1", "T2"),
    param = c("mean", "sd")
  ),
  tidyr::crossing(
    group = c("group 2"),
    label = c("label 1", "label 2"),
    column = c("PL", "T1", "T2"),
    param = c("median", "min", "max")
  )
) |>
  dplyr::arrange_all()

tfrmt(
  # Specify columns in the data
  group = group,
  label = label,
  column = column,
  param = param,
  value = value,
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = list("group" = c("group 1", "group 2")),
      element_block(post_space = "   ")
    )
  ),

  # Specify body plan
  body_plan = body_plan(
    frmt_structure(
      group_val = "group 1", label_val = ".default",
      frmt_combine("{mean} ({sd})",
        mean = frmt("xx.x"),
        sd = frmt("xx.x")
      )
    ),
    frmt_structure(
      group_val = "group 2", label_val = ".default",
      frmt_combine("{median} ({min},{max})",
        median = frmt("xx"),
        min = frmt("xx"),
        max = frmt("xx"), missing = " "
      )
    )
  )
) |>
  print_mock_gt(df)
```

[TABLE]

### Different value formatting but 1 grouping level (using label_val and group_val)

Using the same `df` as before, we can make use of `group_val` and
`label_val` simultaneously for more control over value formatting by
group and label/row. You can see that there is a different formatting
for labels between groups as well as within the same group.

Within the `row_group_structure`, if you’d like to depict formatting for
more than one group you need to supply a named list to `group_val`.
Furthermore, to apply the formatting to more than one label you can
supply the label names within [`c()`](https://rdrr.io/r/base/c.html).

``` r
tfrmt(
  # Specify columns in the data
  group = group,
  label = label,
  column = column,
  param = param,
  value = value,
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = list("group" = c("group 1", "group 2")),
      element_block(post_space = "   ")
    )
  ),

  # Specify body plan
  body_plan = body_plan(
    frmt_structure(
      group_val = "group 1", label_val = "label 1",
      frmt_combine("{mean} ({sd})",
        mean = frmt("xx"),
        sd = frmt("xx")
      )
    ),
    frmt_structure(
      group_val = "group 1", label_val = "label 2",
      frmt_combine("{mean} ({sd})",
        mean = frmt("xx.x"),
        sd = frmt("xx.x")
      )
    ),
    frmt_structure(
      group_val = "group 2", label_val = c("label 1", "label 2"),
      frmt_combine("{median} ({min},{max})",
        median = frmt("xx"),
        min = frmt("xx"),
        max = frmt("xx"), missing = " "
      )
    )
  )
) |>
  print_mock_gt(df)
```

[TABLE]

### Different value formatting but 2 grouping levels (using group_val)

This time we have two grouping levels where parameters differ at the
group level. Thus, we have two instances of
[`crossing()`](https://tidyr.tidyverse.org/reference/expand.html) with
different grp1-param combinations.

When using more than one grouping level you add the group names to the
`group` parameter in [`c()`](https://rdrr.io/r/base/c.html).

``` r
df <- dplyr::bind_rows(
  tidyr::crossing(
    grp1 = c("group 1.1"),
    grp2 = c("group 2.1", "group 2.2"),
    label = c("label 1", "label 2"),
    column = c("PL", "T1", "T2"),
    param = c("count")
  ),
  tidyr::crossing(
    grp1 = c("group 1.2"),
    grp2 = c("group 2.1", "group 2.2"),
    label = c("label 1", "label 2"),
    column = c("PL", "T1", "T2"),
    param = c("mean", "sd")
  )
) |>
  dplyr::arrange_all()

tfrmt(
  group = c(grp1, grp2),
  label = label,
  column = column,
  param = param,
  value = value,
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = ".default",
      element_block(post_space = "   ")
    )
  ),
  body_plan = body_plan(
    frmt_structure(
      group_val = list(
        "grp1" = "group 1.1",
        "grp2" = c("group 2.1", "group 2.2")
      ), label_val = ".default",
      frmt("xx")
    ),
    frmt_structure(
      group_val = list(
        "grp1" = "group 1.2",
        "grp2" = c("group 2.1", "group 2.2")
      ), label_val = ".default",
      frmt_combine("{mean} ({sd})",
        mean = frmt("xx.x"),
        sd = frmt("xx.x")
      )
    )
  )
) |>
  print_mock_gt(df)
```

[TABLE]

------------------------------------------------------------------------
