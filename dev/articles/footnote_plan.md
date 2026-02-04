# Footnote Plan

``` r
library(tfrmt)
```

## Description

In `tfrmt` footnotes are a table component which can be added to the
specification in a manner similar to `body_plan` and `col_plan`. It has
its own dedicated function called `footnote_plan` which consists of a
list of `footnote_structure`s.

Some options for the marks used to indicate footnotes are the following:

- letters (letters: a, b, c, etc.)
- numbers (numbers: 1, 2, 3, etc.)
- standard (set of 4 distinct symbols)
- extended (set of 6 distinct symbols)

## Examples

Let’s go through some examples!

Here is some mock data we’re going to use.

``` r
# Create mock data
df <- tidyr::crossing(
  group = c("group 1", "group 2"),
  label = c("label 1", "label 2"),
  column = c("PL", "T1", "T2", "T1&T2"),
  param = c("count", "percent")
)

# This one is used for examples 5 and 6
span_df <- df |> dplyr::mutate(span = dplyr::case_when(
  column == "PL" ~ "Placebo",
  column %in% c("T1", "T2", "T1&T2") == TRUE ~ "Treatment"
))
```

### Example 1 (one footnote per group/column)

Different footnotes are separated by individual `footnote_structure`s.

``` r
# Create specification
tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = ".default",
      element_block(post_space = "   ")
    )
  ),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{count} ({percent})",
        count = frmt("xx"),
        percent = frmt("xx.x")
      )
    )
  ),

  # Add footnotes here
  footnote_plan = footnote_plan(
    footnote_structure(footnote_text = "Source Note", group_val = "group 1"),
    footnote_structure(footnote_text = "Placebo", column_val = "PL"),
    marks = "standard"
  ),
) |>
  print_mock_gt(df)
#> Message: `tfrmt` will need `value` value to `print_to_gt` when data is available
```

[TABLE]

### Example 2 (same footnote for multiple columns)

When applying a footnote to multiple columns you must enter a named list
where the LHS is the name of the column variable in your input data
(“column” in this example) containing column header names for the output
table, and the RHS is the column header name you have selected (“T1”,
“T2” or “T1&T2” in this case).

``` r
# Create specification
tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = ".default",
      element_block(post_space = "   ")
    )
  ),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{count} ({percent})",
        count = frmt("xx"),
        percent = frmt("xx.x")
      )
    )
  ),

  # Add footnotes here
  footnote_plan = footnote_plan(
    footnote_structure(footnote_text = "All Treatments", list(column = c("T1", "T2", "T1&T2"))),
    marks = "numbers"
  ),
) |>
  print_mock_gt(df)
#> Message: `tfrmt` will need `value` value to `print_to_gt` when data is available
```

[TABLE]

### Example 3 (footnote for same label in all groups)

``` r
# Create specification
tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = ".default",
      element_block(post_space = "   ")
    )
  ),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{count} ({percent})",
        count = frmt("xx"),
        percent = frmt("xx.x")
      )
    )
  ),

  # Add footnotes here
  footnote_plan = footnote_plan(
    footnote_structure(footnote_text = "Footnote goes here", label_val = "label 1"),
    marks = "standard"
  ),
) |>
  print_mock_gt(df)
#> Message: `tfrmt` will need `value` value to `print_to_gt` when data is available
```

[TABLE]

### Example 4 (footnote for specific label in one group)

``` r
# Create specification
tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = ".default",
      element_block(post_space = "   ")
    )
  ),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{count} ({percent})",
        count = frmt("xx"),
        percent = frmt("xx.x")
      )
    )
  ),

  # Add footnotes here
  footnote_plan = footnote_plan(
    footnote_structure(footnote_text = "Footnote goes here", group_val = "group 1", label_val = "label 1"),
    marks = "standard"
  ),
) |>
  print_mock_gt(df)
#> Message: `tfrmt` will need `value` value to `print_to_gt` when data is available
```

[TABLE]

### Example 5 (footnote for spanning header)

``` r
# Add specification
tfrmt(
  group = group,
  label = label,
  column = c("span", "column"),
  param = param,
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = ".default",
      element_block(post_space = "   ")
    )
  ),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{count} ({percent})",
        count = frmt("xx"),
        percent = frmt("xx.x")
      )
    )
  ),
  col_plan = col_plan(
    group, label,
    span_structure(
      span = c("Placebo"),
      column = c("PL")
    ),
    span_structure(
      span = c("Treatment"),
      column = c("T1", "T2", "T1&T2")
    )
  ),

  # Add footnote here
  footnote_plan = footnote_plan(
    footnote_structure(footnote_text = "Footnote goes here", column_val = list(span = "Treatment")),
    marks = "extended"
  )
) |>
  print_mock_gt(span_df)
#> Message: `tfrmt` will need `value` value to `print_to_gt` when data is available
```

[TABLE]

### Example 6 (footnote for column header/s under spanning header)

You can apply a mark to multiple headers by entering a named list for
your column variable.

``` r
# Add specification
tfrmt(
  group = group,
  label = label,
  column = c("span", "column"),
  param = param,
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = ".default",
      element_block(post_space = "   ")
    )
  ),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{count} ({percent})",
        count = frmt("xx"),
        percent = frmt("xx.x")
      )
    )
  ),
  col_plan = col_plan(
    group, label,
    span_structure(
      span = c("Placebo"),
      column = c("PL")
    ),
    span_structure(
      span = c("Treatment"),
      column = c("T1", "T2", "T1&T2")
    )
  ),

  # Add footnote here
  footnote_plan = footnote_plan(
    footnote_structure(footnote_text = "Footnote goes here", column_val = list(span = "Treatment", column = "T1&T2")),
    marks = "extended"
  )
) |>
  print_mock_gt(span_df)
#> Message: `tfrmt` will need `value` value to `print_to_gt` when data is available
```

[TABLE]
