# Table Format

tfrmt, or "table format" is a way to pre-define the non-data components
of your tables, and how the data will be handled once added: i.e. title,
footers, headers, span headers, and cell formats. In addition, tfrmt's
can be layered, building from one table format to the next. For cases
where only one value can be used, the newly defined tfrmt accepts the
latest tfrmt

## Usage

``` r
tfrmt(
  tfrmt_obj,
  group = vars(),
  label = quo(),
  param = quo(),
  value = quo(),
  column = vars(),
  title,
  subtitle,
  row_grp_plan,
  body_plan,
  col_style_plan,
  col_plan,
  sorting_cols,
  big_n,
  footnote_plan,
  page_plan,
  ...
)
```

## Arguments

- tfrmt_obj:

  a tfrmt object to base this new format off of

- group:

  what are the grouping vars of the input dataset

- label:

  what is the label column of the input dataset

- param:

  what is the param column of the input dataset

- value:

  what is the value column of the input dataset

- column:

  what is the column names column in the input dataset

- title:

  title of the table

- subtitle:

  subtitle of the table

- row_grp_plan:

  plan of the row groups blocking. Takes a
  [`row_grp_plan()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/row_grp_plan.md)

- body_plan:

  combination and formatting of the input data. Takes a
  [`body_plan()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/body_plan.md)

- col_style_plan:

  how to style columns including alignment (left, right, character) and
  width. Takes a
  [`col_style_plan()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/col_style_plan.md)

- col_plan:

  a col_plan object which is used to select, rename, and nest columns.
  Takes a
  [`col_plan()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/col_plan.md)

- sorting_cols:

  which columns determine sorting of output

- big_n:

  how to format subject totals ("big Ns") for inclusion in the column
  labels. Takes a
  [`big_n_structure()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/big_n_structure.md)

- footnote_plan:

  footnotes to be added to the table. Takes a
  [`footnote_plan()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/footnote_plan.md)

- page_plan:

  pagination splits to be applied to the table. Takes a
  [`page_plan()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/page_plan.md)

- ...:

  These dots are for future extensions and must be empty.

## Value

tfrmt object

## Details

### NSE and Argument Evaluation

- tfrmt allows users to pass `vars`, `quo`, and unquoted expressions to
  a variety of arguments, such as `group`, `label`, `param`, `value`,
  `column`, and `sorting_cols`. Users accustomed to tidyverse semantics
  should be familiar with this behaviour. However, there is an important
  behaviour difference between tfrmt and normal tidyverse functions.
  Because the data are not a part of tfrmt, it does not know when a
  value being passed to it is intended to be an unquoted expression
  representing a column name or an object from the environment. As such,
  it preferentially uses the value from the environment over preserving
  the entry as an expression. For example, if you have an object
  "my_object" in your environment with the value "Hello world", and try
  to create a tfrmt as `tfrmt(column = my_object)`, it will take the
  value of "my_object" over assuming the column argument is an unquoted
  expression and view the entry to `column` as "Hello World". To pass
  "my_object" to tfrmt as a column name, use quotes around the value:
  `tfrmt(columnn = "my_object")`.

- Additionally, unquoted expressions that match `tfrmt`'s other argument
  names can cause unexpected results. It is recommended to put quotes
  around the value as such: `tfrmt(label = "group")`. In this case, the
  quoting will prevent `tfrmt` from assigning its `group` input value to
  the `label` value.

## Images

Here are some example outputs: ![Example showing no groups, one group
and two
groups](https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/main/images/tfrmt-groups-three-cols-cropped.jpg)

## See also

[Link to related
article](https://gsk-biostatistics.github.io/tfrmt/articles/building_blocks.html)

## Examples

``` r
tfrmt_spec <- tfrmt(
  label = label,
  column = column,
  param = param,
  value=value)

tfrmt_spec <- tfrmt(
  label = label,
  column = column,
  param = param,
  value=value,
# Set the formatting for values
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      frmt_combine("{n} {pct}",
           n = frmt("xxx"),
           pct = frmt_when(
                "==100" ~ "(100%)",
                "==0" ~ "",
                TRUE ~ frmt("(xx.x %)")
                )
           )
    )
  ),
# Specify column styling plan
  col_style_plan = col_style_plan(
    col_style_structure(col = vars(everything()), align = c(".",","," "))
  ))

tfrmt_spec <- tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  value=value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      frmt_combine("{n} {pct}",
           n = frmt("xxx"),
           pct = frmt_when(
                "==100" ~ "(100%)",
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
        label_val = c("Mean", "Median", "Min","Max"),
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
        label_val = c("n","<65 yrs","<12 months","<25"),
        p = frmt_when(
            ">0.99" ~ ">0.99",
            "<0.001" ~ "<0.001",
            TRUE ~ frmt("x.xxx", missing = "")
        )
    )
  ),
  # remove extra cols
  col_plan = col_plan(-grp,
                      -starts_with("ord") ),
  # Specify column styling plan
  col_style_plan = col_style_plan(
    col_style_structure(col = vars(everything()), align = c(".",","," "))
  ),

  # Specify row group plan
  row_grp_plan = row_grp_plan(
    row_grp_structure(
         group_val = ".default",
         element_block(post_space = " ")
    ),
    label_loc = element_row_grp_loc(location = "column")
  )

)

```
