# Display formatted values

A helper for creating positional-alignment specifications for the
col_style_plan. Returns all unique formatted values to appear in the
column(s) specified. Numeric values are represented by x's.

## Usage

``` r
display_val_frmts(tfrmt, .data, mock = FALSE, col = NULL)
```

## Arguments

- tfrmt:

  tfrmt object to apply to the data

- .data:

  Data to apply the tfrmt to

- mock:

  Mock table? TRUE or FALSE (default)

- col:

  Column value to align on from `column` variable. May be a quoted or
  unquoted column name, a tidyselect semantic, or a span_structure.

## Value

text representing character vector of formatted values to be copied and
modified in the col_style_plan

## Examples

``` r
 tf_spec <- tfrmt(
 group = c(rowlbl1,grp),
 label = rowlbl2,
 column = column,
 param = param,
 value = value,
 sorting_cols = c(ord1, ord2),
 body_plan = body_plan(
 frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} ({pct} %)",
                                                                             n = frmt("xxx"),
                                                                             pct = frmt("xx.x"))),
 frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
 frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min","Max"),
                             frmt("xxx.x")),
 frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx")),
 frmt_structure(group_val = ".default", label_val = ".default",
                            p = frmt_when(">0.99" ~ ">0.99",
                                          "<0.15" ~ "<0.15",
                                          TRUE ~ frmt("x.xxx", missing = "")))
))

 display_val_frmts(tf_spec, data_demog, col = vars(everything()))
#> c("",
#>   "x",
#>   " xx",
#>   "xxx",
#>   "x.xxx",
#>   " xx.x",
#>   "<x.xx",
#>   "  x.x",
#>   "xxx.x",
#>   "  x.xx",
#>   " xx.xx",
#>   " xx (xx.x %)",
#>   "  x ( x.x %)",
#>   "xxx (xx.x %)",
#>   "  x (xx.x %)",
#>   " xx ( x.x %)")
 display_val_frmts(tf_spec, data_demog, col = "p-value")
#> c("",
#>   "x.xxx",
#>   "<x.xx")
```
