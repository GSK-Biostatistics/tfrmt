# Big N Structure

Big N structure allows you to specify which values should become the
subject totals ("big N" values) and how they should be formatted in the
table's column labels. Values are specified by providing the value(s) of
the `param` column for which the values are big N's. This will remove
these from the body of the table and place them into columns matching
the values in the column column(s). The default formatting is `N = xx`,
on its own line, but that can be changed by providing a different
[`frmt()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/frmt.md)
to `n_frmt`.

## Usage

``` r
big_n_structure(param_val, n_frmt = frmt("\nN = xx"), by_page = FALSE)
```

## Arguments

- param_val:

  row value(s) of the parameter column for which the values are big N's

- n_frmt:

  [`frmt()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/frmt.md)
  to control the formatting of the big N's

- by_page:

  Option to include different big N's for each group-defined set of
  pages (defined by any variables set to `".default"` in the
  `page_plan`). Default is `FALSE`, meaning only the overall Ns are
  applied

## Value

big_n_structure object

## See also

[Link to related
article](https://gsk-biostatistics.github.io/tfrmt/articles/big_ns.html)
