# Extract underlying data from tfrmt output

Following a call to `print_to_gt`, this function extracts the underlying
data frame(s) from the resulting `gt` or `gt_group` object.

## Usage

``` r
extract_data(x, col_delim = "_")
```

## Arguments

- x:

  A `gt_tbl` or `gt_group` object (usually the output of
  [`print_to_gt()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/print_to_gt.md)).

- col_delim:

  Character string to replace the internal "tlang_delim" separator in
  column names only for tables with spanning headers. Defaults to "\_".

## Value

If `gt_tbl`, a single data frame. If `gt_group`, a list of data frames.
