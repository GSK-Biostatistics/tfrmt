# N Percent Template

This function creates an tfrmt for an n % table, so count based table.
The parameter values for n and percent can be provided (by default it
will assume `n` and `pct`). Additionally the `frmt_when` for formatting
the percent can be specified. By default 100% and 0% will not appear and
everything between 99% and 100% and 0% and 1% will be rounded using
greater than (\>) and less than (\<) signs respectively.

## Usage

``` r
tfrmt_n_pct(
  n = "n",
  pct = "pct",
  pct_frmt_when = frmt_when("==100" ~ frmt(""), ">99" ~ frmt("(>99%)"), "==0" ~ "", "<1"
    ~ frmt("(<1%)"), "TRUE" ~ frmt("(xx.x%)")),
  tfrmt_obj = NULL
)
```

## Arguments

- n:

  name of count (n) value in the parameter column

- pct:

  name of percent (pct) value in the parameter column

- pct_frmt_when:

  formatting to be used on the the percent values

- tfrmt_obj:

  an optional tfrmt object to layer

## Value

tfrmt object

## Examples

    print_mock_gt(tfrmt_n_pct())

![3 by 3
table](https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/master/images/example_n_percent.png)
