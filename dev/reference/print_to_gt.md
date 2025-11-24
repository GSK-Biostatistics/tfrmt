# Print to gt

Print to gt

## Usage

``` r
print_to_gt(tfrmt, .data, .unicode_ws = TRUE)
```

## Arguments

- tfrmt:

  tfrmt object that will dictate the structure of the table

- .data:

  Data to style in order to make the table

- .unicode_ws:

  Whether to convert white space to unicode in preparation for output

## Value

a stylized gt object

## Examples

    library(dplyr)
    # Create tfrmt specification
    tfrmt_spec <- tfrmt(
      label = label,
      column = column,
      param = param,
      value=value,
      body_plan = body_plan(
        frmt_structure(group_val = ".default", label_val = ".default",
                       frmt_combine(
                         "{count} {percent}",
                         count = frmt("xxx"),
                         percent = frmt_when("==100"~ frmt(""),
                                             "==0"~ "",
                                             "TRUE" ~ frmt("(xx.x%)"))))
      ))

    # Create data
    df <- tidyr::crossing(label = c("label 1", "label 2"),
                   column = c("placebo", "trt1"),
                   param = c("count", "percent")) %>%
          dplyr::mutate(value=c(24,19,2400/48,1900/38,5,1,500/48,100/38))

    print_to_gt(tfrmt_spec,df)

![2 by 2 table with labels down the side and placebo and trt1 across the
top](https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/master/images/example_print_to_gt.png)
