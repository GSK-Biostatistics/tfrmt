# Print mock table to GT

Print mock table to GT

## Usage

``` r
print_mock_gt(
  tfrmt,
  .data = NULL,
  .default = 1:3,
  n_cols = NULL,
  .unicode_ws = TRUE
)
```

## Arguments

- tfrmt:

  tfrmt the mock table will be based off of

- .data:

  Optional data. If this is missing, group values, labels values and
  parameter values will be estimated based on the tfrmt

- .default:

  sequence to replace the default values if a dataset isn't provided

- n_cols:

  the number of columns. This will only be used if mock data isn't
  provided. If not supplied, it will default to using the `col_plan`
  from the `tfrmt`. If neither are available it will use 3.

- .unicode_ws:

  Whether to convert white space to unicode in preparation for output

## Value

a stylized gt object

## Examples

      # Create tfrmt specification
      tfrmt_spec <- tfrmt( label = label, column =
      column, param = param, body_plan = body_plan( frmt_structure(group_val =
      ".default", label_val = ".default", frmt_combine( "{count} {percent}",
      count = frmt("xxx"), percent = frmt_when("==100"~ frmt(""), "==0"~ "",
      "TRUE" ~ frmt("(xx.x%)")))) ))

      # Print mock table using default
      print_mock_gt(tfrmt = tfrmt_spec)

![Simple 3 by 3 table without
values](https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/master/images/example_print_mock_gt1.png)

      # Create mock data
      df <- tidyr::crossing(label = c("label 1", "label 2",
      "label 3"), column = c("placebo", "trt1", "trt2"), param = c("count",
      "percent"))

      # Print mock table using mock data
      print_mock_gt(tfrmt_spec, df)

![Simple 3 by 3 table without values, but with column
names](https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/master/images/example_print_mock_gt2.png)
