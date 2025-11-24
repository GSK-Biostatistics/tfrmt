# Display formatting applied to each row

Used when debugging formatting, it is an easy way to allow you to see
which formats are applied to each row in your dataset.

## Usage

``` r
display_row_frmts(tfrmt, .data, convert_to_txt = TRUE)
```

## Arguments

- tfrmt:

  tfrmt object to apply to the data

- .data:

  Data to apply the tfrmt to

- convert_to_txt:

  Logical value converting formatting to text, by default `TRUE`

## Value

formatted tibble

## Examples

``` r
 library(dplyr)
 library(tidyr)

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

 display_row_frmts(tfrmt_spec,df)
#> # A tibble: 8 × 6
#>   label   column  param   value frmt_type    frmt_details       
#>   <chr>   <chr>   <chr>   <dbl> <chr>        <chr>              
#> 1 label 1 placebo count   24    frmt_combine `{count} {percent}`
#> 2 label 1 placebo percent 19    frmt_combine `{count} {percent}`
#> 3 label 1 trt1    count   50    frmt_combine `{count} {percent}`
#> 4 label 1 trt1    percent 50    frmt_combine `{count} {percent}`
#> 5 label 2 placebo count    5    frmt_combine `{count} {percent}`
#> 6 label 2 placebo percent  1    frmt_combine `{count} {percent}`
#> 7 label 2 trt1    count   10.4  frmt_combine `{count} {percent}`
#> 8 label 2 trt1    percent  2.63 frmt_combine `{count} {percent}`
```
