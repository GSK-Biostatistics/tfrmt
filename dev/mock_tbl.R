library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)
library(rlang)
library(purrr)
library(tidyr)
source("dev/sample_data.R")


test <- tfmt(
  title = "Demographic Safety",
  subtitle = "Study ID: GSK12345",
  footer = "A footnote about stuff\nHere is a new line with more stuff",
  group = vars(row_label1),
  label = vars(row_label2)[[1]],
  param = vars(param)[[1]],
  values = vars(value)[[1]],
  column = vars(column)[[1]],
  sorting_cols = vars(ord_layer_1, ord_layer_2),
  body_style = element_style(
    fmt_str(group_val = ".default", label_val = ".default",
            fmt_combine("{count} {percent}",
                        count = fmt(rounding = "XXX"),
                        percent = fmt(rounding = "(xX.X %)",  bounds = element_bounds(upper_exp = "==100", upper_lab = "",
                                                                                      lower_exp = "<1.0"))
            )
    ),
    fmt_str(group_val = c("Age", "Weight"), label_val = "n", fmt(rounding = "XXX")),
    fmt_str(group_val = c("Age", "Weight"), label_val = "Min., Max.",
            fmt_combine("{Min}, {Max}",
                        Min = fmt(rounding = "XXX"),
                        Max = fmt(rounding = "XXX")
            )
    ),
    fmt_str(group_val = c("Age", "Weight"), label_val = "Mean", fmt("xx.x")),
    fmt_str(group_val = c("Age", "Weight"), label_val = "Median", fmt("xx.x")),
    fmt_str(group_val = c("Age", "Weight"), label_val = "Std", fmt("xx.xx"))
  ),
  col_select = vars(-starts_with("ord"))
)



apply_tfmt(data, test)



