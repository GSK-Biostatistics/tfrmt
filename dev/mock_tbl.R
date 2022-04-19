library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)
library(rlang)
library(purrr)
library(tidyr)
source("dev/sample_data.R")


fmt_spec <- tfmt(
  title = "Demographic Safety",
  subtitle = "Study ID: GSK12345",
  footer = "A footnote about stuff<br/>Here is a new line with more stuff",
  #These are the columns that control the general structure of the data
  group = vars(row_label1),
  label = vars(row_label2)[[1]],
  param = vars(param)[[1]],
  values = vars(value)[[1]],
  column = vars(column)[[1]],
  col_align = element_align(left = "total",
                            right = vars(Placebo),
                            dec_pl = c(`Xanomeline High Dose`, `Xanomeline Low Dose`)), # specify by values of column var, can leave any empty
  #This controls how the rows are sorted
  sorting_cols = vars(ord_layer_1, ord_layer_2),
  body_style = element_style(
    fmt_str(group_val = ".default", label_val = ".default",
            fmt_combine("{count} {percent}",
                        count = fmt(rounding = "XXX"),
                        percent = fmt(rounding = "(xX.X %)",  bounds = element_bounds(upper_exp = "==100", upper_lab = "",
                                                                                      lower_exp = "<1.0"))
            )
    ),
    fmt_str(group_val = c("Age", "Weight"), #Values in the group column where you would want to apply this fmt
            label_val = "n", # Value(s) in the label column where you would want to apply this fmt
            fmt(rounding = "XXX")),
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
  # These are the variables to keep
  col_select = vars(-total, everything(), -starts_with("ord"))
)

apply_tfmt(data, fmt_spec)


apply_tfmt(data, fmt_spec) %>%
  gt::gt(.,
         groupname_col = as_label(fmt_spec$group[[1]]),
         rowname_col = as_label(fmt_spec$label)) %>%
  gt::tab_style(
    style = gt::cell_text(font = gt::google_font("PT Mono"), whitespace = "pre"),
    locations = gt::cells_body(columns = everything())
  ) %>%
  gt::cols_align(align = "left", columns = everything()) # also check right, center to see how white space is added/preserved

