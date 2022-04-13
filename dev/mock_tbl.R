library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)
library(rlang)
library(purrr)
library(tidyr)
library(gt)
source("dev/sample_data.R")


tfrmt_spec  <- tfrmt(
  title = "Demographic Safety",
  subtitle = "Study ID: GSK12345",
  footer = "A footnote about stuff<br/>Here is a new line with more stuff",
  #These are the columns that control the general structure of the data
  group = vars(row_label1),
  label = vars(row_label2)[[1]],
  param = vars(param)[[1]],
  values = vars(value)[[1]],
  column = vars(column)[[1]],
  #This controls how the rows are sorted
  sorting_cols = vars(ord_layer_1, ord_layer_2),
  body_style = table_body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt("XXX.XX")),
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine(
                     "{count} {percent}",
                     count = frmt("XXX"),
                     percent = frmt("(XXX.X %)")
                   )),
    frmt_structure(
      group_val = c("Age", "Weight"), #Values in the group column where you would want to apply this fmt
      label_val = "n", # Value(s) in the label column where you would want to apply this fmt
      frmt("XXX")),
    frmt_structure(
      group_val = c("Age", "Weight"),
      label_val = "Min., Max.",
      frmt_combine("{Min}, {Max}",
                   Min = frmt("XXX"),
                   Max = frmt("XXX")
      )
    ),
    frmt_structure(group_val = c("Age", "Weight"), label_val = c("Mean", "Median"), frmt("xx.x")),
    frmt_structure(group_val = c("Age", "Weight"), label_val = "Std", frmt("xx.xx"))
  ),
  # These are the variables to keep
  col_select = vars(-total, everything(), -starts_with("ord"))
)



print_to_gt(tfrmt_spec , data)

.data %>% filter(row_label1 %in% c("Age","Weight")) %>% filter(row_label2 %in% c("Mean","Median","Std"))

frmt_structure(group_val = c("Age", "Weight"), label_val = c("Mean", "Median"), fmt("xx.x"))

