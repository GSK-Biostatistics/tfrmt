library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)
library(rlang)
library(purrr)
library(tidyr)
source("dev/sample_data.R")

tfrmt_spec  <- tfrmt(
  title = "Demographic Table",
  subtitle = "Study ID: GSK12345",
  footer = "A footnote about stuff<br/>Here is a new line with more stuff",
  #These are the columns that control the general structure of the data
  group = vars(row_label1),
  label = "row_label2",
  param = "param",
  values = "value",
  column = "column",
  #This controls how the rows are sorted
  sorting_cols = vars(ord_layer_1, ord_layer_2),
  col_align = element_align(left = "total",
                            right = vars(Placebo),
                            char = c(`Xanomeline High Dose`, `Xanomeline Low Dose`),
                            char_val = c(" ", ",", ".")),
  body_style = table_body_plan(
    # frmt_structure(group_val = ".default", label_val = ".default", frmt("XXX.XX")),
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine(
                     "{count} {percent}",
                     count = frmt("XXX"),
                     percent = frmt_when("==100"~ frmt(""),
                                         "==0"~ "",
                                         "TRUE" ~ frmt("(XXX.X%)")
                     )
                   )),
    frmt_structure(
      group_val = c("Age", "Weight"), #Values in the group column where you would want to apply this fmt
      label_val = "n", # Value(s) in the label column where you would want to apply this fmt
      frmt("XXX")),
    frmt_structure(
      group_val = c("Age", "Weight"),
      label_val = "Min., Max.",
      frmt_combine("{Min}, {Max}",
                   Min = frmt("XX"),
                   Max = frmt("XXX")
      )
    ),
    frmt_structure(group_val = c("Age", "Weight"), label_val = c("Mean", "Median"), frmt("xx.x")),
    frmt_structure(group_val = c("Age", "Weight"), label_val = "Std", frmt("xx.xx"))
  ),
  # These are the variables to keep
  col_select = vars(-total, everything(), -starts_with("ord")),

  spanning_label_grp = span_plan(
    span_structure(
      "Treatment",
      vars(starts_with("Xanomeline"))
    )
  )
)


print_to_gt(tfrmt_spec , data)

print_mock_gt(tfrmt_spec)



# Layering Example --------------------------------------------------------

demog_tbl <- function(){
  tfrmt(
    title = "Demographic Table",
    #These are the columns that control the general structure of the data
    #This controls how the rows are sorted
    sorting_cols = vars(ord_layer_1, ord_layer_2),
    body_style = table_body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("XXX.XX")),
      frmt_structure(group_val = ".default", label_val = ".default",
                     frmt_combine(
                       "{count} {percent}",
                       count = frmt("XXX"),
                       percent = frmt("(XXX.X%)", bounds = element_bounds(upper_exp = "==100", upper_lab = "",
                                                                          lower_exp = "<1.0"))
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
}


demog_tbl() %>%
  tfrmt(
    subtitle = "Study ID: GSK12345",
    footer = "A footnote about stuff<br/>Here is a new line with more stuff",
    group = vars(row_label1),
    label = "row_label2",
    param = "param",
    values = "value",
    column = "column",
  ) %>%
  print_to_gt(data)


# align example -------------------------------------------------------
apply_tfrmt(data, tfrmt_spec) %>%
  gt::gt(.,
         groupname_col = as_label(tfrmt_spec$group[[1]]),
         rowname_col = as_label(tfrmt_spec$label)) %>%
  gt::tab_style(
    style = gt::cell_text(font = gt::google_font("PT Mono"), whitespace = "pre"),
    locations = gt::cells_body(columns = everything())
  ) %>%
  gt::cols_align(align = "right", columns = everything()) # also check right, center to see how white space is added/preserved

