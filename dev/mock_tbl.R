library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)
library(rlang)
library(purrr)



foo <- fmt_str(expression = "{COLNAME1} ({COLNAME2}%)",
        fmt(rounding = "XX",  bounds = element_bounds(upper_exp = ">5"), padding = " "),
        fmt(), cols = vars(Sepal.Length, Sepal.Width))



tfmt(
  title = "Demographic Safety",
  subtitle = "Study ID: GSK12345",
  footer = "A footnote about stuff\nHere is a new line with more stuff",
  group = vars(row_label1),
  label = vars(row_label2),
  param = vars(param),
  values = vars(value),
  column = vars(column),
  sorting_cols = vars(ord_layer_1, ord_layer_2),
  body_style = element_style(
    fmt_str(group = ".default", label = ".default",
            fmt_combine("{count} {percent}",
                        count = fmt(rounding = "XXX"),
                        percent = fmt(rounding = "(xX.X %)",  bounds = element_bounds(upper_exp = "==100", upper_lab = "",
                                                                                      lower_exp = "<1.0"))
            )
    ),
    fmt_str(group = c("Age", "Weight"), label = "n", fmt(rounding = "XXX")),
    fmt_str(group = c("Age", "Weight"), label = "Min., Max.",
            fmt_combine("{Min}, {Max}",
                        Min = fmt(rounding = "XXX"),
                        Max = fmt(rounding = "XXX")
            )
    ),
    fmt_str(group = c("Age", "Weight"), label = "Mean", fmt("xx.x")),
    fmt_str(group = c("Age", "Weight"), label = "Median", fmt("xx.x")),
    fmt_str(group = c("Age", "Weight"), label = "Std", fmt("xx.xx"))
  )
)



data %>%
  filter(param %in% c("count", "percent")) %>%
  apply_combo_fmt(fmt_combine, sym("param"), sym("value"))


element <- element_style(
  fmt_str(group = ".default", label = ".default",
          fmt_combine("{count} {percent}",
                      count = fmt(rounding = "XXX"),
                      percent = fmt(rounding = "(xX.X %)",  bounds = element_bounds(upper_exp = "==100", upper_lab = "",
                                                                                    lower_exp = "<1.0"))
          )
  ),
  fmt_str(group = c("Age", "Weight"), label = "n", fmt(rounding = "XXX")),
  fmt_str(group = c("Age", "Weight"), label = "Min., Max.",
          fmt_combine("{Min}, {Max}",
                      Min = fmt(rounding = "XXX"),
                      Max = fmt(rounding = "XXX")
          )
  ),
  fmt_str(group = c("Age", "Weight"), label = "Mean", fmt("xx.x")),
  fmt_str(group = c("Age", "Weight"), label = "Median", fmt("xx.x")),
  fmt_str(group = c("Age", "Weight"), label = "Std", fmt("xx.xx"))
)
foo <-
tibble(group = element$all_fmts %>% map(~.$group),
       label = element$all_fmts %>% map(~.$label),
       fmt = element$all_fmts %>% map(~.$fmt)) %>%
  mutate(order = row_number())
#Function to test if you could apply a fmt to a row
# test is there is only one group
data


