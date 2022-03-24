library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)
library(rlang)
library(purrr)
data


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
    fmt_str(group = "Age", label = "Min., Max.",
            fmt_combine("{Min}, {Max}",
                        Min = fmt(),
                        Max = fmt(),
                        missing = fx()
            )
    ),

  )

)


fmt_combine <- fmt_combine("{count} {percent}",
            count = fmt(rounding = "XXX"),
            percent = fmt(rounding = "(xX.X %)",  bounds = element_bounds(upper_exp = "==100", upper_lab = "",
                                                                          lower_exp = "<1.0"))
)
group = vars(row_label1)
label = vars(row_label2)
column = vars(column)
.data <- data %>%
  filter(param %in% c("count", "percent"))
param = vars(param); values = vars(value)

# Add test to ensure they only has a length of 1
param = param[[1]]
values = values [[1]]





fmt <-fmt(rounding = "(XX.X %)",  bounds = element_bounds(upper_exp = ">99.9", upper_lab = "",
                                                          lower_exp = "<1.0"), missing = "MISSING")

