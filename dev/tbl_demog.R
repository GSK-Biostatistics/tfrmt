devtools::load_all()

# Source data creation--------------------------------------------------

source("dev/data/tbl_demog_data.R")

tbl_demog_data <- create_tbl_demog_data() %>% select(-grp)


# table -------------------------------------------------------------------

fmt_spec <- tfmt(
  group = vars(rowlbl1),
  label = quo(rowlbl2),
  param = quo(param),
  column = quo(column),
  value = quo(value),
  sorting_cols = vars(ord1, ord2),
  col_select = vars(-starts_with("ord")),
  body_style = element_style(
    fmt_str(group_val = ".default",
            label_val = ".default",
            fmt_combine("{n} {pct}",
                        n = fmt(rounding = "XXX"),
                        pct = fmt(rounding = "(xX.X %)")
            )
    ),

    fmt_str(group_val = ".default", label_val = "n", fmt("xxx")),


    fmt_str(group_val = ".default", label_val = "Mean", fmt("xxx.x")),
    fmt_str(group_val = ".default", label_val = ".default", SD = fmt("xxx.xx")),
    fmt_str(group_val = ".default", label_val = ".default", Median = fmt("xxx.x")),
    fmt_str(group_val = ".default", label_val = ".default", Min = fmt("xxx.x")),
    fmt_str(group_val = ".default", label_val = ".default", Max = fmt("xxx.x")),
    fmt_str(group_val = ".default", label_val = ".default", p = fmt(rounding = "x.xxx",
                                                                       bounds = element_bounds(upper_exp = ">0.99", upper_lab = ">0.99",
                                                                                               lower_exp = "<0.001", lower_lab = "<0.001"),
                                                                       missing = ""))
  ))

#temp
library(rlang)
#library(gt)

# tfmt values
out <- apply_tfmt(tbl_demog_data,fmt_spec) # does not work


