

# Source data creation--------------------------------------------------

source("dev/data/tbl_ae_data.R")

tbl_ae_data <- create_tbl_ae_data()

fmt_spec <- tfmt(
  group = vars(AEBODSYS),
  label = quo(AETERM),
  param = quo(param),
  column = quo(column),
  value = quo(value),
  sorting_cols = vars(ord1, ord2),
  col_select = vars(-starts_with("ord")),
  body_style = element_style(
    fmt_str(group_val = ".default", label_val = ".default",
            fmt_combine("{n} {pct}",
                        n = fmt(rounding = "XXX"),
                        pct = fmt(rounding = "(xX.X %)",  bounds = element_bounds(upper_exp = "==100", upper_lab = "",
                                                                                  lower_exp = "==0", lower_lab = ""))
            )
    ),
    fmt_str(group_val = ".default", label_val = ".default", AEs = fmt("[XXX]")),
    fmt_str(group_val = ".default", label_val = ".default", pval = fmt(rounding = "x.xxx",
                                                                       bounds = element_bounds(upper_exp = ">0.99", upper_lab = ">0.99",
                                                                                               lower_exp = "<0.001", lower_lab = "<0.001"),
                                                                       missing = "--"))
  ))

#temp
library(rlang)
library(gt)

# tfmt values
apply_tfmt(tbl_ae_data,fmt_spec)

# make full table
apply_tfmt(tbl_ae_data,fmt_spec) %>%
  gt() %>%
  # groupname_col = as_label(fmt_spec$group[[1]]),
  # rowname_col = as_label(fmt_spec$label)) %>%
  tab_header(title = fmt_spec$title,
             subtitle = fmt_spec$subtitle) %>%
  apply_gt_footnote(fmt_spec$footer) %>%
  tab_style(
    style = cell_text(indent = px(15)),
    locations = cells_body(
      columns = AETERM,
      rows = ! AETERM==AEBODSYS
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = AETERM,
      rows = AETERM==AEBODSYS
    )
  ) %>%
  cols_hide(AEBODSYS)%>%
  tab_spanner_delim(delim = "__") %>%
  cols_label(p_low = "Placebo vs. Low Dose",
             p_high = "Placebo vs. High Dose",
             AETERM = "System Organ Class/Preferred Term")



