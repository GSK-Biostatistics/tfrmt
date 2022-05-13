

# Source data creation--------------------------------------------------

source("dev/data/tbl_ae_data.R")

tbl_ae_data <- create_tbl_ae_data()

fmt_spec <- tfrmt(
  group = vars(AEBODSYS),
  label = quo(AETERM),
  param = quo(param),
  column = quo(column),
  value = quo(value),
  sorting_cols = vars(ord1, ord2),
  col_select = vars(-starts_with("ord")),
  body_style = table_body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
            frmt_combine("{n} {pct}",
                        n = frmt("XXX"),
                        pct = frmt_when(
                          "==100" ~ "",
                          "==0" ~ "",
                          TRUE ~ frmt("(xx.x %)")))),
    frmt_structure(group_val = ".default", label_val = ".default", AEs = frmt("[XXX]")),
    frmt_structure(group_val = ".default", label_val = ".default", pval = frmt_when(">0.99" ~ ">0.99",
                                                                                    "<0.001" ~ "<0.001",
                                                                                    TRUE ~ frmt("x.xxx", missing = "--")))
  ))



# 1. actual table
tbl_ae_data %>%
  apply_tfrmt(fmt_spec)

# 2. existing table plan as mock
make_mock_data(fmt_spec) %>%
  apply_tfrmt(fmt_spec, mock = TRUE)

# 3. supply data shell for mock
tbl_ae_data %>%
  select(-value) %>%
  apply_tfrmt(fmt_spec, mock = TRUE)

library(gt)
tbl_ae_data %>%
  select(-value) %>%
  apply_tfrmt(fmt_spec, mock = TRUE)  %>%
  gt() %>%
  tab_header(title = fmt_spec$title,
             subtitle = fmt_spec$subtitle) %>%
  apply_gt_footnote(fmt_spec$footer) %>%
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body(
      columns = AETERM,
      rows = ! AETERM==AEBODSYS
    )
  ) %>%
  cols_hide(AEBODSYS)%>%
  tab_spanner_delim(delim = "__") %>%
  cols_label(p_low = "Placebo vs. Low Dose",
             p_high = "Placebo vs. High Dose",
             AETERM = "System Organ Class/Preferred Term")
