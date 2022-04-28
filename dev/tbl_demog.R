devtools::load_all()

# Source data creation--------------------------------------------------

source("dev/data/tbl_demog_data.R")

tbl_demog_data <- create_tbl_demog_data()


# table -------------------------------------------------------------------

frmt_spec <- tfrmt(
  group = vars(rowlbl1, grp),
  label = quo(rowlbl2),
  param = quo(param),
  column = quo(column),
  value = quo(value),
  sorting_cols = vars(ord1, ord2),
  row_grp_style = row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = "   "))
    ),
  body_style = table_body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{n} {pct}",
                                n = frmt("XXX"),
                                pct = frmt_when("==100" ~ "",
                                                "==0" ~ "",
                                                TRUE ~ frmt("(XX.X %)"))
                   )
    ),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
    frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min","Max"), frmt("xxx.x")),
    frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx")),
    frmt_structure(group_val = ".default", label_val = ".default", p = frmt_when(">0.99" ~ ">0.99",
                                                                                 "<0.001" ~ "<0.001",
                                                                                 TRUE ~ frmt("x.xxx", missing = "")))
  ),
  col_select = vars(-starts_with("ord"), -grp))

apply_tfrmt(tbl_demog_data, frmt_spec)

print_to_gt(frmt_spec, tbl_demog_data)


