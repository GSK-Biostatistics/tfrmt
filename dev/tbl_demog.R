devtools::load_all()

# Source data creation--------------------------------------------------

source("dev/data/tbl_demog_data.R")

tbl_demog_data <- create_tbl_demog_data() %>% select(-grp)


# table -------------------------------------------------------------------

fmt_spec <- tfrmt(
  group = vars(rowlbl1),
  label = quo(rowlbl2),
  param = quo(param),
  column = quo(column),
  value = quo(value),
  sorting_cols = vars(ord1, ord2),
  col_select = vars(-starts_with("ord")),
  body_style = table_body_plan(
    frmt_structure(group_val = ".default",
            label_val = ".default",
            frmt_combine("{n} {pct}",
                        n = frmt("XXX"),
                        pct = frmt("(xX.X %)")
            )
    ),

    frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),


    frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min","Max"), frmt("xxx.x")),
    frmt_structure(group_val = ".default", label_val = ".default", SD = frmt("xxx.xx")),
    frmt_structure(group_val = ".default", label_val = ".default", p = frmt("x.xxx",
                                                                       bounds = element_bounds(upper_exp = ">0.99", upper_lab = ">0.99",
                                                                                               lower_exp = "<0.001", lower_lab = "<0.001"),
                                                                       missing = ""))
  ))


print_to_gt(fmt_spec, tbl_demog_data)


