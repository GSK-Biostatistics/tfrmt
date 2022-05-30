devtools::load_all()

# Source data creation--------------------------------------------------

source("dev/data/tbl_demog_data.R")


tbl_demog_data <- create_tbl_demog_data() %>%  select(-grp) %>% ungroup



# table -------------------------------------------------------------------

fmt_spec1 <- tfrmt(
  group = vars(rowlbl1),
  label = quo(rowlbl2),
  param = quo(param),
  column = quo(column),
  value = quo(value),
  sorting_cols = vars(ord1, ord2),
  row_grp_plan = row_grp_plan(
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
  col_select = vars(-starts_with("ord")))

fmt_spec2 <- tfrmt(
  group = vars(rowlbl1),
  label = quo(rowlbl2),
  param = quo(param),
  column = quo(column),
  value = quo(value),
  sorting_cols = vars(ord1, ord2),
  col_select = vars( -starts_with("ord")),
  body_style = table_body_plan(
    frmt_structure(group_val = ".default",
                   label_val = ".default",
                   frmt_combine("{n} {pct}",
                                n = frmt("XXX", missing = ""),
                                pct = frmt("(xX.X %)", missing = "")
                   )
    ),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
    frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min","Max"), frmt("xxx.x")),
    frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx"))
  ))


# 1. actual table
tbl_demog_data %>%
  apply_tfrmt(fmt_spec1)

tbl_demog_data %>%
  print_to_gt(fmt_spec1, .)


# 2. existing table plan as mock
make_mock_data(fmt_spec1) %>%
  apply_tfrmt(fmt_spec1, mock = TRUE)

print_mock_gt(fmt_spec1)

# 3. simplified table plan as mock (1 param per row)
#  note we do not have order columns in the mock data
make_mock_data(fmt_spec2) %>%
  apply_tfrmt(fmt_spec2, mock = TRUE)

print_mock_gt(fmt_spec2)

# even better if we supply the data
tbl_demog_data %>%
  select(-value) %>%
  apply_tfrmt(fmt_spec1, mock = TRUE)

tbl_demog_data %>%
  select(-value) %>%
  print_mock_gt(fmt_spec1, .data = .)
