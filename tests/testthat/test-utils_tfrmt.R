test_that("Check apply_tfrmt", {

  set.seed(1234)

  raw_data_cat <- crossing(group = "A",label = c("w", "x", "y", "z"),
                       col = paste("Var", 1:4), param2 = c("count", "pct")) %>%
    rowwise() %>%
    mutate(ord1 = 1,
           ord2 = 26-which(label==letters),
           val2 = case_when(
             label == "w" & param2 == "pct" ~100.0,
             param2=="count" ~ as.double(rpois(n=1, lambda = 150)),
             param2 == "pct" ~runif(n=1,max = 100)
           )
    )

  raw_data_cont <- crossing(group = "B",label = c("w", "i", "j", "k"),
                          col = paste("Var", 1:4), param2 = c("val")) %>%
    rowwise() %>%
    mutate(ord1 = 2,
           ord2 = which(label==letters),
           val2 = case_when(
             label == "w" ~ as.double(rpois(n=1, lambda = 150)),
             label == "i" ~ rnorm(n=1, mean = 75, 13),
             label == "j" ~ rnorm(n=1, mean = 10, 3),
             label == "k" ~ rnorm(n=1, mean = 72, 7)
           )
    )
  raw_dat <- bind_rows(raw_data_cat, raw_data_cont)

  plan  <- tfrmt(
    #These are the columns that control the general structure of the data
    group = vars(group),
    label = "label",
    param = "param2",
    values = "val2",
    column = "col",
    #This controls how the rows are sorted
    sorting_cols = vars(ord1, ord2),
    col_align = element_align(left = "group",
                              right = vars(label),
                              char = vars(`Var 1`, `Var 2`, `Var 3`, `Var 4`),
                              char_val = c(" ", ",", ".")),
    body_style = table_body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("XXX.XX")),
      frmt_structure(group_val = ".default", label_val = ".default",
                     frmt_combine(
                       "{count} {pct}",
                       count = frmt("XXX"),
                       percent = frmt_when("==100"~ frmt(""),
                                           "==0"~ "",
                                           "TRUE" ~ frmt("(XXX.X%)"))
                     )),
      frmt_structure(group_val = c("B"), label_val = "w", # Value(s) in the label column where you would want to apply this fmt
        frmt("XXX")),
      frmt_structure(group_val = "B", label_val = c("i", "k"), frmt("xx.x")),
      frmt_structure(group_val = "B", label_val = "j", frmt("xx.xx"))
    ),
    # These are the variables to keep
    col_select = vars(everything(), -starts_with("ord"))
  )

  man_df <-  tribble(
    ~group, ~label, ~`Var 1`,        ~`Var 2`,        ~`Var 3`,        ~`Var 4`,
   "A",     "z",     "146 ( 13.2%)", "134 ( 56.5%)", "142 (  3.9%)", "156 ( 94.6%)",
   "A",     "y",     "150 (  4.2%)", "144 ( 56.5%)", "165 ( 66.8%)", "167 ( 89.9%)",
   "A",     "x",     "129 ( 76.0%)", "139 ( 31.2%)", "153 ( 24.4%)", "158 ( 15.3%)",
   "A",     "w",     "135         ", "141         ", "143         ", "137         ",
   "B",     "i",     " 83.5       ", " 68.9       ", " 78.2       ", " 79.2       ",
   "B",     "j",     " 10.8       ", " 11.1       ", "  8.8       ", "  5.7       ",
   "B",     "k",     " 80.3       ", " 72.5       ", " 87.3       ", " 71.6       ",
   "B",     "w",     "147         ", "149         ", "143         ", "159         "
  )

  expect_equal(apply_tfrmt(raw_dat, plan),
               man_df)

  plan$sorting_cols <- NULL

  man_df_ord <- man_df %>%
    mutate(foo = case_when(label == "w" ~ 1,
                           label == "i" ~ 2,
                           label == "k" ~ 3,
                           TRUE ~ 4)) %>%
    arrange(group, foo, label) %>%
    select(-foo)

  expect_equal(apply_tfrmt(raw_dat, plan),
               man_df_ord)

  expect_error(
    apply_tfrmt(raw_dat, tfrmt(
      label = "label2")),
      "Variable Specified in 'label' doesn't exist in the supplied dataset. Please check the tfrmt and try again."
  )

  expect_error(
    apply_tfrmt(raw_dat, tfrmt(
      group = "label2")),
      "Variable Specified in 'group' doesn't exist in the supplied dataset. Please check the tfrmt and try again."
    )

})

