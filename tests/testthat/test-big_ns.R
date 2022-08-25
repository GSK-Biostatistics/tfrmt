test_that("Defining the big Ns", {
  bn1 <- big_n_structure(param_val = "bigN", n_frmt = frmt("Hello World"))
  bn2 <- big_n_structure(param_val = c("bigN", "N"))

  expect_s3_class(bn1,"big_n_structure")
  expect_s3_class(bn2,"big_n_structure")

  expect_equal(bn1[["param_val"]], c("bigN"), ignore_attr = c(".Environment"))
  expect_equal(bn1[["n_frmt"]], frmt("Hello World"), ignore_attr = c(".Environment"))
  expect_equal(bn2[["param_val"]], c("bigN", "N"), ignore_attr = c(".Environment"))
  expect_equal(bn2[["n_frmt"]], frmt("\nN = xx"), ignore_attr = c(".Environment"))

  expect_error(big_n_structure(param_val = "bigN", n_frmt = "Hello World"))
})



test_that("Simple Case big_n", {

  data <- tibble(Group = rep(c("Age (y)", "Sex", "Age (y)", "Sex"), c(3, 3, 6,12)),
                 Label = rep(c("n", "Mean (SD)", "Male","Female"), c(6, 6,6,6)),
                 Column = rep(c("Placebo", "Treatment", "Total"), times = 8),
                 Param = rep(c("n", "mean", "sd", "n", "pct", "n", "pct"),  c(6, 3, 3, 3,3,3,3)),
                 Value = c(15,13,28,14,13,27,73.56, 74.231,71.84,9.347,7.234,8.293,8,7,15,8/14,7/13,15/27,6,6,12,6/14,6/13,12/27
                 )
  ) %>%
    # Note because tfrmt only does rounding we will need to have the percents multiplied by 100
    mutate(Value = case_when(Param == "pct" ~ Value * 100,
                             TRUE ~ Value),
           ord1 = if_else(Group == "Age (y)", 1, 2),
           ord2 = if_else(Label == "n", 1, 2))

  big_ns <- tibble(Column = c("Placebo", "Treatment", "Total"),
                   Param = "bigN",
                   Value = c(30,30,60))


  data <- bind_rows(data, big_ns)

  tfrmt_sans_colplan <- tfrmt(
    group = Group,
    label = Label,
    column = Column,
    value = Value,
    param = Param,
    sorting_cols = c(ord1, ord2),
    body_plan = body_plan(
      frmt_structure(group_val = ".default",
                     label_val = ".default",
                     frmt_combine("{n} {pct}",
                                  n = frmt("X"),
                                  pct = frmt("(xx.x%)", missing = " ")
                     )
      ),
      frmt_structure(group_val = "Age (y)", label_val = "Mean (SD)",
                     frmt_combine("{mean} ({sd})",
                                  mean = frmt("XX.X"),
                                  sd = frmt("x.xx")
                     )
      ),
      frmt_structure(group_val = ".default", label_val = "n", frmt("xx"))
    ),
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " "))
    ),
    big_n = big_n_structure(param_val = "bigN")
  )

  tfrmt_wit_colplan <- tfrmt_sans_colplan %>%
    tfrmt(
      col_plan = col_plan(everything(), -starts_with("ord"), "Total")
    )

  auto_sans_colplan <- tfrmt_sans_colplan %>%
    apply_tfrmt(.data = data, tfrmt = ., mock = FALSE) %>%
    names()

  expect_equal(
    auto_sans_colplan,
    c(
      "Label",
      "ord1",
      "ord2",
      "Placebo\nN = 30",
      "Total\nN = 60",
      "Treatment\nN = 30",
      "..tfrmt_row_grp_lbl"
    )
  )

  auto_wit_colplan <- tfrmt_wit_colplan %>%
    apply_tfrmt(.data = data, tfrmt = ., mock = FALSE) %>%
    names()

  expect_equal(
    auto_wit_colplan,
    c(
      "Label",
      "Placebo\nN = 30",
      "Treatment\nN = 30",
      "Total\nN = 60",
      "..tfrmt_row_grp_lbl"
    )
  )

  auto_mock <- apply_tfrmt(.data = select(data, -Value), tfrmt = tfrmt_wit_colplan, mock = TRUE) %>%
    names()

  expect_equal(auto_mock, c(
    "Label",
    "Placebo\nN = xx",
    "Treatment\nN = xx",
    "Total\nN = xx",
    "..tfrmt_row_grp_lbl"
  ))

})

test_that("Test with spanning headers", {

  dat <- tibble::tribble(
    ~group,     ~label,        ~span2,  ~span1,     ~my_col,    ~parm,   ~val,
    "g1", "rowlabel1",  "column cols", "cols 1,2", "col1"  ,  "value",    1,
    "g1", "rowlabel1",  "column cols", "cols 1,2", "col2"  ,  "value",    1,
    "g1", "rowlabel1",             NA,         NA, "mycol3",  "value",    1,
    "g1", "rowlabel1",  "column cols", "col 4"   , "col4"  ,  "value",    1,
    "g1", "rowlabel1",             NA,         NA, "mycol5",  "value",    1,
    "g1", "rowlabel2",  "column cols", "cols 1,2", "col1"  ,  "value",    2,
    "g1", "rowlabel2",  "column cols", "cols 1,2", "col2"  ,  "value",    2,
    "g1", "rowlabel2",             NA,        NA , "mycol3",  "value",    2,
    "g1", "rowlabel2",  "column cols", "col 4"   , "col4"  ,  "value",    2,
    "g1", "rowlabel2",             NA,         NA, "mycol5",  "value",    2,
    "g2", "rowlabel3",  "column cols", "cols 1,2", "col1"  ,  "value",    3,
    "g2", "rowlabel3",  "column cols", "cols 1,2", "col2"  ,  "value",    3,
    "g2", "rowlabel3",             NA,         NA, "mycol3",  "value",    3,
    "g2", "rowlabel3",  "column cols", "col 4"   , "col4"  ,  "value",    3,
    "g2", "rowlabel3",             NA,         NA, "mycol5",  "value",    3,
    #big n's
    NA, NA,  "column cols", NA, NA  ,  "bigN",    18,
    NA, NA,  "column cols", "cols 1,2", NA  ,  "bigN",    12,
    NA, NA,  "column cols", "col 4"   , "col4"  ,  "bigN",    6,
    NA, NA,             NA,         NA, "mycol3",  "bigN",    6,
  )



  auto <- tfrmt(
    group = group,
    label = label,
    param = parm,
    value = val,
    column = c(span2, span1, my_col),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("x"))
    ),
    col_plan = col_plan(
      group,
      label,
      starts_with("col"),
      new_col_3 = mycol3,
      -mycol5
    ),
    big_n = big_n_structure(param_val = "bigN")
  ) %>%
    apply_tfrmt(.data = dat, tfrmt = ., mock = FALSE) %>%
    names()

  man <- c("group"                                                                    , "label",
           "column cols\nN = 18___tlang_delim___cols 1,2\nN = 12___tlang_delim___col1", "column cols\nN = 18___tlang_delim___cols 1,2\nN = 12___tlang_delim___col2",
           "column cols\nN = 18___tlang_delim___col 4___tlang_delim___col4\nN =  6"   , "new_col_3\nN =  6")
  expect_equal(auto, man)
})




test_that("Multiple big N params", {

  data <- tibble(Group = rep(c("Age (y)", "Sex", "Age (y)", "Sex"), c(3, 3, 6,12)),
                 Label = rep(c("n", "Mean (SD)", "Male","Female"), c(6, 6,6,6)),
                 Column = rep(c("Placebo", "Treatment", "Total"), times = 8),
                 Param = rep(c("n", "mean", "sd", "n", "pct", "n", "pct"),  c(6, 3, 3, 3,3,3,3)),
                 Value = c(15,13,28,14,13,27,73.56, 74.231,71.84,9.347,7.234,8.293,8,7,15,8/14,7/13,15/27,6,6,12,6/14,6/13,12/27
                 )
  ) %>%
    # Note because tfrmt only does rounding we will need to have the percents multiplied by 100
    mutate(Value = case_when(Param == "pct" ~ Value * 100,
                             TRUE ~ Value),
           ord1 = if_else(Group == "Age (y)", 1, 2),
           ord2 = if_else(Label == "n", 1, 2))

  big_ns <- tibble(Column = c("Placebo", "Treatment", "Total"),
                   Param = c("bigN", "big_n", "big_n"),
                   Value = c(30,30,60))


  data <- bind_rows(data, big_ns)

  auto <- tfrmt(
    group = Group,
    label = Label,
    column = Column,
    value = Value,
    param = Param,
    sorting_cols = c(ord1, ord2),
    body_plan = body_plan(
      frmt_structure(group_val = ".default",
                     label_val = ".default",
                     frmt_combine("{n} {pct}",
                                  n = frmt("X"),
                                  pct = frmt("(xx.x%)", missing = " ")
                     )
      ),
      frmt_structure(group_val = "Age (y)", label_val = "Mean (SD)",
                     frmt_combine("{mean} ({sd})",
                                  mean = frmt("XX.X"),
                                  sd = frmt("x.xx")
                     )
      ),
      frmt_structure(group_val = ".default", label_val = "n", frmt("xx"))
    ),
    col_plan = col_plan(everything(), -starts_with("ord"), "Total"),
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " "))
    ),
    big_n = big_n_structure(param_val = c("bigN", "big_n"))
  ) %>%
    apply_tfrmt(.data = data, tfrmt = ., mock = FALSE) %>%
    names()

  man <- big_ns %>%
    mutate(foo = str_c(Column, "\nN = ", Value)) %>%
    pull(foo) %>%
    c("Label", . , "..tfrmt_row_grp_lbl")
  expect_equal(auto, man)
})

test_that("Overlapping Big N's",{

  data <- tibble(Group = rep(c("Age (y)", "Sex", "Age (y)", "Sex"), c(3, 3, 6,12)),
                 Label = rep(c("n", "Mean (SD)", "Male","Female"), c(6, 6,6,6)),
                 Column = rep(c("Placebo", "Treatment", "Total"), times = 8),
                 Param = rep(c("n", "mean", "sd", "n", "pct", "n", "pct"),  c(6, 3, 3, 3,3,3,3)),
                 Value = c(15,13,28,14,13,27,73.56, 74.231,71.84,9.347,7.234,8.293,8,7,15,8/14,7/13,15/27,6,6,12,6/14,6/13,12/27
                 )
  ) %>%
    # Note because tfrmt only does rounding we will need to have the percents multiplied by 100
    mutate(Value = case_when(Param == "pct" ~ Value * 100,
                             TRUE ~ Value),
           ord1 = if_else(Group == "Age (y)", 1, 2),
           ord2 = if_else(Label == "n", 1, 2))

  big_ns <- tibble(Column = c("Placebo", "Treatment", "Total", "Total"),
                   Param = "bigN",
                   Value = c(30,30,60, 65))


  data <- bind_rows(data, big_ns)

  tfrmt_test <- tfrmt(
    group = Group,
    label = Label,
    column = Column,
    value = Value,
    param = Param,
    sorting_cols = c(ord1, ord2),
    body_plan = body_plan(
      frmt_structure(group_val = ".default",
                     label_val = ".default",
                     frmt_combine("{n} {pct}",
                                  n = frmt("X"),
                                  pct = frmt("(xx.x%)", missing = " ")
                     )
      ),
      frmt_structure(group_val = "Age (y)", label_val = "Mean (SD)",
                     frmt_combine("{mean} ({sd})",
                                  mean = frmt("XX.X"),
                                  sd = frmt("x.xx")
                     )
      ),
      frmt_structure(group_val = ".default", label_val = "n", frmt("xx"))
    ),
    col_plan = col_plan(everything(), -starts_with("ord"), "Total"),
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " "))
    ),
    big_n = big_n_structure(param = "bigN")
  )

  expect_warning(apply_tfrmt(.data = data, tfrmt = tfrmt_test, mock = FALSE) %>%
    names())

})

test_that("Missing Big N in dataset", {
  dat <- tibble::tribble(
    ~group,     ~label,        ~span2,  ~span1,     ~my_col,    ~parm,   ~val,
    "g1", "rowlabel1",  "column cols", "cols 1,2", "col1"  ,  "value",    1,
    "g1", "rowlabel1",  "column cols", "cols 1,2", "col2"  ,  "value",    1,
    "g1", "rowlabel1",             NA,         NA, "mycol3",  "value",    1,
    "g1", "rowlabel1",  "column cols", "col 4"   , "col4"  ,  "value",    1,
    "g1", "rowlabel1",             NA,         NA, "mycol5",  "value",    1,
    "g1", "rowlabel2",  "column cols", "cols 1,2", "col1"  ,  "value",    2,
    "g1", "rowlabel2",  "column cols", "cols 1,2", "col2"  ,  "value",    2,
    "g1", "rowlabel2",             NA,        NA , "mycol3",  "value",    2,
    "g1", "rowlabel2",  "column cols", "col 4"   , "col4"  ,  "value",    2,
    "g1", "rowlabel2",             NA,         NA, "mycol5",  "value",    2,
    "g2", "rowlabel3",  "column cols", "cols 1,2", "col1"  ,  "value",    3,
    "g2", "rowlabel3",  "column cols", "cols 1,2", "col2"  ,  "value",    3,
    "g2", "rowlabel3",             NA,         NA, "mycol3",  "value",    3,
    "g2", "rowlabel3",  "column cols", "col 4"   , "col4"  ,  "value",    3,
    "g2", "rowlabel3",             NA,         NA, "mycol5",  "value",    3,
  )

  tfrmt_test <- tfrmt(
    group = group,
    label = label,
    param = parm,
    value = val,
    column = c(span2, span1, my_col),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("x"))
    ),
    col_plan = col_plan(
      group,
      label,
      starts_with("col")
    ),
    big_n = big_n_structure(param = "bigN")
  )

  expect_warning(apply_tfrmt(.data = dat, tfrmt = tfrmt_test, mock = FALSE))
})

