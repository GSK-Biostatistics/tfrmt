
test_that("Mock data column names are correct", {

  plan  <- tfrmt(
    group = "my_group",
    label = "my_label",
    param = "param2",
    values = "val2",
    column = "col",
    body_style = table_body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  )
  mock_dat <- make_mock_data(plan)

  expect_equal(c("my_group", "my_label", "param2", "col"),
               names(mock_dat))
})

test_that("Mock data contains all levels", {

  # handle multiple group vars
  plan  <- tfrmt(
    group = vars(grp1, grp2),
    label = "my_label",
    param = "param2",
    values = "val2",
    column = "col",
    body_style = table_body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  )
  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 1)

  expect_equal(mock_dat,
               tribble(
                 ~grp1,   ~grp2,  ~ my_label, ~param2, ~col ,
                 "group1", "group1", "label1",  "param1", "col1" ,
                 "group1", "group1", "label2",   "param1", "col1" ,
                 "group1", "group2", "label1",   "param1", "col1",
                 "group1", "group2", "label2",   "param1", "col1",
                 "group2", "group1", "label1",   "param1", "col1",
                 "group2", "group1", "label2",   "param1", "col1",
                 "group2", "group2", "label1",   "param1", "col1",
                 "group2", "group2", "label2",   "param1", "col1"
               ))

  # group & label values specified
  plan  <- tfrmt(
    group = vars(grp1, grp2),
    label = "my_label",
    param = "param2",
    values = "val2",
    column = "col",
    body_style = table_body_plan(
      frmt_structure(group_val = list(grp1 = ".default", grp2 = c("c","d")), label_val = c("e","f"), mean = frmt("xx.x"))
    )
  )
  mock_dat <- make_mock_data(plan, .default = 1, n_col = 1)

  expect_equal(mock_dat,
               tribble(
                ~ grp1  , ~ grp2, ~ my_label, ~ param2, ~ col,
                "group1", "c"   , "e"       , "mean"  , "col1",
                "group1", "c"   , "f"       , "mean"  , "col1",
                "group1", "d"   , "e"       , "mean"  , "col1",
                "group1", "d"   , "f"       , "mean"  , "col1"
               ))


  # group & label specified + multiple levels/columns
  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 2)

  expect_equal(mock_dat,
               tribble(
                 ~ grp1  , ~ grp2, ~ my_label, ~ param2, ~ col,
                 "group1", "c"   , "e"       , "mean"  , "col1",
                 "group1", "c"   , "e"       , "mean"  , "col2",
                 "group1", "c"   , "f"       , "mean"  , "col1",
                 "group1", "c"   , "f"       , "mean"  , "col2",
                 "group1", "d"   , "e"       , "mean"  , "col1",
                 "group1", "d"   , "e"       , "mean"  , "col2",
                 "group1", "d"   , "f"       , "mean"  , "col1",
                 "group1", "d"   , "f"       , "mean"  , "col2",
                 "group2", "c"   , "e"       , "mean"  , "col1",
                 "group2", "c"   , "e"       , "mean"  , "col2",
                 "group2", "c"   , "f"       , "mean"  , "col1",
                 "group2", "c"   , "f"       , "mean"  , "col2",
                 "group2", "d"   , "e"       , "mean"  , "col1",
                 "group2", "d"   , "e"       , "mean"  , "col2",
                 "group2", "d"   , "f"       , "mean"  , "col1",
                 "group2", "d"   , "f"       , "mean"  , "col2"
               ))

  # multiple frmt_structure
  plan  <- tfrmt(
    group = vars(grp1, grp2),
    label = "my_label",
    param = "param2",
    values = "val2",
    column = "col",
    body_style = table_body_plan(
      frmt_structure(group_val = list(grp1 = ".default", grp2 = c("c","d")), label_val = c("e","f"), mean = frmt("xx.x")),
      frmt_structure(group_val = ".default", label_val = ".default", N = frmt("xx"))
    )
  )
  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 1)

  expect_equal(mock_dat,
               tribble(
                 ~ grp1  , ~ grp2  , ~ my_label, ~ param2, ~ col,
                 "group1", "c"     , "e"       , "mean"  , "col1",
                 "group1", "c"     , "f"       , "mean"  , "col1",
                 "group1", "d"     , "e"       , "mean"  , "col1",
                 "group1", "d"     , "f"       , "mean"  , "col1",
                 "group2", "c"     , "e"       , "mean"  , "col1",
                 "group2", "c"     , "f"       , "mean"  , "col1",
                 "group2", "d"     , "e"       , "mean"  , "col1",
                 "group2", "d"     , "f"       , "mean"  , "col1",
                 "group1", "group1", "label1"  , "N"     , "col1",
                 "group1", "group1", "label2"  , "N"     , "col1",
                 "group1", "group2", "label1"  , "N"     , "col1",
                 "group1", "group2", "label2"  , "N"     , "col1",
                 "group2", "group1", "label1"  , "N"     , "col1",
                 "group2", "group1", "label2"  , "N"     , "col1",
                 "group2", "group2", "label1"  , "N"     , "col1",
                 "group2", "group2", "label2"  , "N"     , "col1"
               ))
})
