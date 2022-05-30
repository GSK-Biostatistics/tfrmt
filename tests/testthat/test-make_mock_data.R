
test_that("Mock data column names are correct", {

  plan  <- tfrmt(
    group = "my_group",
    label = "my_label",
    param = "param2",
    values = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  )
  mock_dat <- make_mock_data(plan)


  expect_equal(c("my_group", "my_label", "param2", "col"),
               names(mock_dat))
})

test_that("Mock data contains all levels", {

  # handle 2 group vars
  plan  <- tfrmt(
    group = vars(grp1, grp2),
    label = "my_label",
    param = "param2",
    values = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  )
  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 1)

  expect_equal(mock_dat,
               tribble(
                 ~grp1,    ~grp2,     ~ my_label,  ~param2,  ~col ,
                 "grp1_1", "grp2_1", "my_label_1", "param2_1", "col1" ,
                 "grp1_1", "grp2_1", "my_label_2", "param2_1", "col1" ,
                 "grp1_1", "grp2_2", "my_label_1", "param2_1", "col1",
                 "grp1_1", "grp2_2", "my_label_2", "param2_1", "col1",
                 "grp1_2", "grp2_1", "my_label_1", "param2_1", "col1",
                 "grp1_2", "grp2_1", "my_label_2", "param2_1", "col1",
                 "grp1_2", "grp2_2", "my_label_1", "param2_1", "col1",
                 "grp1_2", "grp2_2", "my_label_2", "param2_1", "col1"
               ))

  # handle many group vars
  plan  <- tfrmt(
    group = vars(grp1, grp2, grp3, grp4),
    label = "my_label",
    param = "param2",
    values = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = list(grp1 = "A", grp2 = c("a","b")), label_val = ".default", frmt("xx.x")),
      frmt_structure(group_val = list(grp1 = "B", grp2 = c("a","b")), label_val = ".default", frmt("xx.x")),
      frmt_structure(group_val = list(grp3 = "C", grp4 = c("a","b")), label_val = ".default", frmt("xx.x")),
      frmt_structure(group_val = list(grp3 = "D", grp4 = c("a","b")), label_val = ".default", frmt("xx.x"))
    )
  )
  mock_dat <- make_mock_data(plan, .default = 1, n_col = 1)

  expect_equal(mock_dat,
               tribble(
                 ~grp1,    ~grp2  ,  ~grp3   , ~grp4   ,  ~my_label  , ~param2   ,~col,
                 "A"     , "a"      ,"grp3_1" ,"grp4_1" ,"my_label_1" ,"param2_1" ,"col1" ,
                 "A"     , "b"      ,"grp3_1" ,"grp4_1" ,"my_label_1" ,"param2_1" ,"col1" ,
                 "B"     , "a"      ,"grp3_1" ,"grp4_1" ,"my_label_1" ,"param2_1" ,"col1" ,
                 "B"     , "b"      ,"grp3_1" ,"grp4_1" ,"my_label_1" ,"param2_1" ,"col1" ,
                 "grp1_1", "grp2_1" ,"C"      ,"a"      ,"my_label_1" ,"param2_1" ,"col1" ,
                 "grp1_1", "grp2_1" ,"C"      ,"b"      ,"my_label_1" ,"param2_1" ,"col1" ,
                 "grp1_1", "grp2_1" ,"D"      ,"a"      ,"my_label_1" ,"param2_1" ,"col1" ,
                 "grp1_1" ,"grp2_1" ,"D"      ,"b"      ,"my_label_1" ,"param2_1" ,"col1",
               ))




  # group & label values specified
  plan  <- tfrmt(
    group = vars(grp1, grp2),
    label = "my_label",
    param = "param2",
    values = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = list(grp1 = ".default", grp2 = c("c","d")), label_val = c("e","f"), mean = frmt("xx.x"))
    )
  )
  mock_dat <- make_mock_data(plan, .default = 1, n_col = 1)

  expect_equal(mock_dat,
               tribble(
                 ~ grp1  , ~ grp2, ~ my_label, ~ param2, ~ col,
                 "grp1_1", "c"   , "e"       , "mean"  , "col1",
                 "grp1_1", "c"   , "f"       , "mean"  , "col1",
                 "grp1_1", "d"   , "e"       , "mean"  , "col1",
                 "grp1_1", "d"   , "f"       , "mean"  , "col1"
               ))


  # group & label specified + multiple levels/columns
  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 2)

  expect_equal(mock_dat,
               tribble(
                 ~ grp1  , ~ grp2, ~ my_label, ~ param2, ~ col,
                 "grp1_1", "c"   , "e"       , "mean"  , "col1",
                 "grp1_1", "c"   , "e"       , "mean"  , "col2",
                 "grp1_1", "c"   , "f"       , "mean"  , "col1",
                 "grp1_1", "c"   , "f"       , "mean"  , "col2",
                 "grp1_1", "d"   , "e"       , "mean"  , "col1",
                 "grp1_1", "d"   , "e"       , "mean"  , "col2",
                 "grp1_1", "d"   , "f"       , "mean"  , "col1",
                 "grp1_1", "d"   , "f"       , "mean"  , "col2",
                 "grp1_2", "c"   , "e"       , "mean"  , "col1",
                 "grp1_2", "c"   , "e"       , "mean"  , "col2",
                 "grp1_2", "c"   , "f"       , "mean"  , "col1",
                 "grp1_2", "c"   , "f"       , "mean"  , "col2",
                 "grp1_2", "d"   , "e"       , "mean"  , "col1",
                 "grp1_2", "d"   , "e"       , "mean"  , "col2",
                 "grp1_2", "d"   , "f"       , "mean"  , "col1",
                 "grp1_2", "d"   , "f"       , "mean"  , "col2"
               ))

  # multiple frmt_structure
  plan  <- tfrmt(
    group = vars(grp1, grp2),
    label = "my_label",
    param = "param2",
    values = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = list(grp1 = ".default", grp2 = c("c","d")), label_val = c("e","f"), mean = frmt("xx.x")),
      frmt_structure(group_val = ".default", label_val = ".default", N = frmt("xx"))
    )
  )
  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 1)

  expect_equal(mock_dat,
               tribble(
                 ~ grp1  , ~ grp2  , ~ my_label    , ~ param2, ~ col,
                 "grp1_1", "c"     , "e"           , "mean"  , "col1",
                 "grp1_1", "c"     , "f"           , "mean"  , "col1",
                 "grp1_1", "d"     , "e"           , "mean"  , "col1",
                 "grp1_1", "d"     , "f"           , "mean"  , "col1",
                 "grp1_2", "c"     , "e"           , "mean"  , "col1",
                 "grp1_2", "c"     , "f"           , "mean"  , "col1",
                 "grp1_2", "d"     , "e"           , "mean"  , "col1",
                 "grp1_2", "d"     , "f"           , "mean"  , "col1",
                 "grp1_1", "grp2_1", "my_label_1"  , "N"     , "col1",
                 "grp1_1", "grp2_1", "my_label_2"  , "N"     , "col1",
                 "grp1_1", "grp2_2", "my_label_1"  , "N"     , "col1",
                 "grp1_1", "grp2_2", "my_label_2"  , "N"     , "col1",
                 "grp1_2", "grp2_1", "my_label_1"  , "N"     , "col1",
                 "grp1_2", "grp2_1", "my_label_2"  , "N"     , "col1",
                 "grp1_2", "grp2_2", "my_label_1"  , "N"     , "col1",
                 "grp1_2", "grp2_2", "my_label_2"  , "N"     , "col1"
               ))
})


test_that("Check mock when value it missing", {
  data <- crossing(
    label = c("Intent-To-Treat (ITT)",
              "Safety",
              "Efficacy",
              "Complete Week 24",
              "Complete Study"
    ),
    column = c("Placebo\n(N=XX)",
               "Xanomeline\nLow Dose\n(N=XX)",
               "Xanomeline\nHigh Dose\n(N=XX)",
               "Total\n(N=XXX)"
    ),
    param = c("n", "percent")
  )

  plan <- tfrmt(
    label = "label",
    column = "column",
    param = "param",
    title = "Summary of Populations",
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default", label_val = ".default",
        frmt_combine("{n} ({percent}%)",
                     n = frmt("xx"),
                     percent = frmt("xxx"))
      )
    )
  )

  #Make mock
  quietly(print_mock_gt)(plan, data)$warnings %>%
    expect_equal(character())
})


test_that("Test when no body_style is present", {
  tfrmt_obj_one_span <- tfrmt(
    label = label,
    group = group,
    param = param,
    column = vars(columns),
    col_plan = col_plan(
      group,label,
      col1, col2,
      span_structure("test label", col3),
      vars(col4:col10)
      ))

  input_data <- tibble(
    group = "groupvar",
    label = "labels",
    param = "params",
    columns = paste0("col",1:10)
  )

  gt_out <- print_mock_gt(tfrmt_obj_one_span, input_data)

  expect_equal(gt_out$`_data`,
               input_data %>%
                 mutate(val = "X.X") %>%
                 pivot_wider(names_from = columns, values_from = val) %>%
                 select(-param) %>%
                 rename(`test label___tlang_delim___col3` = col3))


})


