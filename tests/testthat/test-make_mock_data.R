
test_that("Mock data column names are correct", {

  plan  <- tfrmt(
    group = "my_group",
    label = "my_label",
    param = "param2",
    value = "val2",
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
    value = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  )
  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 1)

  expect_equal(mock_dat,
               tibble::tribble(
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
    value = "val2",
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
               tibble::tribble(
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
    value = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = list(grp1 = ".default", grp2 = c("c","d")), label_val = c("e","f"), mean = frmt("xx.x"))
    )
  )
  mock_dat <- make_mock_data(plan, .default = 1, n_col = 1)

  expect_equal(mock_dat,
               tibble::tribble(
                 ~ grp1  , ~ grp2, ~ my_label, ~ param2, ~ col,
                 "grp1_1", "c"   , "e"       , "mean"  , "col1",
                 "grp1_1", "c"   , "f"       , "mean"  , "col1",
                 "grp1_1", "d"   , "e"       , "mean"  , "col1",
                 "grp1_1", "d"   , "f"       , "mean"  , "col1"
               ))


  # group & label specified + multiple levels/columns
  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 2)

  expect_equal(mock_dat,
               tibble::tribble(
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
    value = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = list(grp1 = ".default", grp2 = c("c","d")), label_val = c("e","f"), mean = frmt("xx.x")),
      frmt_structure(group_val = ".default", label_val = ".default", N = frmt("xx"))
    )
  )
  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 1)

  expect_equal(mock_dat,
               tibble::tribble(
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

  # no duplicate levels introduced if body plan references a dummy level
  plan1  <- tfrmt(
    group = "grp",
    label = "my_label",
    param = "param2",
    value = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x")),
      frmt_structure(group_val = list(grp = "grp_1"), label_val = ".default", frmt("xx.xx")
    )
    )
  )
  mock_dat1 <- make_mock_data(plan1, .default = 1:2, n_col = 1)

  plan2  <- tfrmt(
    group = "grp",
    label = "my_label",
    param = "param2",
    value = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
      )
    )
  mock_dat2 <- make_mock_data(plan2, .default = 1:2, n_col = 1)

  expect_equal(mock_dat1, mock_dat2)
})


test_that("Check mock when value is missing", {
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
  expect_snapshot(
    out <- print_mock_gt(plan, data)
  )
})


test_that("Test when no body_style or values is present", {

  tfrmt_obj_one_span <- tfrmt(
    label = label,
    group = group,
    param = param,
    column = vars(spanner, columns),
    col_plan = col_plan(
      group,label,
      col1, col2,
      span_structure(spanner = "test label", columns= col3),
      vars(col4:col10)
      ),
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc("gtdefault"))
    )

  input_data <- tibble(
    group = "groupvar",
    label = "labels",
    param = "params",
    spanner = "test label",
    columns = paste0("col",1:10)
  )


  expect_message(
    gt_out <- print_mock_gt(tfrmt_obj_one_span, input_data),
    "Message: `tfrmt` will need `value` value to `print_to_gt` when data is avaliable",
    fixed = TRUE
  )

  expect_equal(gt_out$`_data`,
               input_data %>%
                 mutate(val = "X.X") %>%
                 pivot_wider(
                   names_from = c(spanner, columns),
                   names_sep =  .tlang_delim,
                   values_from = val
                   ) %>%
                 select(-param) %>%
                 mutate(..tfrmt_row_grp_lbl=FALSE)
               )


})

test_that("Mock data contains sorting_cols when available", {

  # handle single sorting cols
  plan  <- tfrmt(
    group = vars(grp1, grp2),
    label = "my_label",
    param = "param2",
    value = "val2",
    column = "col",
    sorting_cols = c(ord1),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  )

  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 1)

  expect_equal(mock_dat,
               tibble::tribble(
                 ~grp1,    ~grp2,     ~ my_label,  ~param2,    ~ord1,   ~col,
                 "grp1_1", "grp2_1", "my_label_1", "param2_1",     1, "col1",
                 "grp1_1", "grp2_1", "my_label_2", "param2_1",     1, "col1",
                 "grp1_1", "grp2_2", "my_label_1", "param2_1",     1, "col1",
                 "grp1_1", "grp2_2", "my_label_2", "param2_1",     1, "col1",
                 "grp1_2", "grp2_1", "my_label_1", "param2_1",     1, "col1",
                 "grp1_2", "grp2_1", "my_label_2", "param2_1",     1, "col1",
                 "grp1_2", "grp2_2", "my_label_1", "param2_1",     1, "col1",
                 "grp1_2", "grp2_2", "my_label_2", "param2_1",     1, "col1"
               ))

  # handle 2 sorting cols
  plan  <- tfrmt(
    group = vars(grp1, grp2),
    label = "my_label",
    param = "param2",
    value = "val2",
    column = "col",
    sorting_cols = c(ord1, ord2),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  )

  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 1)

  expect_equal(mock_dat,
               tibble::tribble(
                 ~grp1,    ~grp2,     ~ my_label,  ~param2,    ~ord1, ~ord2,  ~col,
                 "grp1_1", "grp2_1", "my_label_1", "param2_1",     1,     1,"col1",
                 "grp1_1", "grp2_1", "my_label_2", "param2_1",     1,     1,"col1",
                 "grp1_1", "grp2_2", "my_label_1", "param2_1",     1,     1,"col1",
                 "grp1_1", "grp2_2", "my_label_2", "param2_1",     1,     1,"col1",
                 "grp1_2", "grp2_1", "my_label_1", "param2_1",     1,     1,"col1",
                 "grp1_2", "grp2_1", "my_label_2", "param2_1",     1,     1,"col1",
                 "grp1_2", "grp2_2", "my_label_1", "param2_1",     1,     1,"col1",
                 "grp1_2", "grp2_2", "my_label_2", "param2_1",     1,     1,"col1"
               ))

})

test_that("Mock data includes all columns identified in tfrmt", {

  # handle one column
  plan  <- tfrmt(
    group = grp,
    label = "my_label",
    param = "param2",
    value = "val2",
    column = col,
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  )

  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 2)

  expect_equal(mock_dat,
               tibble::tribble(
                 ~grp,   ~ my_label,    ~param2,     ~col,
                 "grp_1", "my_label_1", "param2_1", "col1",
                 "grp_1", "my_label_1", "param2_1", "col2",
                 "grp_1", "my_label_2", "param2_1", "col1",
                 "grp_1", "my_label_2", "param2_1", "col2",
                 "grp_2", "my_label_1", "param2_1", "col1",
                 "grp_2", "my_label_1", "param2_1", "col2",
                 "grp_2", "my_label_2", "param2_1", "col1",
                 "grp_2", "my_label_2", "param2_1", "col2"
               ))

  # handle two columns
  plan  <- tfrmt(
    group = grp,
    label = "my_label",
    param = "param2",
    value = "val2",
    column = c(col1,col2),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  )

  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 2)

  expect_equal(mock_dat,
               tibble::tribble(
                    ~grp,   ~ my_label,    ~param2,       ~col1,  ~col2,
                 "grp_1", "my_label_1", "param2_1", "span_col1", "col21",
                 "grp_1", "my_label_1", "param2_1", "span_col1", "col22",
                 "grp_1", "my_label_2", "param2_1", "span_col1", "col21",
                 "grp_1", "my_label_2", "param2_1", "span_col1", "col22",
                 "grp_2", "my_label_1", "param2_1", "span_col1", "col21",
                 "grp_2", "my_label_1", "param2_1", "span_col1", "col22",
                 "grp_2", "my_label_2", "param2_1", "span_col1", "col21",
                 "grp_2", "my_label_2", "param2_1", "span_col1", "col22"
               ))

  # handle three columns
  plan  <- tfrmt(
    group = grp,
    label = "my_label",
    param = "param2",
    value = "val2",
    column = c(col1,col2,col3),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  )

  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 2)

  expect_equal(mock_dat,
               tibble::tribble(
                    ~grp,   ~ my_label,    ~param2,       ~col1,       ~col2,  ~col3,
                 "grp_1", "my_label_1", "param2_1", "span_col1", "span_col2", "col31",
                 "grp_1", "my_label_1", "param2_1", "span_col1", "span_col2", "col32",
                 "grp_1", "my_label_2", "param2_1", "span_col1", "span_col2", "col31",
                 "grp_1", "my_label_2", "param2_1", "span_col1", "span_col2", "col32",
                 "grp_2", "my_label_1", "param2_1", "span_col1", "span_col2", "col31",
                 "grp_2", "my_label_1", "param2_1", "span_col1", "span_col2", "col32",
                 "grp_2", "my_label_2", "param2_1", "span_col1", "span_col2", "col31",
                 "grp_2", "my_label_2", "param2_1", "span_col1", "span_col2", "col32"
               ))

})

test_that("Printing Mock data removes value when it exists in the input data",{

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
  ) %>%
    mutate(
      value_to_remove = 1
    )

  plan <- tfrmt(
    label = "label",
    column = "column",
    param = "param",
    value = "value_to_remove",
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
  expect_message(
    print_mock_gt(plan, data),
    "Removing `value_to_remove` from input data for mocking.",
    fixed = TRUE
  )


})

test_that("Mock data can be made and printed without label",{


  plan <- tfrmt(
    column = "column",
    param = "param",
    value = "value_to_remove",
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

  dat <- make_mock_data(plan)

  expect_equal(
    dat,
    tibble(
      param = c("n","n","n","percent","percent","percent"),
      column = paste0("column", rep(1:3, times = 2))
    )
  )

  #Make mock
  expect_silent(
    print_mock_gt(plan, .data = dat)
  )

})


test_that("Mock data can be printed from a tfrmt without a body plan",{


  plan <- tfrmt(
    column = "column",
    param = "param",
    value = "value_to_remove",
    title = "Summary of Populations"
  )

  mock_gt <- print_mock_gt(plan)

  #Make mock
  expect_equal(
    mock_gt,
    print_mock_gt(plan, .data = tibble::tibble(column = c("column1","column2","column3"), param = "n")),
    ignore_function_env = TRUE,
    ignore_attr = TRUE
  )


})

test_that("Using col_plan to get column names", {
  # Without any spanning
  basic_cols <- tfrmt(
    group = "group",
    label = "label",
    column = "column",
    param = "param",
    value = "value",
    sorting_cols = c(ord1, ord2),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("X.X"))
    ),
    col_plan = col_plan(
      group, label,
      "Placebo",
      new_name = "Low",
      contains("High")
    ))

  col_names <- make_mock_data(basic_cols) %>%
    pull(column) %>%
    unique()
  expect_equal(col_names, c("Placebo", "Low", "High"))

  #With spanning
  auto_col_df <- tfrmt(
    group = group,
    label = quo(label),
    param = parm,
    column = c(test1,test2),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("X.X"))
    ),
    col_plan = col_plan(
      group,
      label,
      col4,
      span_structure(test1 = `span 1`, test2 = c(col1, contains("col2"))),
      span_structure(test1 = `span 2`, test2 = c(col7, col8)),
      col3,
      -col5
    )
  ) %>%
    make_mock_data() %>%
    select(test1, test2) %>%
    distinct(test1, test2)

  man_col_df <- tibble(test1 = c(rep(NA, 3), rep(c("span 1", "span 2"), each = 2)),
                       test2 = c("col4","col3", "col5", "col1", "col2", "col7","col8"))
  expect_equal(auto_col_df, man_col_df)


# When you do crossing in the span structure
  auto_col_crossing <- tfrmt(
    group = group,
    label = quo(label),
    param = parm,
    column = c(visit,trt),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("X.X"))
    ),
  col_plan = col_plan(
    model_results_category,
    measure,
    span_structure(
      visit = c(`Week 4`,`Week 8`, `Week 12`),
      trt = c(`Placebo`,`GSK123456 100 mg`)
    ),
    -starts_with("ord")
  )) %>%
    make_mock_data() %>%
    distinct(trt, visit)

  man_col_crossing <- tibble::tribble(
    ~trt ,                   ~visit ,
    "model_results_category", NA_character_,
    "measure",                NA_character_,
    "ord",                    NA_character_,
    "GSK123456 100 mg",       "Week 12",
    "Placebo",                "Week 12",
    "GSK123456 100 mg",       "Week 4",
    "Placebo",                "Week 4",
    "GSK123456 100 mg",       "Week 8",
    "Placebo",                "Week 8",
  )

  expect_equal(auto_col_crossing, man_col_crossing)



})

test_that("Will add big N avaliable", {
  pop_tbl_tfrmt <- tfrmt(
    column = TRT01A,
    label = name,
    param = param,
    value = value,
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default",
                     frmt_combine("{n} ({pct}%)",
                                  n = frmt("xx"),
                                  pct = frmt("xx")))
    ),
    big_n = big_n_structure(param_val = "big_n"),
    col_plan = col_plan(
      starts_with("Xanomeline"),
      "Placebo",
      "Total"
    )
  )

  auto_big_n_df <- make_mock_data(pop_tbl_tfrmt) %>%
    filter(param == "big_n")

  man_big_n_df <- tibble::tribble(
    ~name,  ~param,  ~TRT01A,
    NA_character_,    "big_n", "Xanomeline",
    NA_character_,    "big_n", "Placebo"   ,
    NA_character_,    "big_n", "Total"
  )

  expect_equal(auto_big_n_df, man_big_n_df)

})

test_that("Mock data for col_plan with only drops", {

  drop_tfrmt <- tfrmt(
    # specify columns in the data
    group = c(rowlbl1,grp),
    label = rowlbl2,
    column = column,
    param = "param",
    value = value,
    sorting_cols = c(ord1, ord2),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default",
                     frmt_combine("{n} ({pct}%)",
                                  n = frmt("xx"),
                                  pct = frmt("xx")))
    ),
    # remove extra cols
    col_plan = col_plan(-grp,
                        -starts_with("ord") )
  )

  make_mock_data(drop_tfrmt) %>%
    pull(column) %>%
    unique() %>%
    expect_equal(c("column1", "column2", "column3"))
})

test_that("Mock data for col_plan does not add group, label, or sorting_cols names to `column` variable", {


  tf_cols <- tfrmt(
    group="grp",
    label="lbl",
    param="prm",
    column="col",
    value="val",
    sorting_cols = c("ord1","ord2"),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    ),
    col_plan = col_plan(
      grp,
      lbl,
      col1,
      col2,
      -ord1,
      -ord2
    )
  )

  make_mock_data(tf_cols) %>%
    pull(col) %>%
    unique() %>%
    expect_equal(c("col1","col2"))

})

test_that("Mock data ignores col_plan with everything()",{

  tf_everything <- tfrmt(
    group = "group",
    label = "label",
    column = "column",
    param = "param",
    value = "value",
    sorting_cols = c(ord1, ord2),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("X.X"))
    ),
    col_plan = col_plan(
      -Total,
      everything()
    ))

  col_names <- make_mock_data(tf_everything) %>%
    pull(column) %>%
    unique()
  expect_equal(col_names, c("column1","column2","column3"))
})
