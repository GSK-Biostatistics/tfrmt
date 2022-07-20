
test_that("Defining the spanning structure", {

  s1 <- span_structure(
    label = "Test Label",
    vars(A,B)
  )

  s2 <- span_structure(
    label = "Test Label",
    span_structure(
      label = "Test Sub Label",
      vars(A,B)
    ),
    vars(C,D)
  )

  expect_s3_class(s1,"span_structure")
  expect_s3_class(s2,"span_structure")
  expect_s3_class(s2,"span_structures")

  expect_equal(s1$label, "Test Label")
  expect_equal(s2$label, "Test Label")

  expect_equal(s1$span_cols, list(vars(A,B)), ignore_attr = TRUE)
  expect_equal(s2$span_cols, list(span_structure(label = "Test Sub Label",vars(A,B)),vars(C,D)), ignore_attr = TRUE)

})

test_that("Defining the col plan", {

  s1 <- col_plan(
    span_structure(
      label = "Test Label",
      vars(A,B)
    ))

  expect_s3_class(s1,"col_plan")
  expect_s3_class(s1,"plan")

  #Go into object
  tfrmt_test <- tfrmt(column = vars(columns),
                      col_plan = col_plan(span_structure("test label", vars(col3))))

  expect_true(!is.null(tfrmt_test$col_plan))

  # expect error when writing invalid col_plan
    expect_error(
      tfrmt(
        col_plan = col_plan(
      group,label,
      col1, col2,
      span_structure("test label", col3),
      col4:col10
    )))

})

test_that("From the spanning structure get cols to span across", {

  s1 <- span_structure(
    label = "Test Label",
    vars(A,B)
  )


  s2 <- span_structure(
    label = "Test Label",
    span_structure(
      label = "Test Sub Label",
      vars(A,B)
    ),
    vars(C,D)
  )

  s3 <- span_structure(
    label = "Test Label",
    vars(D),
    span_structure(
      label = "Test Sub Label",
      vars(C,A)
    ),
    vars(B)
  )

  s4 <- span_structure(
    label = "Test Label",
    vars(-A)
  )


  sample_df <- data.frame(A = character(), B = character(), C = character(), D = character())

  s1_cols <- eval_tidyselect_on_colvec(s1, column_vec = c("A","B","C","D"))
  s2_cols <- eval_tidyselect_on_colvec(s2, column_vec = c("A","B","C","D"))
  s3_cols <- eval_tidyselect_on_colvec(s3, column_vec = c("A","B","C","D"))
  s4_cols <- eval_tidyselect_on_colvec(s4, column_vec = c("A","B","C","D"))

  expect_equal(s1_cols,c("A","B"))
  expect_equal(s2_cols,c("A","B","C","D"))
  expect_equal(s3_cols,c("D","C","A","B"))
  expect_equal(s4_cols,c("B","C","D"))

})

test_that("Order is kept for multi-col columns",{
  test <- tibble(col_1 = "test",
                 col_2 = c("this", "other"),
                 col_3 = c("delm", "delm"),
                 label = "label",
                 param = "pam",
                 value = c(1.08089, 9.23948))


  tfrmt<-tfrmt(
    label = label,
    param = param,
    values = value,
    column = vars(col_1, col_2, col_3),
    body_plan = body_plan(
      frmt_structure(pam = frmt("x.xx"))
    ),
    col_style_plan = col_style_plan(element_col(align = ".", col = vars(delm)),
                               element_col(align = "right"))
  )


  new_name_ord <- apply_tfrmt(test, tfrmt) %>%
    select(-label) %>%
    names()

  new_name_ord_in_dat <- test %>%
    select(starts_with("col")) %>%
    unite("new", sep=.tlang_delim) %>%
    pull(new)

  expect_equal(new_name_ord, new_name_ord_in_dat)
})

test_that("From col plan spanning structures, get df to add to data",{

  tfrmt_obj_one_span <- tfrmt(
    label = label,
    group = group,
    param = param,
    column = vars(columns),
    col_plan = col_plan(span_structure("test label",
                                       col3)))

  tfrmt_obj_one_span_rename <- tfrmt(
    label = label,
    group = group,
    param = param,
    column = vars(columns),
    col_plan = col_plan(span_structure("test label",
                                       new_col3 = col3)))

  tfrmt_obj_nested_spans <- tfrmt(label = label,
                                  group = group,
                                  param = param,
                                  column = vars(columns),
                                  col_plan = col_plan(
                                    span_structure(
                                      "test label1",
                                      span_structure("test label1.1",
                                                     vars(col1),
                                                     span_structure("test label1.1.1",
                                                                    vars(col2))),
                                      vars(col3),
                                      span_structure("test label1.2", vars(col5))
                                    ),
                                    span_structure("test label2", vars(col7)),
                                    starts_with("col")
                                  )
  )

  tfrmt_obj_nested_spans_rename <- tfrmt(label = label,
                                         group = group,
                                         param = param,
                                         column = vars(columns),
                                         col_plan = col_plan(
                                           span_structure(
                                             "test label1",
                                             span_structure("test label1.1",
                                                            vars(col1),
                                                            span_structure("test label1.1.1",
                                                                           vars(col2))),
                                             new_col3 = col3,
                                             span_structure("test label1.2", vars(col5))
                                           ),
                                           span_structure("test label2", vars(col7)),
                                           starts_with("col")
                                         )
  )

  input_data <- tibble(
    group = "groupvar",
    label = "labels",
    param = "params",
    columns = paste0("col",1:10)
  )

  one_span_wide_data <- apply_span_structures_to_data(tfrmt_obj = tfrmt_obj_one_span, x = input_data)

  expect_equal(
    one_span_wide_data,
    tibble(
      `__tlang_span_structure_column__1` = c(NA, NA, "test label", rep(NA, 7)),
      columns = paste0("col",1:10),
      group = "groupvar",
      label = "labels",
      param = "params",
    )
  )

  one_span_wide_data_rename <- apply_span_structures_to_data(tfrmt_obj = tfrmt_obj_one_span_rename, x = input_data)

  expect_equal(
    one_span_wide_data_rename,
    tibble(
      `__tlang_span_structure_column__1` = c(NA, NA, "test label", rep(NA, 7)),
      columns = paste0("col",1:10),
      group = "groupvar",
      label = "labels",
      param = "params",
    )
  )

  nested_spans_wide_data <- apply_span_structures_to_data(tfrmt_obj = tfrmt_obj_nested_spans, x = input_data)

  output_data <- tibble(
    `__tlang_span_structure_column__1` = c("test label1", "test label1", "test label1", NA, "test label1", NA, "test label2", NA, NA, NA),
    `__tlang_span_structure_column__2` = c("test label1.1", "test label1.1", NA, NA, "test label1.2", NA, NA, NA, NA, NA),
    `__tlang_span_structure_column__3` = c(NA, "test label1.1.1", rep(NA, 8)),
    columns = paste0("col",1:10),
    group = "groupvar",
    label = "labels",
    param = "params",
  )

  expect_equal(
    nested_spans_wide_data,
    output_data
  )

  nested_spans_wide_data_rename <- apply_span_structures_to_data(tfrmt_obj = tfrmt_obj_nested_spans_rename, x = input_data)

  expect_equal(
    nested_spans_wide_data_rename,
    output_data
  )

})

test_that("Build simple tfrmt with multiple columns and apply to basic data and compare against spanning_structure",{

  basic_multi_column_template <- tfrmt(
    group = group,
    label = quo(label),
    param = parm,
    values = val,
    column = c(test1,test2),
    col_plan = col_plan(
      group, label, col4, col1, col2, col3, -col5
    )
  )

  spanned_column_template <- tfrmt(
    group = group,
    label = quo(label),
    param = parm,
    values = val,
    column = c(test2),
    col_plan = col_plan(
      group, label, col4, span_structure("span 1",col1, col2), col3, -col5
    )
  )

  basic_example_dataset <- tibble::tribble(
    ~group,     ~label,    ~test1, ~test2,    ~parm, ~val,
    "g1", "rowlabel1",  "span 1", "col1",  "value",    1,
    "g1", "rowlabel1",  "span 1", "col2",  "value",    1,
    "g1", "rowlabel1",        NA, "col3",  "value",    1,
    "g1", "rowlabel1",        NA, "col4",  "value",    1,
    "g1", "rowlabel1",        NA, "col5",  "value",    1,
    "g1", "rowlabel2",  "span 1", "col1",  "value",    2,
    "g1", "rowlabel2",  "span 1", "col2",  "value",    2,
    "g1", "rowlabel2",        NA, "col3",  "value",    2,
    "g1", "rowlabel2",        NA, "col4",  "value",    2,
    "g1", "rowlabel2",        NA, "col5",  "value",    2,
    "g2", "rowlabel3",  "span 1", "col1",  "value",    3,
    "g2", "rowlabel3",  "span 1", "col2",  "value",    3,
    "g2", "rowlabel3",        NA, "col3",  "value",    3,
    "g2", "rowlabel3",        NA, "col4",  "value",    3,
    "g2", "rowlabel3",        NA, "col5",  "value",    3,
  )

  suppressMessages({
    processed_gt <- print_to_gt(tfrmt = basic_multi_column_template, .data = basic_example_dataset)
    processed_gt_2 <- print_to_gt(tfrmt = spanned_column_template, .data = basic_example_dataset %>% select(-test1))
  })

  expect_equal(
    processed_gt,
    processed_gt_2
  )

})

test_that("Build simple tfrmt with multiple columns and apply to basic data and compare against spanning_structure - with renaming",{

  basic_multi_column_template <- tfrmt(
    group = group,
    label = quo(label),
    param = parm,
    values = val,
    column = c(test1,test2),
    col_plan = col_plan(
      group, label, new_col_4 = col4, new_col_1 = col1, col2, col3, -col5
    )
  )

  spanned_column_template <- tfrmt(
    group = group,
    label = quo(label),
    param = parm,
    values = val,
    column = c(test2),
    col_plan = col_plan(
      group, label, new_col_4 = col4, span_structure("span 1", new_col_1 = col1, col2), col3, -col5
    )
  )

  basic_example_dataset <- tibble::tribble(
    ~group,     ~label,    ~test1, ~test2,    ~parm, ~val,
    "g1", "rowlabel1",  "span 1", "col1",  "value",    1,
    "g1", "rowlabel1",  "span 1", "col2",  "value",    1,
    "g1", "rowlabel1",        NA, "col3",  "value",    1,
    "g1", "rowlabel1",        NA, "col4",  "value",    1,
    "g1", "rowlabel1",        NA, "col5",  "value",    1,
    "g1", "rowlabel2",  "span 1", "col1",  "value",    2,
    "g1", "rowlabel2",  "span 1", "col2",  "value",    2,
    "g1", "rowlabel2",        NA, "col3",  "value",    2,
    "g1", "rowlabel2",        NA, "col4",  "value",    2,
    "g1", "rowlabel2",        NA, "col5",  "value",    2,
    "g2", "rowlabel3",  "span 1", "col1",  "value",    3,
    "g2", "rowlabel3",  "span 1", "col2",  "value",    3,
    "g2", "rowlabel3",        NA, "col3",  "value",    3,
    "g2", "rowlabel3",        NA, "col4",  "value",    3,
    "g2", "rowlabel3",        NA, "col5",  "value",    3,
  )

  suppressMessages({
      processed_gt <- print_to_gt(tfrmt = basic_multi_column_template, .data = basic_example_dataset)
      processed_gt_2 <- print_to_gt(tfrmt = spanned_column_template, .data = basic_example_dataset %>% select(-test1))
  })


  expect_equal(
    processed_gt,
    processed_gt_2
  )

})

test_that("Build simple tfrmt with multiple columns and with renaming duplicated colnames across spans",{

  multi_col_df <- tibble::tribble(
    ~label, ~col0, ~col1, ~col2, ~param, ~value,
    "A",    "A_",  "A",    "AA",  "p1", 123,
    "A",    "A_",  "A",    "AB",  "p1", 12,
    "A",    "A_",  "B",    "AA",  "p1", 1,
    "A",    "A_",  "B",    "AB",  "p1", 123,
    "A",    "B_",  "C",    "AA",  "p1", 12,
    "A",    "B_",  "C",    "AB",  "p1", 1,
    "A",    "B_",  "D",    "BB",  "p1", 123,
    "A",     NA,   NA,    "BB",  "p1", 12,
    "B",    "A_",  "A",    "AA",  "p1", 123,
    "B",    "A_",  "A",    "AB",  "p1", 12,
    "B",    "A_",  "B",    "AA",  "p1", 1,
    "B",    "A_",  "B",    "AB",  "p1", 123,
    "B",    "B_",  "C",    "AA",  "p1", 12,
    "B",    "B_",  "C",    "AB",  "p1", 1,
    "B",    "B_",  "D",    "BB",  "p1", 123,
    "B",     NA,   NA,    "BB",  "p1", 12,
    "B",     NA,   NA,    "CC",  "p1", 12
  )

  multi_column_template <- tfrmt(
    label = label,
    column = c(col0, col1, col2),
    param = param,
    value = value,
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("XXXX"))
    ),
    col_plan = col_plan(
      label,
      starts_with("A_"),
      renamed_A = AA,
      AB,
      renamed_BB = BB,
      renamed_CC = CC
      )
    )

  multi_column_template_spanners <- tfrmt(
    label = label,
    column = col_unite,
    param = param,
    value = value,
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("XXXX"))
    ),
    col_plan = col_plan(
      label,
      span_structure(
        "A_",
        span_structure("A",
                       renamed_A = A___A__AA,
                       AB = A___A__AB),
        span_structure("B",
                       renamed_A = A___B__AA,
                       AB = A___B__AB)
      ),
      span_structure(
        "B_",
        span_structure("C",
                       renamed_A = B___C__AA,
                       AB = B___C__AB
        ),
        span_structure("D",
                       renamed_BB = B___D__BB
        )
      ),
      renamed_BB = BB,
      renamed_CC = CC
      )
  )

  suppressMessages({
      processed_gt <- print_to_gt(tfrmt = multi_column_template, .data = multi_col_df)
      processed_gt_2 <- print_to_gt(tfrmt = multi_column_template_spanners, .data = multi_col_df %>% unite(col0:col2,col = col_unite, sep = "__",remove = TRUE,na.rm = TRUE))
  })


  expect_equal(
    processed_gt,
    processed_gt_2
  )

})

test_that("Build simple tfrmt with spans with child spans that are and are not spanned",{

  dat <- tibble::tribble(
    ~group,      ~label,     ~top_span, ~child_span,  ~my_col,    ~parm, ~val,
    "g1", "rowlabel1", "column cols",  "cols 1,2",   "col1",  "value",    1,
    "g1", "rowlabel1", "column cols",  "cols 1,2",   "col2",  "value",    1,
    "g1", "rowlabel1",     "my cols",          NA, "mycol3",  "value",    1,
    "g1", "rowlabel1", "column cols",          NA,   "col4",  "value",    1,
    "g1", "rowlabel1",     "my cols",          NA, "mycol5",  "value",    1,
    "g1", "rowlabel2", "column cols",  "cols 1,2",   "col1",  "value",    2,
    "g1", "rowlabel2", "column cols",  "cols 1,2",   "col2",  "value",    2,
    "g1", "rowlabel2",     "my cols",          NA, "mycol3",  "value",    2,
    "g1", "rowlabel2", "column cols",          NA,   "col4",  "value",    2,
    "g1", "rowlabel2",     "my cols",          NA, "mycol5",  "value",    2,
    "g2", "rowlabel3", "column cols",  "cols 1,2",   "col1",  "value",    3,
    "g2", "rowlabel3", "column cols",  "cols 1,2",   "col2",  "value",    3,
    "g2", "rowlabel3",     "my cols",          NA, "mycol3",  "value",    3,
    "g2", "rowlabel3", "column cols",          NA,   "col4",  "value",    3,
    "g2", "rowlabel3",     "my cols",          NA, "mycol5",  "value",    3
  )

  tfrmt_with_parial_child_span <- tfrmt(
    group = group,
    label = label,
    param = parm,
    values = val,
    column = my_col,
    col_plan = col_plan(
      group,
      label,
      span_structure(
        "column cols",
        span_structure(
          "cols 1,2",
          col1, col2
        ),
        col4
      ),
      span_structure(
        "my cols",
        new_col_3 = mycol3,
        mycol5
      ))
  )

  tfrmt_multiple_cols <- tfrmt(
    group = group,
    label = label,
    param = parm,
    values = val,
    column = c(top_span, child_span, my_col),
    col_plan = col_plan(
      group,
      label,
      col1,
      col2,
      col4,
      new_col_3 = mycol3,
      mycol5
    )
  )


  suppressMessages({
      processed_gt <- print_to_gt(tfrmt = tfrmt_multiple_cols, .data = dat)
      processed_gt_2 <- print_to_gt(tfrmt = tfrmt_with_parial_child_span, .data = dat %>% select(-top_span, -child_span))
  })

  expect_equal(
    processed_gt,
    processed_gt_2
  )

})

test_that("Build simple tfrmt with spans with child spans that are and are not spanned - removal",{

  dat <- tibble::tribble(
    ~group,      ~label,     ~top_span, ~child_span,  ~my_col,    ~parm, ~val,
    "g1", "rowlabel1", "column cols",  "cols 1,2",   "col1",  "value",    1,
    "g1", "rowlabel1", "column cols",  "cols 1,2",   "col2",  "value",    1,
    "g1", "rowlabel1",     "my cols",          NA, "mycol3",  "value",    1,
    "g1", "rowlabel1", "column cols",          NA,   "col4",  "value",    1,
    "g1", "rowlabel1",     "my cols",          NA, "mycol5",  "value",    1,
    "g1", "rowlabel2", "column cols",  "cols 1,2",   "col1",  "value",    2,
    "g1", "rowlabel2", "column cols",  "cols 1,2",   "col2",  "value",    2,
    "g1", "rowlabel2",     "my cols",          NA, "mycol3",  "value",    2,
    "g1", "rowlabel2", "column cols",          NA,   "col4",  "value",    2,
    "g1", "rowlabel2",     "my cols",          NA, "mycol5",  "value",    2,
    "g2", "rowlabel3", "column cols",  "cols 1,2",   "col1",  "value",    3,
    "g2", "rowlabel3", "column cols",  "cols 1,2",   "col2",  "value",    3,
    "g2", "rowlabel3",     "my cols",          NA, "mycol3",  "value",    3,
    "g2", "rowlabel3", "column cols",          NA,   "col4",  "value",    3,
    "g2", "rowlabel3",     "my cols",          NA, "mycol5",  "value",    3
  )

  tfrmt_with_parial_child_span <- tfrmt(
    group = group,
    label = label,
    param = parm,
    values = val,
    column = my_col,
    col_plan = col_plan(
      group,
      label,
      span_structure(
        "column cols",
        span_structure(
          "cols 1,2",
          col1, col2
        ),
        col4
      ),
      span_structure(
        "my cols",
        new_col_3 = mycol3
      ),
      -mycol5
      )
  )

  tfrmt_multiple_cols <- tfrmt(
    group = group,
    label = label,
    param = parm,
    values = val,
    column = c(top_span, child_span, my_col),
    col_plan = col_plan(
      group,
      label,
      col1,
      col2,
      col4,
      new_col_3 = mycol3,
      -mycol5
    )
  )


  suppressMessages({
      processed_gt <- print_to_gt(tfrmt = tfrmt_multiple_cols, .data = dat)
      processed_gt_2 <- print_to_gt(tfrmt = tfrmt_with_parial_child_span, .data = dat %>% select(-top_span, -child_span))
  })

  expect_equal(
    processed_gt,
    processed_gt_2
  )

})


test_that("span_structure returns correct errors",{

  expect_error(
    span_structure(
      label = 1234
    ),
    "`label` must be a character vector",
    fixed = TRUE
  )

})

test_that("col_plan returns correct errors",{

  expect_error(
    col_plan(
      func("invalid entry")
    ),
    paste0(
      "Invalid entry: `func(\"invalid entry\")`\n",
      "Only span_structures (`span_structure()`),",
      " selection helpers (See <https://tidyselect.r-lib.org/reference>),",
      "  or unquoted expressions representing variable names  can be entered",
      " as contents. Changing the names of individual variables using",
      " new_name = old_name syntax is allowable"
    ),
    fixed = TRUE
  )

})

test_that("tfrmt returns error when defining multiple columns and span_structures in col_plan",{


  expect_error(
    tfrmt(
      column = c(test1,test2),
      col_plan = col_plan(
        span_structure("my labels", test_column),
        column2
      )
    ),
    paste0(
      "Multiple columns defined in `column` argument of tfrmt ",
      "as well as span_structures in `col_plan`.\n",
      "The use of only one approach is permitted. ",
      "Select a single column or remove span_structures from `col_plan()`"
    ),
    fixed = TRUE
  )

})




# this happens in the select_col_plan, rather than the row_grp_plan
test_that("Suppress printing of groups", {

  mock_multi_grp <- tibble::tribble(
    ~grp1,    ~grp2,     ~ my_label,
    "grp1_1", "grp2_1", "my_label_1",
    "grp1_1", "grp2_1", "my_label_2",
    "grp1_1", "grp2_2", "my_label_1",
    "grp1_1", "grp2_2", "my_label_2",
    "grp1_2", "grp2_1", "my_label_1",
    "grp1_2", "grp2_1", "my_label_2",
    "grp1_2", "grp2_2", "my_label_1",
    "grp1_2", "grp2_2", "my_label_2",
  ) %>%
    mutate(
      trtA = rep("xx (xx%)", 8),
      trtB = rep("xx (xx%)", 8),
      trtC = rep("xx (xx%)", 8),
    )

  #option 1
  spec_noprint_row_grp <- tfrmt(
    group = c(grp1, grp2),
    label = my_label,
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "noprint"))
  )
  #option 2
  spec_noprint_col_plan1 <- tfrmt(
    group = c(grp1, grp2),
    label = my_label,
    col_plan = col_plan(my_label, starts_with("trt"))
  )
  #option 3
  spec_noprint_col_plan2 <- tfrmt(
    group = c(grp1, grp2),
    label = my_label,
    col_plan = col_plan(-starts_with("grp"))
  )

  df_no_grp <- tibble::tribble(
    ~my_label,   ~trtA,     ~trtB,     ~trtC  ,
    "my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
  )

  expect_equal(select_col_plan(mock_multi_grp, spec_noprint_row_grp), df_no_grp)
  expect_equal(select_col_plan(mock_multi_grp, spec_noprint_col_plan1), df_no_grp)
  expect_equal(select_col_plan(mock_multi_grp, spec_noprint_col_plan2), df_no_grp)

})


test_that("Tidyselect subtraction with span_structure",{

  df <- crossing(label = c("label 1", "label 2", "label 3"),
                 column = c("trt1", "trt2", "pl", "trt1&trt2"),
                 param = c("count", "percent")) %>%
    mutate(ord1 = rep(seq(1:length(unique(.$label))), each = nrow(.)/length(unique(.$label)) ))

  df_fake_values <- df %>% mutate(values = runif(nrow(df)))

  tfrmt_minus_selection <-  tfrmt(
      # Specify columns in the data
      label = label,
      column = column,
      param = param,
      values = values,
      sorting_cols = c(ord1),
      # Specify body plan
      body_plan = body_plan(
        frmt_structure(group_val = ".default", label_val = ".default",
                       frmt_combine(
                         "{count} {percent}",
                         count = frmt("XXX"),
                         percent = frmt_when("==100"~ frmt(""),
                                             "==0"~ "",
                                             "TRUE" ~ frmt("(XX.X%)"))
                       ))
      ),
      # Remove extra cols and create spans
      col_plan = col_plan(
        -starts_with("ord"),
        span_structure(
          "Treatment",
          T1 = trt1,
          T2 = trt2,
          `T1&T2`= `trt1&trt2`),
        span_structure(
          "Placebo",
          PL = pl)
      )
    )


  mock_gt <- print_mock_gt(tfrmt_minus_selection, df)

  ## keeps the spanners & original cols other than ones that start with "ord". renaming occurs as needed
  expect_equal(
    names(mock_gt$`_data`),
    c("label", "Placebo___tlang_delim___PL", "Treatment___tlang_delim___T1",
      "Treatment___tlang_delim___T1&T2", "Treatment___tlang_delim___T2",
      "..tfrmt_row_grp_lbl"
    )
  )

  real_gt <- print_to_gt(tfrmt_minus_selection, df_fake_values)

  ## keeps the spanners & original cols other than ones that start with "ord". renaming occurs as needed
  expect_equal(
    names(real_gt$`_data`),
    c("label", "Placebo___tlang_delim___PL", "Treatment___tlang_delim___T1",
      "Treatment___tlang_delim___T1&T2", "Treatment___tlang_delim___T2",
      "..tfrmt_row_grp_lbl"
    )
  )


  tfrmt_minus_selection_2 <- tfrmt_minus_selection %>%
    tfrmt(
      col_plan = col_plan(
        span_structure(
          "Treatment",
          T1 = trt1,
          T2 = trt2,
          `T1&T2`= `trt1&trt2`),
        span_structure(
          "Placebo",
          PL = pl),
        -starts_with("ord")
      )
    )

  mock_gt2 <- tfrmt_minus_selection_2 %>%
    print_mock_gt(df)


  ## keeps only the spanners, label is dropped
  expect_equal(
    names(mock_gt2$`_data`),
    c("Treatment___tlang_delim___T1",
      "Treatment___tlang_delim___T2",
      "Treatment___tlang_delim___T1&T2",
      "Placebo___tlang_delim___PL",
      "..tfrmt_row_grp_lbl"
    )
  )

  real_gt2 <- print_to_gt(tfrmt_minus_selection_2, df_fake_values)

  ## keeps the spanners & original cols other than ones that start with "ord". renaming occurs as needed
  expect_equal(
    names(real_gt$`_data`),
    c("label", "Placebo___tlang_delim___PL", "Treatment___tlang_delim___T1",
      "Treatment___tlang_delim___T1&T2", "Treatment___tlang_delim___T2",
      "..tfrmt_row_grp_lbl"
    )
  )

})
