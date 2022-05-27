
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

  s1_cols <- span_col_select(s1, data = sample_df)
  s2_cols <- span_col_select(s2, data = sample_df)
  s3_cols <- span_col_select(s3, data = sample_df)
  s4_cols <- span_col_select(s4, data = sample_df)

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
    body_style = table_body_plan(
      frmt_structure(pam = frmt("x.xx"))
    ),
    col_align = col_align_plan(element_align(align = ".", col = vars(delm)),
                               element_align(align = "right"))
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

  basic_example_dataset <- tribble(
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

  suppressWarnings({
    processed_gt <- print_to_gt(tfrmt = basic_multi_column_template, .data = basic_example_dataset)
    processed_gt_2 <- print_to_gt(tfrmt = spanned_column_template, .data = basic_example_dataset %>% select(-test1))
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
