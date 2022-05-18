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

  sample_df <- data.frame(A = character(), B = character(), C = character(), D = character())

  s1_cols <- span_col_select(s1, data = sample_df)
  s2_cols <- span_col_select(s2, data = sample_df)
  s3_cols <- span_col_select(s3, data = sample_df)

  expect_equal(s1_cols,c("A","B"))
  expect_equal(s2_cols,c("A","B","C","D"))
  expect_equal(s3_cols,c("D","C","A","B"))

})

test_that("Apply the col_plan to a simple gt", {

  col_plan_obj <- col_plan(
    span_structure(
      label = "Top Label Level 1",
      span_structure(
        label = "Second Label Level 1.1",
        vars(mpg, hp)
      ),
      span_structure(
        label = "Second Label Level 1.2",
        vars(starts_with("d"))
      ),
      vars(cyl)
    ),
    span_structure(
      label = "Top Label Level 2",
      vars(wt,qsec)
    )
  )

  basic_sorted_gt <- mtcars  %>%
    select_col_plan(col_plan_obj) %>%
    gt::gt()

  spanned_cols_gt <- basic_sorted_gt %>%
    apply_gt_spanning_labels(
      col_plan = col_plan_obj
    )

  ## make sure spanning columns applied properly
  expect_equal(
    spanned_cols_gt$`_spanners`$spanner_id,
    c("Top Label Level 2", "Second Label Level 1.1", "Second Label Level 1.2","Top Label Level 1")
  )

  expect_equal(
    spanned_cols_gt$`_spanners`$spanner_label,
    list(c("Top Label Level 2"),
         c("Second Label Level 1.1"),
         c("Second Label Level 1.2"),
         c("Top Label Level 1"))
  )

  expect_equal(
    spanned_cols_gt$`_spanners`$vars,
    list(c("wt", "qsec"),
         c("mpg", "hp"),
         c("disp", "drat"),
         c("mpg", "hp", "disp", "drat", "cyl"))
  )

  expect_equal(
    spanned_cols_gt$`_boxhead`$var,
    c("mpg", "hp", "disp", "drat", "cyl","wt", "qsec")
  )

})

test_that("Build simple tfrmt with multiple columns and apply to basic data",{

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
    processed_data <- apply_tfrmt(basic_example_dataset,tfrmt = basic_multi_column_template)
    processed_gt <- print_to_gt(tfrmt_spec = basic_multi_column_template, .data = basic_example_dataset)
  })




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
      " as contents. Changes the names of individual variables using",
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
