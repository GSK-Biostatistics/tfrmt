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

  expect_equal(s1$span_cols, list(vars(A,B)))
  expect_equal(s2$span_cols, list(span_structure(label = "Test Sub Label",vars(A,B)),vars(C,D)))

})

test_that("Defining the spanning plan", {

  s1 <- span_plan(
    span_structure(
    label = "Test Label",
    vars(A,B)
  ))

  expect_s3_class(s1,"span_plan_grp")

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

test_that("Apply the spanning structure to a simple gt", {


  spanning_lab_struct <- span_plan(
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

  basic_gt <- mtcars %>% gt::gt()

  spanned_cols_gt <- apply_gt_spanning_labels(
      basic_gt,
      spanning_lab_struct = spanning_lab_struct
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
    c("mpg", "hp", "disp", "drat", "cyl","wt", "qsec", "vs", "am","gear", "carb")
  )

})

test_that("span_structure returns correct errors",{

  expect_error(
    span_structure(
      label = 1234
    ),
    "`label` must be a character vector or element_label",
    fixed = TRUE
  )

  expect_error(
    span_structure(
      label = "1234",
      "invalid_Entry"
    ),
    paste0(
      "Only objects of type quosures (`var()`), or span_structure (`span_structure()`)",
      " can be entered as contents to span a label across"
    ),
    fixed = TRUE
  )

  expect_error(
    span_structure(
      label = "1234",
      quo(invalid_Entry)
    ),
    paste0(
      "Only objects of type quosures (`var()`), or span_structure (`span_structure()`)",
      " can be entered as contents to span a label across"
    ),
    fixed = TRUE
  )

})

test_that("span_plan returns correct errors",{

  expect_error(
    span_plan(
      "invalid entry"
    ),
    "All entries in span_plan must be a span_structure",
    fixed = TRUE
  )

})
