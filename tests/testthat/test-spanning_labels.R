test_that("Defining the spanning format", {

  s1 <- span_frmt(
    label = "Test Label",
    vars(A,B)
  )

  s2 <- span_frmt(
    label = "Test Label",
    span_frmt(
      label = "Test Sub Label",
      vars(A,B)
    ),
    vars(C,D)
  )

  expect_s3_class(s1,"span_frmt")
  expect_s3_class(s2,"span_frmt")
  expect_s3_class(s2,"span_frmts")

  expect_equal(s1$label, "Test Label")
  expect_equal(s2$label, "Test Label")
  expect_equal(s2$label, "Test Label")

  expect_equal(s1$span_cols, list(vars(A,B)))
  expect_equal(s2$span_cols, list(span_frmt(label = "Test Sub Label",vars(A,B)),vars(C,D)))

})

test_that("Defining the spanning structure", {

  s1 <- span_structure(
    span_frmt(
    label = "Test Label",
    vars(A,B)
  ))

  expect_s3_class(s1,"span_struct_grp")

})

test_that("Apply the spanning structure to a simple gt", {

  spanning_lab_struct <- span_structure(
    span_frmt(
      label = "Top Label Level 1",
      span_frmt(
        label = "Second Label Level 1.1",
        vars(mpg, hp)
      ),
      span_frmt(
        label = "Second Label Level 1.2",
        vars(starts_with("d"))
      ),
      vars(cyl)
    ),
    span_frmt(
      label = "Top Label Level 2",
      vars(wt,qsec)
    )
  )

  basic_gt <- mtcars %>% gt::gt()

  spanned_cols_gt <- apply_gt_spanning_labels(
      basic_gt,
      spanning_lab_struct = spanning_lab_struct
    )

  expect_equal(nrow(spanned_cols_gt$`_spanners`),4)
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

})


