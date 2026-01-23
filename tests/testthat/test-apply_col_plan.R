
test_that("create_stub_head() works", {

  # no labels
  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(-ord, grp2, lbl, `1`),
      group = rlang::quos(grp2),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "indented"
    ),
    ""
  )

  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(-ord, grp2, lbl, `1`),
      group = rlang::quos(grp2),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "column"
    ),
    c("", "")
  )

  # multiple labels

  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(-ord, Group2 = grp2, Group1 = grp1, Label = lbl, `1`),
      group = rlang::quos(grp2, grp1),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "indented"
    ),
    "Group2"
  )

  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(-ord, Group2 = grp2, Group1 = grp1, Label = lbl, `1`),
      group = rlang::quos(grp2, grp1),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "column"
    ),
    c("Group2", "Group1", "Label")
  )

  # mix of labeled/unlabeled

  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(-ord, grp2, Group1 = grp1, Label = lbl, `1`),
      group = rlang::quos(grp2, grp1),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "indented"
    ),
    "Group1"
  )
  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(-ord, grp2, Group1 = grp1, Label = lbl, `1`),
      group = rlang::quos(grp2, grp1),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "column"
    ),
    c("", "Group1", "Label")
  )

  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(-ord, grp2, Group1 = grp1, Label = lbl, `1`),
      group = rlang::quos(grp2, grp1),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "column"
    ),
    c("", "Group1", "Label")
  )


  # col plan out of order (should retain group/label original order)

  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(Label = lbl, grp1, Group2 = grp2, everything()),
      group = rlang::quos(grp2, grp1),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "indented"
    ),
    "Group2"
  )
  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(Label = lbl, grp1, Group2 = grp2, everything()),
      group = rlang::quos(grp2, grp1),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "column"
    ),
    c("Group2", "", "Label")
  )


  # col plan with subtraction

  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(-grp1, Label = lbl, Group2 = grp2, everything()),
      group = rlang::quos(grp2, grp1),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "indented"
    ),
    "Group2"
  )
  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(-grp1, Label = lbl, Group2 = grp2, everything()),
      group = rlang::quos(grp2, grp1),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "column"
    ),
    c("Group2", "Label")
  )
})
