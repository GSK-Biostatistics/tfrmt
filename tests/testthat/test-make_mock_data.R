
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

# test_that("Mock data contains all levels of group/label", {
#
#   plan  <- tfrmt(
#     group = "my_group",
#     label = "my_label",
#     param = "param2",
#     values = "val2",
#     column = "col",
#     body_style = table_body_plan(
#       frmt_structure(group_val = list(grp1 = ".default", grp2 = c("c","d")), label_val = c("e","f"), frmt("xx.x"))
#     )
#   )
#
#
# })
