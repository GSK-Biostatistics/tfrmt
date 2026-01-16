test_that("Basic functionality: Resets a component and other components remain unchanged", {
  t_frmt <- tfrmt(
    title = "Table Title",
    group = c("row_label1"),
    label = c("row_label2"),
    param = c("param"),
    value = c("value"),
    column = c("column")
  )


  modified_tfrmt <- reset_component(t_frmt, "title")

  expect_null(modified_tfrmt$title)
  expect_equal(modified_tfrmt$group, t_tfrmt$group, ignore_attr = TRUE )
  expect_equal(modified_tfrmt$label, quo(row_label2), ignore_attr = TRUE)
  expect_equal(modified_tfrmt$param, quo(param), ignore_attr = TRUE)
  expect_equal(modified_tfrmt$value, quo(value), ignore_attr = TRUE)
  expect_equal(modified_tfrmt$column, vars(column), ignore_attr = TRUE)
})

test_that("Error for non-'tfrmt' object", {
  non_tfrmt_obj <- list(title = "My Title", subtitle = "My Subtitle")

  expect_error(
    reset_component(non_tfrmt_obj, "subtitle"),
    "The input object must be of class 'tfrmt'."
  )
})

test_that("Error for invalid component_name type", {
  t_frmt <- tfrmt(
    title = "Table Title",
    group = c("row_label1"),
    label = c("row_label2"),
    param = c("param"),
    value = c("value"),
    column = c("column")
  )


  expect_error(
    reset_component(t_frmt, c("title", "data")),
    "The component name must be a single string."
  )

  expect_error(
    reset_component(t_frmt, 123),
    "The component name must be a single string."
  )
})

test_that("Error for nonexistent component", {
  t_frmt <- tfrmt(
    title = "Table Title",
    group = c("row_label1"),
    label = c("row_label2"),
    param = c("param"),
    value = c("value"),
    column = c("column")
  )


  expect_error(
    reset_component(t_frmt, "nonexistent"),
    "component 'nonexistent' does not exist in the tfrmt object."
  )
})
