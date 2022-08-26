test_that("expr_to_filter - quosure", {

  var <- "value1"
  default <- ".default"
  quo_val <- quo(col)

  filter_var <- expr_to_filter.quosure(cols = quo_val, val = var)
  filter_default <- expr_to_filter.quosure(cols = quo_val, val = default)

  expect_equal(filter_var, "`col` %in% c('value1')")
  expect_equal(filter_default, "TRUE")

})


test_that("expr_to_filter - quosures", {

  var <- "value1"
  default <- ".default"
  var_list_named <- list(col1 = "value1", col2 = "value2")
  var_list_default <- list(col1 = ".default", col2 = "value2")
  var_list_misnamed <- list(col1 = ".default", col12 = "value2")
  var_list_missing <- list(col2 = "value2")
  quos_val_1 <- vars(col1)
  quos_val_2 <- vars(col1,col2)

  ## length 1 vars = length one var
  filter_var <- expr_to_filter.quosures(cols = quos_val_1, val = var)
  filter_default <- expr_to_filter.quosures(cols = quos_val_1, val = default)

  expect_equal(filter_var, "`col1` %in% c('value1')")
  expect_equal(filter_default, "TRUE")

  ## length 2 vars & length one var

  ### Must be a list
  expect_error(
    expr_to_filter.quosures(cols = quos_val_2, val = var),
    "If multiple cols are provided, val must be a named list",
    fixed = TRUE
  )

  ### .default is allowed
  expect_equal(
    expr_to_filter.quosures(cols = quos_val_2, val = default),
    "TRUE"
    )

  ## length 2 vars & length two var
  filter_var_list_named <- expr_to_filter.quosures(cols = quos_val_2, val = var_list_named)
  filter_var_list_default <- expr_to_filter.quosures(cols = quos_val_2, val = var_list_default)

  expect_equal(filter_var_list_named, "`col1` %in% c('value1') & `col2` %in% c('value2')")
  expect_equal(filter_var_list_default, "TRUE & `col2` %in% c('value2')")

  ## length 2 vars & length two var incorrect names
  expect_error(
    expr_to_filter.quosures(cols = quos_val_2, val = var_list_misnamed),
    "Names of val entries do not all match col values",
    fixed = TRUE
  )



})


