test_that("symbols are converted to quosures", {
    res <- parse_col_plan_entry(quote(col1))
    expect_true(rlang::is_quosure(res))
    expect_equal(rlang::quo_get_expr(res), quote(col1))
})

test_that("the empty symbol returns NULL", {
  expect_null(parse_col_plan_entry(rlang::missing_arg()))
})

test_that("tidyselect calls are converted to quosures without being evaluated", {
  res <- parse_col_plan_entry(quote(starts_with("a")))
  expect_true(rlang::is_quosure(res))
  expect_equal(rlang::quo_get_expr(res), quote(starts_with("a")))

  res_neg <- parse_col_plan_entry(quote(-col1))
  expect_true(rlang::is_quosure(res_neg))
  expect_equal(rlang::quo_get_expr(res_neg), quote(-col1))
})

test_that("vars() calls are evaluated", {
  res_vars <- parse_col_plan_entry(quote(vars(col1, col2)))
  expect_true(rlang::is_quosures(res_vars))
  expect_length(res_vars, 2)
})

test_that("span_structure() calls are evaluated", {
  res <- parse_col_plan_entry(quote(span_structure(a = col1)))
  expect_s3_class(res, "span_structure")
})

test_that("character entries become length-one quosures", {
  res <- parse_col_plan_entry("col1")
  expect_true(rlang::is_quosure(res))
  expect_equal(rlang::quo_get_expr(res), quote(col1))
})

test_that("invalid calls error", {
  expect_snapshot(parse_col_plan_entry(quote(foo(x))), error = TRUE)
  expect_snapshot(col_plan(foo(x)), error = TRUE)
})

test_that("invalid calls inside span_structure() omit the span structure bullet", {
  expect_snapshot(span_structure(a = foo(x)), error = TRUE)
})

test_that("unsupported entry types error", {
  expect_error(parse_col_plan_entry(1i), "Unexpected entry type")
})
