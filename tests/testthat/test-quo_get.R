test_that("turn length one unquoted vals into args", {

  temp_function <- function(x){
    quo_get(
      args = "x",
      as_var_args = "x"
    )
  }

  expect_equal(
    temp_function(bare_arg_val)$x,
    vars(bare_arg_val),
    ignore_attr = TRUE
  )

})

test_that("turn length 2 unquoted vals into args", {

  temp_function <- function(x){
    quo_get(
      args = "x",
      as_var_args = "x"
    )
  }

  expect_equal(
    temp_function(c(bare_arg_val1, bare_arg_val2))$x,
    vars(bare_arg_val1, bare_arg_val2),
    ignore_attr = TRUE
  )

})

test_that("turn length one unquoted quo into args", {

  temp_function <- function(x){
    quo_get(
      args = "x",
      as_quo_args = "x"
    )
  }

  expect_equal(
    temp_function(bare_arg_val)$x,
    quo(bare_arg_val),
    ignore_attr = TRUE
  )

})

test_that("turn length two unquoted quo into args", {

  temp_function <- function(x){
    quo_get(
      args = "x",
      as_quo_args = "x"
    )
  }

  warning_res <- capture_warnings({
    tempfunc_res <-
      temp_function(c(bare_arg_val1, bare_arg_val2))
  })

  expect_equal(
    tempfunc_res$x,
    quo(bare_arg_val1),
    ignore_attr = TRUE
  )

})

