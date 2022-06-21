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


test_that("throw error when input argument errors out", {

  temp_function_1 <- function(x, ...){
    val <- temp_function_2(...)
    val
  }

  temp_function_2 <- function(..., env = parent.frame()){

    arg_parent <- names(formals(sys.function(sys.parent(1))))

    args <- setdiff(arg_parent,c("..."))

    val <- quo_get(
      args,
      envir = env
    )

    val
  }

  fail_function <- function(...){

    dot_list <- as.list(substitute(substitute(...)))[-1]

    if(length(dot_list) == 0){
      stop("I HAVE FAILED")
    }else{
      dot_list
    }
  }

  expect_silent(
    temp_function_1(x = fail_function("test value"))
  )

  expect_error(
    temp_function_1(x = fail_function()),
    "Error in evaluating argument `x`:\n Error in fail_function(): I HAVE FAILED",
    fixed = TRUE
  )

})

