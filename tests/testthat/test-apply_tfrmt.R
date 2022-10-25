


test_that("pivot_wider_tfrmt gives message when frmt_combine may be missing",{

  dat1 <- tibble::tribble(
    ~grp2, ~lbl, ~prm, ~column, ~val, ~ord,
    "c",   "n", "n",   1,   1,  1,
    "c",   "n", "n_2", 1,   1.1,  1,
    "b",   "m", "n",   1,   2,  2,
    "b",   "m", "n_2", 1,   2.1,  2,
    "v",   "s", "n",   1,   3,  3,
    "v",   "s", "n_3",   1,   3.3,  3,
    "p",   "e", "n",   1,   4,  4
  )

  dat2 <- tibble::tribble(
    ~grp1, ~grp2, ~lbl, ~prm, ~column, ~val, ~ord,
    "d",   "c",   "n", "n",   1,   1,  1,
    "d",   "c",   "n", "n_2", 1,   1.1,  1,
    "a",   "b",   "m", "n",   1,   2,  2,
    "a",   "b",   "m", "n_2",   1,   2.1,  2,
    "q",   "v",   "s", "n",   1,   3,  3,
    "b",   "p",   "e", "n",   1,   4,  4
  )


  dat3 <- tibble::tribble(
    ~grp1, ~grp2, ~lbl, ~prm, ~column, ~val, ~ord,
    "d",   "c",   "n", "n",   1,   1,  1,
    "d",   "c",   "n", "n_2", 1,   1.1,  1,
    "a",   "b",   "m", "n",   1,   2,  2,
    "a",   "b",   "m", "n_2", 1,   2.1,  2,
    "q",   "v",   "s", "n",   1,   3,  3,
    "q",   "v",   "s", "n_3",   1,   3.3,  3,
    "b",   "p",   "e", "n",   1,   4,  4
  )




  tfrmt_temp <- tfrmt(
    group = grp2,
    label = lbl,
    column = column,
    value = val,
    param = prm,
    sorting_cols = ord,
    col_plan = col_plan(-ord),
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt("x.x")
        ),
      frmt_structure(
        group_val = ".default",
        label_val = "m",
        frmt_combine("{n}/{n_2}",
                     n = frmt("x"),
                     n_2 = frmt("x.x")
                     )
        )
      )
    )

  expect_message(
    processed_dat <- apply_tfrmt(dat1, tfrmt_temp, mock = FALSE),
    paste0(
      c(
        "Multiple param listed for the same group/label values.",
        "The following frmt_structures may be missing from the body_plan",
        "or the order may need to be changed:",
        "- `frmt_structure(group_val = \"c\", label_val = \"n\", frmt_combine(\"{n}, {n_2}\",n = frmt(\"xx\"), n_2 = frmt(\"xx\")))`",
        "- `frmt_structure(group_val = \"v\", label_val = \"s\", frmt_combine(\"{n}, {n_3}\",n = frmt(\"xx\"), n_3 = frmt(\"xx\")))`"
      ),
      collapse = "\n"
    ),
    fixed = TRUE
  )

  expect_message(
    processed_dat <- apply_tfrmt(dat2, tfrmt_temp %>% tfrmt(group = c(grp1,grp2)), mock = FALSE),
    paste0(
      c(
        "Multiple param listed for the same group/label values.",
        "The following frmt_structures may be missing from the body_plan",
        "or the order may need to be changed:",
        "- `frmt_structure(group_val = list(grp1 = \"d\", grp2 = \"c\"), label_val = \"n\", frmt_combine(\"{n}, {n_2}\",n = frmt(\"xx\"), n_2 = frmt(\"xx\")))`"
      ),
      collapse = "\n"
    ),
    fixed = TRUE
  )

  expect_message(
    processed_dat <- apply_tfrmt(dat3, tfrmt_temp %>% tfrmt(group = c(grp1,grp2)), mock = FALSE),
    paste0(
      c(
        "Multiple param listed for the same group/label values.",
        "The following frmt_structures may be missing from the body_plan",
        "or the order may need to be changed:",
        "- `frmt_structure(group_val = list(grp1 = \"d\", grp2 = \"c\"), label_val = \"n\", frmt_combine(\"{n}, {n_2}\",n = frmt(\"xx\"), n_2 = frmt(\"xx\")))`",
        "- `frmt_structure(group_val = list(grp1 = \"q\", grp2 = \"v\"), label_val = \"s\", frmt_combine(\"{n}, {n_3}\",n = frmt(\"xx\"), n_3 = frmt(\"xx\")))`"
      ),
      collapse = "\n"
    ),
    fixed = TRUE
  )


  tfrmt_temp2<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1,grp),
    label = rowlbl2,
    column = column,
    param = param,
    value = value,
    sorting_cols = c(ord1, ord2))

  # use quietly to grab messages from apply_tfrmt
  safe_apply_tfrmt<-purrr::quietly(apply_tfrmt)

  expect_equal(
    safe_apply_tfrmt(data_demog %>% filter(rowlbl1=="Age (y)"), tfrmt_temp2 , mock = FALSE)$messages,
    c(
    "The following rows of the given dataset have no format applied to them 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57\n"
    ,"Multiple param listed for the same group/label values.\nThe following frmt_structures may be missing from the body_plan\nor the order may need to be changed:\n- `frmt_structure(group_val = list(rowlbl1 = \"Age (y)\", grp = \"cat\"), label_val = c(\"65-80 yrs\",\"<65 yrs\",\">80 yrs\"), frmt_combine(\"{n}, {pct}\",n = frmt(\"xx\"), pct = frmt(\"xx\")))`"      )
    )


})

test_that("test tentative_process",{

  passing_func <- function(x,y = "value"){
    paste0(x,y)
  }

  failing_func <- function(x,y = "value"){
    stop("this function failed")
    paste0(x,y)
  }
  
  rlang_abort_func <- function(x,y = "value"){
    rlang::abort("this function failed2")
    paste0(x,y)
  }

  ## function passing in tentative process
  expect_equal(
    tentative_process("x",passing_func),
    "xvalue"
  )

  passing_func_messages <- capture_messages({
    tentative_process("x",passing_func)
  })
  
  expect_true(is_empty(passing_func_messages))
  expect_equal(
    passing_func_messages,
    character()
  )
  
  ## function failing in tentative process
  expect_equal(
    suppressMessages(tentative_process("x",failing_func)),
    "x"
  )
  
  failing_func_messages <- capture_messages({
    tentative_process("x",failing_func)
  })

  expect_true(!is_empty(failing_func_messages))
  expect_equal(
    failing_func_messages,
    "Unable to to apply failing_func.\nReason: this function failed\n"
  )
  
  ## function failing in tentative process
  expect_equal(
    suppressMessages(tentative_process("x",rlang_abort_func)),
    "x"
  )
  
  rlang_abort_func_messages <- capture_messages({
    tentative_process("x",rlang_abort_func)
  })
  
  expect_true(!is_empty(rlang_abort_func_messages))
  expect_equal(
    rlang_abort_func_messages,
    "Unable to to apply rlang_abort_func.\nReason: this function failed2\n"
  )
  
})
