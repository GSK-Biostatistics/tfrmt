test_that("convert_ws_unicode works as expected", {
  gt_with_ws <- data.frame(
    group = "    trailing.   and.   leading.  ws.   ",
    value = " ( x  x)"
  ) |>
    gt::gt()

  gt_with_unicode <- convert_ws_unicode(gt_with_ws)

  # test that metadata is present in gt ready to apply transform

  # columns to apply to
  expect_equal(
    gt_with_unicode$`_transforms`[[1]]$resolved$colnames,
    c("group", "value")
  )

  # rows to apply to
  expect_equal(
    gt_with_unicode$`_transforms`[[1]]$resolved$rows,
    1
  )

  # function to apply
  whitespace_function <- gt_with_unicode$`_transforms`[[1]]$fn

  test_strings <- c(
    "nospaces",
    " single spaces ",
    "  multiple   spaces  "
  )

  unicode_strings <- c(
    "nospaces",
    "\u00A0single\u00A0spaces\u00A0",
    "\u00A0\u00A0multiple\u00A0\u00A0\u00A0spaces\u00A0\u00A0"
  )

  expect_equal(
    whitespace_function(test_strings),
    unicode_strings
  )
})

test_that("print_to_gt() works", {
  dat <- tibble::tribble(
    ~group  , ~label  , ~span1   , ~span2  , ~lower     , ~param , ~val ,
    "mygrp" , "mylbl" , "span01" , "span1" , "lower1_a" , "prm"  ,    1 ,
    "mygrp" , "mylbl" , "span01" , "span1" , "lower1_b" , "prm"  ,    1 ,
    "mygrp" , "mylbl" , "span01" , "span2" , "lower2_a" , "prm"  ,    1 ,
    "mygrp" , "mylbl" , "span01" , "span2" , "lower2_b" , "prm"  ,    1 ,
    "mygrp" , "mylbl" , "span02" , "span3" , "lower2_a" , "prm"  ,    1 ,
    "mygrp" , "mylbl" , "span02" , "span3" , "lower2_b" , "prm"  ,    1
  )

  tfrmt_spec <- tfrmt(
    group = "group",
    label = "label",
    param = "param",
    column = c("span1", "span2", "lower"),
    value = "val",
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt("x.xx")
      )
    )
  )

  expect_no_error(
    print_to_gt(
      tfrmt_spec,
      .data = dat
    )
  )

  expect_snapshot(
    print_to_gt(
      tfrmt_spec,
      .data = dat
    )[["_data"]]
  )
})

test_that("print_to_gt() complains with incorrect inputs", {
  # complains when the first argument is not `tfrmt`
  expect_snapshot(
    error = TRUE,
    print_to_gt(mtcars)
  )

  # complains when the `.data` argument is not a data.frame
  tfrmt_spec <- tfrmt(
    label = label,
    column = column,
    param = param,
    value = value,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{count} {percent}",
          count = frmt("xxx"),
          percent = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("(xx.x%)")
          )
        )
      )
    )
  )

  expect_snapshot(
    error = TRUE,
    print_to_gt(tfrmt_spec, "foo")
  )
})

test_that("print_mock_gt() messages when tfrmt$param is missing", {
  # no message when tfrmt$param is not empty
  tfrmt_spec <- tfrmt(
    label = label,
    column = column,
    param = param,
    value = value,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{count} {percent}",
          count = frmt("xxx"),
          percent = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("(xx.x%)")
          )
        )
      )
    )
  )

  expect_no_message(
    print_mock_gt(
      tfrmt_spec
    )
  )

  # message when tfrmt$param is empty
  tfrmt_spec_no_param <- tfrmt(
    label = label,
    column = column,
    value = value,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{count} {percent}",
          count = frmt("xxx"),
          percent = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("(xx.x%)")
          )
        )
      )
    )
  )

  expect_message(
    print_mock_gt(
      tfrmt_spec_no_param
    ),
    "`tfrmt` will need a `param` value to `print_to_gt` when data is available",
    fixed = TRUE
  )
  expect_snapshot({
    print_mock_gt(
      tfrmt_spec_no_param
    )[["_data"]]
  })
})

test_that("print_mock_gt() messages when tfrmt$column is missing", {
  # no message when tfrmt$column is not missing
  tfrmt_spec <- tfrmt(
    label = label,
    column = column,
    param = param,
    value = value,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{count} {percent}",
          count = frmt("xxx"),
          percent = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("(xx.x%)")
          )
        )
      )
    )
  )

  expect_no_message(
    print_mock_gt(
      tfrmt_spec
    )
  )

  # message when tfrmt$column is missing
  tfrmt_spec_no_column <- tfrmt(
    label = label,
    param = param,
    value = value,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{count} {percent}",
          count = frmt("xxx"),
          percent = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("(xx.x%)")
          )
        )
      )
    )
  )

  expect_message(
    print_mock_gt(
      tfrmt_spec_no_column
    ),
    "`tfrmt` will need `column` value(s) to `print_to_gt` when data is available",
    fixed = TRUE
  )

  expect_snapshot({
    print_mock_gt(
      tfrmt_spec_no_column
    )[["_data"]]
  })
})

test_that("print_mock_gt() messages when tfrmt$value is missing", {
  # no message when tfrmt$value is not missing
  tfrmt_spec <- tfrmt(
    label = label,
    column = column,
    param = param,
    value = value,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{count} {percent}",
          count = frmt("xxx"),
          percent = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("(xx.x%)")
          )
        )
      )
    )
  )

  expect_no_message(
    print_mock_gt(
      tfrmt_spec
    )
  )

  # message when tfrmt$value is missing
  tfrmt_spec_no_value <- tfrmt(
    label = label,
    column = column,
    param = param,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{count} {percent}",
          count = frmt("xxx"),
          percent = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("(xx.x%)")
          )
        )
      )
    )
  )

  expect_message(
    print_mock_gt(
      tfrmt_spec_no_value
    ),
    "`tfrmt` will need `value` value to `print_to_gt` when data is available",
    fixed = TRUE
  )

  expect_snapshot({
    print_mock_gt(
      tfrmt_spec_no_value
    )[["_data"]]
  })
})

test_that("print_mock_gt() with missing body_plan", {
  # when tfrmt$body_plan is missing, one is made up on the fly
  tfrmt_spec_no_body_plan <- tfrmt(
    label = label,
    column = column,
    param = param,
    value = value
  )

  expect_no_message(
    print_mock_gt(
      tfrmt_spec_no_body_plan
    )
  )

  tfrmt_spec_default_body_plan <- tfrmt(
    label = label,
    column = column,
    param = param,
    value = value,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt("X.X")
      )
    )
  )

  expect_snapshot(
    print_mock_gt(
      tfrmt_spec_no_body_plan
    )[["_data"]]
  )

  expect_identical(
    print_mock_gt(
      tfrmt_spec_no_body_plan
    )[["_data"]],
    print_mock_gt(
      tfrmt_spec_default_body_plan
    )[["_data"]],
    # .col_plan_vars is a list of quosures and their environments would not match
    ignore_attr = ".col_plan_vars"
  )
})

test_that("print_mock_data() removes `value` when it exists in the input data", {
  data <- tidyr::crossing(
    label = c(
      "Intent-To-Treat (ITT)",
      "Safety",
      "Efficacy",
      "Complete Week 24",
      "Complete Study"
    ),
    column = c(
      "Placebo\n(N=XX)",
      "Xanomeline\nLow Dose\n(N=XX)",
      "Xanomeline\nHigh Dose\n(N=XX)",
      "Total\n(N=XXX)"
    ),
    param = c(
      "n",
      "percent"
    )
  ) |>
    dplyr::mutate(
      value_to_remove = 1
    )

  plan <- tfrmt(
    label = "label",
    column = "column",
    param = "param",
    value = "value_to_remove",
    title = "Summary of Populations",
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{n} ({percent}%)",
          n = frmt("xx"),
          percent = frmt("xxx")
        )
      )
    )
  )

  expect_message(
    print_mock_gt(
      plan,
      data
    ),
    "Removing `value_to_remove` from input data for mocking.",
    fixed = TRUE
  )

  expect_snapshot(
    print_mock_gt(
      plan,
      data
    )
  )
})
