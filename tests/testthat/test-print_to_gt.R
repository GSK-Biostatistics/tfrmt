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
