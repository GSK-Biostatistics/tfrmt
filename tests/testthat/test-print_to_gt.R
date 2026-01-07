test_that("convert_ws_unicode works as expected",{
  gt_with_ws <- data.frame(group = "    trailing.   and.   leading.  ws.   ",
                           value = " ( x  x)") |>
    gt::gt()

  gt_with_unicode <- convert_ws_unicode(gt_with_ws)

  # test that metadata is present in gt ready to apply transform

  # columns to apply to
  expect_equal(gt_with_unicode$`_transforms`[[1]]$resolved$colnames, c("group", "value"))

  # rows to apply to
  expect_equal(gt_with_unicode$`_transforms`[[1]]$resolved$rows,1)

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

  expect_equal(whitespace_function(test_strings), unicode_strings)

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
    value=value,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{count} {percent}",
          count = frmt("xxx"),
          percent = frmt_when(
            "==100"~ frmt(""),
            "==0"~ "",
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
