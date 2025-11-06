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
  expected_function <- function(x) {
    str_replace_all(x, pattern = "\\s", replacement = "\u00A0")
  }
  # match environments
  environment(expected_function) <- environment(gt_with_unicode$`_transforms`[[1]]$fn)
  expect_equal(gt_with_unicode$`_transforms`[[1]]$fn,expected_function)

})
