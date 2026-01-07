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
    "  multiple   spaces  ",
    # 1 3 1 4 2 1
    "combination of   single and    multiple  spaces ",
    NA,
    "     leading    whitespace",
    'trailing  whitespace     '
  )

  unicode_strings <- c(
    "nospaces",
    "\u00A0single spaces\u00A0",
    "\u00A0\u00A0multiple \u00A0 spaces\u00A0\u00A0",
    "combination of \u00A0 single and \u00A0 \u00A0multiple \u00A0spaces\u00A0",
    NA,
    "\u00A0\u00A0\u00A0\u00A0\u00A0leading \u00A0 \u00A0whitespace",
    'trailing \u00A0whitespace\u00A0\u00A0\u00A0\u00A0\u00A0'
  )

  expect_equal(whitespace_function(test_strings), unicode_strings)

})
