
test_that("build frmt objects",{

  #frmt
  frmt_spec <- frmt_builder(param = c("mean","sd"), frmt_string = c("xx.x","xx.xx"))
  frmt_string <- "mean = frmt('xx.x'), sd = frmt('xx.xx')"

  expect_equal(frmt_spec, frmt_string)

  # frmt_combine
  frmt_spec <- frmt_combine_builder(param_combine = "{mean} ({sd})",
                                    param = c("mean","sd"), frmt_string = c("xx.x","xx.xx"))
  frmt_string <- "frmt_combine('{mean} ({sd})', mean = frmt('xx.x'), sd = frmt('xx.xx'))"

  expect_equal(frmt_spec, frmt_string)


  # frmt_structure
  frmt_list <- list(
    "frmt_combine('{mean} ({sd})', mean = frmt('xx.x'), sd = frmt('xx.xx'))",
    "median = frmt('xx.x')",
    "n = frmt('xxx')"
  )
  frmt_spec <- frmt_structure_builder(group_val = ".default", label_val = "ige", frmt_list)
  frmt_string <- list(
    "frmt_structure(group_val = .default, label_val = ige, frmt_combine('{mean} ({sd})', mean = frmt('xx.x'), sd = frmt('xx.xx')))",
    "frmt_structure(group_val = .default, label_val = ige, median = frmt('xx.x'))",
    "frmt_structure(group_val = .default, label_val = ige, n = frmt('xxx'))")

  expect_equal(frmt_spec, frmt_string)
})


