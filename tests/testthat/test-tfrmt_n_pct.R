test_that("tfrmt_n_pct", {

  # format to avoid issues with the environment
  expect_equal(format(tfrmt_n_pct()$body_plan),
               body_plan(
                 frmt_structure(
                   group_val = ".default", label_val = ".default",
                   frmt_combine("{n} {pct}", n = frmt("xxx"),
                                pct = frmt_when("==100"~ frmt(""),
                                                ">99"~ frmt("(>99%)"),
                                                "==0"~ "",
                                                "<1" ~ frmt("(<1%)"),
                                                "TRUE" ~ frmt("(xx.x%)")))
                 )
               ) %>% format()
  )
  # See that it can change when n is changed
  expect_equal(format(tfrmt_n_pct("n_distinct")$body_plan),
               body_plan(
                 frmt_structure(
                   group_val = ".default", label_val = ".default",
                   frmt_combine("{n_distinct} {pct}", n_distinct = frmt("xxx"),
                                pct = frmt_when("==100"~ frmt(""),
                                                ">99"~ frmt("(>99%)"),
                                                "==0"~ "",
                                                "<1" ~ frmt("(<1%)"),
                                                "TRUE" ~ frmt("(xx.x%)")))
                 )
               ) %>% format()
  )
  # Change the frmt_when
  expect_equal(format(tfrmt_n_pct(pct_frmt_when =
                                   frmt_when(
                                     "==100" ~ "",
                                     "==0" ~ "",
                                     TRUE ~ frmt("(xx.x %)")
                                   ))$body_plan),
               body_plan(
                 frmt_structure(
                   group_val = ".default", label_val = ".default",
                   frmt_combine("{n} {pct}", n = frmt("xxx"),
                                pct = frmt_when(
                                  "==100" ~ "",
                                  "==0" ~ "",
                                  TRUE ~ frmt("(xx.x %)")
                                ))
                 )
               ) %>% format()
  )

  # layer with existing tfrmt
  test <- tfrmt(
    column = column
  )
  expect_equal(tfrmt_n_pct(tfrmt_obj = test)$column[[1]] %>%
    as_label(),
    "column")

  expect_error(tfrmt_n_pct(n = ""))
  expect_error(tfrmt_n_pct(pct = NULL))

})

