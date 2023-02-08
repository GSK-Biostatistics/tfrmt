test_that("json basic tfrmt",{
  #Empty tfrmt
  tfrmt() %>%
    as_json() %>%
    expect_snapshot()

  #Complete tfrmt
  basic_filled_in <- tfrmt(
    group = group,
    label = label,
    param = parm,
    value = val,
    column = c(span2, span1, my_col)) %>%
    as_json() %>%
    expect_snapshot()
})


test_that("json Titles and subtitle", {
  # Titles and subtitle
  tfrmt(
        title = "Test Title",
        subtitle = "Also a test"
  ) %>%
    as_json() %>%
    expect_snapshot()
})

test_that("json row group plans",{
  tfrmt(
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = c("A","C"), element_block(post_space = "---")),
      row_grp_structure(group_val = c("B"), element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "column")
    )
  ) %>%
    as_json() %>%
    expect_snapshot()

  tfrmt(
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = list(grp1 = "A", grp2 = "b"), element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "spanning")
    )
  ) %>%
    as_json() %>%
    expect_snapshot()
})

test_that("json body plan", {
  tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = "group1",
        label_val = ".default",
        frmt("XXX")
      )
    )
  ) %>%
    as_json() %>%
    expect_snapshot()

  tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = list(grp_col1 = "group1", grp_col2 = "subgroup"),
        label_val = ".default",
        frmt("XXX")
      )
    )
  ) %>%
    as_json() %>%
    expect_snapshot()

  #Format when test
  tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_when(
          ">3" ~ frmt("(X.X%)"),
          "<=3" ~ frmt("Undetectable")
          )
      )
    )
  ) %>%
    as_json() %>%
    expect_snapshot()

  #Format combine test
  tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{param1} {param2}",
          param1 = frmt("XXX %"),
          param2 = frmt("XX.XXX")
        )
      )
    )
  ) %>%
    as_json() %>%
    expect_snapshot()
  #Scientific test
  tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = "group1",
        label_val = ".default",
        frmt("xx.xx", scientific = "x10^xx")
      )
    )
  ) %>%
    as_json() %>%
    expect_snapshot()

  #Everything test
  tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{param1} {param2}",
          param1 = frmt("XXX %"),
          param2 = frmt_when(
            ">3" ~ frmt("(X.X%)"),
            "<=3" ~ frmt("Undetectable")
          )
        )
      ),
      frmt_structure(
        group_val = "test1",
        label_val = ".default",
        foo = frmt("xx.x")
      ),
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_when(
          ">0.4" ~ frmt("(X.X%)"),
          "<=0.4" ~ frmt_combine("[{par2m1}-{param2}]",
                                 param1 = frmt("XXX"),
                                 param2 = frmt("XXX"))
        )
      )
    )
  ) %>%
    as_json() %>%
    expect_snapshot()
})


# big n's
test_that("json big n", {
  tfrmt(
    big_n = big_n_structure(param_val = "bigN", n_frmt = frmt("\nN = xx"))
  ) %>%
    as_json() %>%
    expect_snapshot()

})

#Footnote plans
test_that("json footnote plan", {
  tfrmt(
    footnote_plan = footnote_plan(
      footnote_structure(footnote_text = "Source Note"),
      footnote_structure(footnote_text = "Placebo", column_val = "PL"),
      marks = "standard")
  ) %>%
    as_json() %>%
    expect_snapshot()

  # multiple columns
  tfrmt(
    footnote_plan = footnote_plan(
      footnote_structure(footnote_text = "All Treatments",
                         column_val = list(column = c("T1","T2", "T1&T2"))),
      marks = "numbers")
  ) %>%
    as_json() %>%
    expect_snapshot()

  # group and labels
  tfrmt(
    footnote_plan = footnote_plan(
      footnote_structure(footnote_text = "Footnote goes here",
                         group_val = "group 1", label_val = "label 1"))
  ) %>%
    as_json() %>%
    expect_snapshot()

  # Nest columns
  tfrmt(
    footnote_plan = footnote_plan(
      footnote_structure(footnote_text = "Footnote goes here",
                         column_val = list(span = "Treatment", column = "T1&T2"))
    )
  ) %>%
    as_json() %>%
    expect_snapshot()

})

test_that("json col_plan", {
  #Basic test
  tfrmt(col_plan = col_plan(col1, col2, col3)) %>%
    as_json()%>%
    expect_snapshot()

  #Basic renaming
  tfrmt(col_plan = col_plan("foo"=col1, col2, col3)) %>%
    as_json() %>%
    expect_snapshot()

  #Basic tidyselect
  base_ts <- tfrmt(col_plan = col_plan(starts_with("col"))) %>%
    as_json() %>%
    expect_snapshot()

  #Basic span strucure
  tfrmt(
    column = c(span1, col),
    col_plan = col_plan(span_structure(span1 = c("col 4")))) %>%
    as_json() %>%
    expect_snapshot()


    # Span structure test
  span_tfrmt <- tfrmt(
    group = group,
    label = label,
    param = parm,
    value = val,
    column = c(span2, span1, my_col),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("x"))
    ),
    col_plan = col_plan(
      group,
      label,
      span_structure(span1 = c("col 4")),
      span_structure(span1 = c("cols 1,2"), my_col = c("col2", "col1")),
      everything(),
      new_col_3 = mycol3,
      -mycol5
    )
  ) %>%
    as_json() %>%
    expect_snapshot()


})

#col_style_plan
test_that("json col_plan",{
  tfrmt(
    col_style_plan= col_style_plan(
      col_style_structure(align = "left", width = 100, col = "my_var"),
      col_style_structure(align = "right", col = vars(four)),
      col_style_structure(align = c(".", ",", " "), col = vars(two, three))
    )
  ) %>%
    as_json()
})


# test_that("json Writing out", {
#   test_loc <- tempfile(fileext = ".json")
#   tfrmt(
#     # specify columns in the data
#     group = c(rowlbl1,grp),
#     label = rowlbl2,
#     column = column,
#     param = param,
#     value = value,
#     sorting_cols = c(ord1, ord2),
#     # specify value formatting
#     body_plan = body_plan(
#       frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
#                                                                                   n = frmt("xxx"),
#                                                                                   pct = frmt_when("==100" ~ "",
#                                                                                                   "==0" ~ "",
#                                                                                                   TRUE ~ frmt("(xx.x %)")))),
#       frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
#       frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min","Max"), frmt("xxx.x")),
#       frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx")),
#       frmt_structure(group_val = ".default", label_val = ".default", p = frmt("")),
#       frmt_structure(group_val = ".default", label_val = c("n","<65 yrs","<12 months","<25"), p = frmt_when(">0.99" ~ ">0.99",
#                                                                                                             "<0.001" ~ "<0.001",
#                                                                                                             TRUE ~ frmt("x.xxx", missing = "")))
#     ),
#     # remove extra cols
#     col_plan = col_plan(-grp,
#                         -starts_with("ord") ),
#     # Specify column styling plan
#     col_style_plan = col_style_plan(
#       col_style_structure(align = c(".",","," "), col = vars(everything()))
#     ),
#
#     # Specify row group plan
#     row_grp_plan = row_grp_plan(
#       row_grp_structure(group_val = ".default", element_block(post_space = " ")),
#       label_loc = element_row_grp_loc(location = "column")
#     )
#
#   ) %>%
#     tfrmt_to_json(path = test_loc)
#
# })
#
#
