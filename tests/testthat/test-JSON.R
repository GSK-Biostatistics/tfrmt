test_that("json basic tfrmt",{
  #Empty tfrmt
  tfrmt() %>%
    tfrmt_to_json() %>%
    expect_snapshot()

  #Complete tfrmt
  basic_filled_in <- tfrmt(
    group = group,
    label = label,
    param = parm,
    value = val,
    column = c(span2, span1, my_col)) %>%
    tfrmt_to_json() %>%
    expect_snapshot()
})


test_that("json Titles and subtitle", {
  # Titles and subtitle
  tfrmt(
        title = "Test Title",
        subtitle = "Also a test"
  ) %>%
    tfrmt_to_json() %>%
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
    tfrmt_to_json() %>%
    expect_snapshot()

  tfrmt(
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = list(grp1 = "A", grp2 = "b"), element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "spanning")
    )
  ) %>%
    tfrmt_to_json() %>%
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
    tfrmt_to_json() %>%
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
    tfrmt_to_json() %>%
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
      ),
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
    tfrmt_to_json()

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
    tfrmt_to_json()
  #Scientific test

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
        group_val = ".default",
        label_val = ".default",
        foo = frmt("xx.x")
      ),
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
    tfrmt_to_json()
})
#Body Plans

# big n's

#Footnote plans

test_that("json col_plan", {
  #Basic test
  tfrmt(col_plan = col_plan(col1, col2, col3)) %>%
    tfrmt_to_json()%>%
    expect_snapshot()

  #Basic renaming
  tfrmt(col_plan = col_plan("foo"=col1, col2, col3)) %>%
    tfrmt_to_json() %>%
    expect_snapshot()

  #Basic tidyselect
  base_ts <- tfrmt(col_plan = col_plan(starts_with("col"))) %>%
    tfrmt_to_json() %>%
    expect_snapshot()

  #Basic span strucure
  tfrmt(
    column = c(span1, col),
    col_plan = col_plan(span_structure(span1 = c("col 4")))) %>%
    tfrmt_to_json() %>%
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
  tfrmt_to_json() %>%
    expect_snapshot()


})


#col_style_plan
#sorting columns




