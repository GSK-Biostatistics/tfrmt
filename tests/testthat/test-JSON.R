test_that("json basic tfrmt",{
  #Empty tfrmt
  #Writing
  tfrmt() %>%
    as_json() %>%
    expect_snapshot()
  #Reading
  tfrmt() %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(tfrmt())

  #Complete tfrmt
  basic_filled_in <- tfrmt(
    group = group,
    label = label,
    param = parm,
    value = val,
    column = c(span2, span1, my_col))
  # Writing
  basic_filled_in %>%
    as_json() %>%
    expect_snapshot()
  # Reading
  basic_filled_in %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(basic_filled_in, ignore_attr = TRUE)

})


test_that("json Titles and subtitle", {
  # Titles and subtitle
  titles <- tfrmt(
    title = "Test Title",
    subtitle = "Also a test"
  )
  titles %>%
    as_json() %>%
    expect_snapshot()

  titles %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(titles, ignore_attr = TRUE)
})

test_that("json row group plans",{
  rgp <- tfrmt(
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = c("A","C"), element_block(post_space = "---")),
      row_grp_structure(group_val = c("B"), element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "column")
    )
  )

  rgp %>%
    as_json() %>%
    expect_snapshot()

  rgp %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(rgp, ignore_attr = TRUE )

  rgp_named <- tfrmt(
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = list(grp1 = "A", grp2 = "b"), element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "spanning")
    )
  )
  rgp_named %>%
    as_json() %>%
    expect_snapshot()

  rgp_named %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(rgp_named, ignore_attr = TRUE )
})

test_that("json body plan", {
  frmt1 <- tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = "group1",
        label_val = ".default",
        frmt("XXX")
      )
    )
  )

  frmt1 %>%
    as_json() %>%
    expect_snapshot()
  frmt1 %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(frmt1, ignore_attr = TRUE)



  frmt2 <- tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = list(grp_col1 = "group1", grp_col2 = "subgroup"),
        label_val = ".default",
        test = frmt("XXX")
      )
    )
  )

  frmt2 %>%
    as_json() %>%
    expect_snapshot()

  frmt2 %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(frmt2, ignore_attr = TRUE)

  frmt3 <- tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = "group1",
        label_val = ".default",
        frmt("XXX", transform = ~.*100)
      )
    )
  )

  frmt3 %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(frmt3, ignore_attr = TRUE)

  frmt4 <- tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = "group1",
        label_val = ".default",
        frmt("XXX", transform = function(x){x*4})
      )
    )
  )
  new_frmt <- frmt4 %>%
    as_json() %>%
    json_to_tfrmt(json = .)
  # Get out the function to see if it runs the same
  fx1 <- frmt4[[6]][[1]][[4]][[1]][[4]]
  fx2 <- new_frmt[[6]][[1]][[4]][[1]][[4]]
  expect_equal(fx1(1:5), fx2(1:5))

  #Format when test
  frmt_when_simp <- tfrmt(
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
  )
  frmt_when_simp %>%
    as_json() %>%
    expect_snapshot()

  frmt_when_simp %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(frmt_when_simp, ignore_attr = TRUE)
  #Format combine test
  frmt_comb_simp <- tfrmt(
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
  )
  frmt_comb_simp %>%
    as_json() %>%
    expect_snapshot()
  frmt_comb_simp %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(frmt_comb_simp, ignore_attr = TRUE)
  #Scientific test
  sci <- tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = "group1",
        label_val = ".default",
        frmt("xx.xx", scientific = "x10^xx")
      )
    )
  )

  sci %>%
    as_json() %>%
    expect_snapshot()

  sci %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(sci, ignore_attr = TRUE)

  #Everything test
  complex_frmt <- tfrmt(
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
  )

  complex_frmt %>%
    as_json() %>%
    expect_snapshot()

  complex_frmt %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(complex_frmt, ignore_attr = TRUE)
})


# big n's
test_that("json big n", {
  big_n <- tfrmt(
    big_n = big_n_structure(param_val = "bigN", n_frmt = frmt("\nN = xx"))
  )
  big_n %>%
    as_json() %>%
    expect_snapshot()

  big_n %>%
    as_json() %>%
    json_to_tfrmt(json = . ) %>%
    expect_equal(big_n, ignore_attr = TRUE)

})

#Footnote plans
test_that("json footnote plan", {
  fn_simp <- tfrmt(
    footnote_plan = footnote_plan(
      footnote_structure(footnote_text = "Source Note"),
      footnote_structure(footnote_text = "Placebo", column_val = "PL"),
      marks = "standard")
  )
  fn_simp %>%
    as_json() %>%
    expect_snapshot()

  fn_simp%>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(fn_simp, ignore_attr = TRUE)

  # multiple columns
  fn_cols <- tfrmt(
    footnote_plan = footnote_plan(
      footnote_structure(footnote_text = "All Treatments",
                         column_val = list(column = c("T1","T2", "T1&T2"))),
      marks = "numbers")
  )
  fn_cols %>%
    as_json() %>%
    expect_snapshot()

  fn_cols%>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(fn_cols, ignore_attr = TRUE)

  # group and labels
  gl_fn <- tfrmt(
    footnote_plan = footnote_plan(
      footnote_structure(footnote_text = "Footnote goes here",
                         group_val = "group 1", label_val = "label 1"))
  )
  gl_fn %>%
    as_json() %>%
    expect_snapshot()
  gl_fn %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(gl_fn)

  # Nest columns
  nested_fn <- tfrmt(
    footnote_plan = footnote_plan(
      footnote_structure(footnote_text = "Footnote goes here",
                         column_val = list(span = "Treatment", column = "T1&T2"))
    )
  )
  nested_fn %>%
    as_json() %>%
    expect_snapshot()
  nested_fn %>%
    as_json() %>%
    json_to_tfrmt(json = . ) %>%
    expect_equal(nested_fn, ignore_attr = TRUE)

})

test_that("json col_plan", {
  #Basic test
  cp <- tfrmt(col_plan = col_plan(col1, col2, col3))
  cp %>%
    as_json()%>%
    expect_snapshot()

  cp %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(cp, ignore_attr = TRUE)

  #Basic renaming
  rename <- tfrmt(col_plan = col_plan("foo"=col1, col2, col3))

  rename %>%
    as_json() %>%
    expect_snapshot()

  rename %>%
    as_json() %>%
    json_to_tfrmt(json = .)%>%
    expect_equal(rename, ignore_attr = TRUE)

  #Basic tidyselect
  base_ts <- tfrmt(col_plan = col_plan(starts_with('col')))
  base_ts %>%
    as_json() %>%
    expect_snapshot()

  base_ts %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(base_ts, ignore_attr = c(".Environment"))

  #Basic span structure
  span <- tfrmt(
    column = c(span1, col),
    col_plan = col_plan(span_structure(span1 = c("col 4"))))

  span %>%
    as_json() %>%
    expect_snapshot()

  span %>%
    as_json() %>%
    json_to_tfrmt(json = .) %>%
    expect_equal(span, ignore_attr = TRUE)


  # Span structure test
  span_tfrmt <- tfrmt(
    column = c(span2, span1, my_col),
    col_plan = col_plan(
      group,
      label,
      span_structure(span1 = c("col 4")),
      span_structure(span1 = c("cols 1,2"), my_col = c("col2", "col1")),
      span_structure(
        span2 = "Top Label Level 1",
        span1 = "Second Label Level 1.1",
        my_col = c(col_3, col_4)
      ),
      span_structure(
        span2 = "Top Label Level 1",
        span1 = c("col2", "col1"),
        my_col = starts_with("B")
      ),
      span_structure(
        span2 = "Top Label Level 1",
        my_col = col_5
      ),
      span_structure(
        span2 = "Top Label Level 2",
        my_col = c(col_6, col_7)
      ),
      everything(),
      new_col_3 = mycol3,
      -mycol5
    )
  )
  span_tfrmt %>%
    as_json() %>%
    expect_snapshot()

  span_tfrmt %>%
    as_json() %>%
    json_to_tfrmt(json = . )%>%
    expect_equal(span_tfrmt, ignore_attr =TRUE)


})

#col_style_plan
test_that("json col_style_plan",{
  csp <- tfrmt(
    col_style_plan= col_style_plan(
      col_style_structure(align = "left", width = 100, col = "my_var"),
      col_style_structure(align = "right", col = vars(four)),
      col_style_structure(align = c(".", ",", " "), col = vars(two, three)),
      col_style_structure(width = 25, col = span_structure(span = value, col = val2))
    )
  )
  csp %>%
    as_json() %>%
    expect_snapshot()
  csp %>%
    as_json() %>%
    json_to_tfrmt(json = . ) %>%
    expect_equal(csp, ignore_attr = TRUE)
})


test_that("json read/write", {
  test_loc <- "test.json"
  test_tfrmt <- tfrmt(
    # specify columns in the data
    group = c(rowlbl1,grp),
    label = rowlbl2,
    column = column,
    param = param,
    value = value,
    sorting_cols = c(ord1, ord2),
    # specify value formatting
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)")))),
      frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
      frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min","Max"), frmt("xxx.x")),
      frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx")),
      frmt_structure(group_val = ".default", label_val = ".default", p = frmt("")),
      frmt_structure(group_val = ".default", label_val = c("n","<65 yrs","<12 months","<25"), p = frmt_when(">0.99" ~ ">0.99",
                                                                                                            "<0.001" ~ "<0.001",
                                                                                                            TRUE ~ frmt("x.xxx", missing = "")))
    ),
    # remove extra cols
    col_plan = col_plan(-grp,
                        -starts_with("ord") ),
    # Specify column styling plan
    col_style_plan = col_style_plan(
      col_style_structure(align = c(".",","," "), col = vars(everything()))
    ),

    # Specify row group plan
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "column")
    )

  )

  #Write out to json file
  tfrmt_to_json(test_tfrmt, path = test_loc)

  # Reading in
  read_tfrmt <- json_to_tfrmt(path = test_loc)

  expect_equal(read_tfrmt, test_tfrmt,
               ignore_attr = TRUE)

})


