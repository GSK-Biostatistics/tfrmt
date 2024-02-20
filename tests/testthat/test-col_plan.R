
test_that("Defining the spanning structure", {

  s1 <- span_structure(
    c1 = "Test Label",
    c2 = vars(A,B)
  )

  s2 <- span_structure(
    c1 = "Test Label",
    c2 = "Test Sub Label",
    c3 = vars(A,B)
  )

  expect_s3_class(s1,"span_structure")
  expect_s3_class(s2,"span_structure")

  expect_equal(s1[["c1"]], list(quo(`Test Label`)), ignore_attr = c(".Environment"))
  expect_equal(s1[["c2"]], list(quo(`A`),quo(`B`)), ignore_attr = c(".Environment"))
  expect_equal(s1[["c1"]], list(quo(`Test Label`)), ignore_attr = c(".Environment"))
  expect_equal(s2[["c2"]], list(quo(`Test Sub Label`)), ignore_attr = c(".Environment"))
  expect_equal(s2[["c3"]], list(quo(`A`),quo(`B`)), ignore_attr = c(".Environment"))

})

test_that("Defining the col plan",{

  s1 <- col_plan(
    span_structure(
      c1 = "Test Label",
      c2 = vars(A,B)
    ))

  expect_s3_class(s1,"col_plan")
  expect_s3_class(s1,"plan")

})

test_that("Defining the col plan in tfrmt", {

  #Go into object
  tfrmt_test <- tfrmt(
    column = vars(col1, col2),
    col_plan = col_plan(
      span_structure(col1 = "test label",
                     col2 = vars(col3)
                     )
      )
    )

  expect_true(!is.null(tfrmt_test$col_plan))

  # expect error when writing invalid col_plan
    expect_error(
      tfrmt(
        col_plan = col_plan(
          group,label,
          span_structure(col1 = "test label", col2 = col3)
        )
      ),
      "No columns defined in the `column` argument of tfrmt but provided a span_structure() in `col_plan`.",
      fixed = TRUE
    )

    expect_error(
      tfrmt(
        column = col2,
        col_plan = col_plan(
          group,label,
          span_structure(col1 = "test label", col2 = col3)
        )
      ),
      "A single column defined in `column` argument of tfrmt but provided a span_structure() in `col_plan`.",
      fixed = TRUE
    )

    expect_error(
      tfrmt(
        column = c(col1, col2),
        col_plan = col_plan(
          group,label,
          span_structure(col11 = "test label", col2 = col3)
        )
      ),
      "Columns defined in `span_structure` are not defined columns in the tfrmt\nColumn Values: `col1`, `col2`\nInvalid Column Names in Span Structure: `col11`",
      fixed = TRUE
    )

})

test_that("Test applying a col_plan - simple",{

  col_vars <- vars(col1, col2)

  cp_keep <- col_plan(
    first_col,
    span_structure(col1 = c("test val"), col2 = c("val2","val1"))
  )

  cp_drop <- col_plan(
    first_col,
    span_structure(col1 = "test val", col2 = c("val2","val1")),
    .drop = TRUE
  )

  cp_subtraction <- col_plan(
    -first_col,
    span_structure(col1 = c("test val"), col2 = c("val2","val1"))
  )

  cp_subtraction_span <- col_plan(
    -first_col,
    span_structure(col1 = c("test val"), col2 = c(-val1))
  )

  name_col <- c(
    "first_col",
    "test val___tlang_delim___val1",
    "test val___tlang_delim___val2",
    "extra_col"
  )

  expect_equal(
    create_col_order(cp = cp_keep, columns = col_vars, data_names = name_col),
    vars(
      first_col,
      `test val___tlang_delim___val2`,
      `test val___tlang_delim___val1`,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_drop, columns = col_vars, data_names = name_col),
    vars(
      first_col,
      `test val___tlang_delim___val2`,
      `test val___tlang_delim___val1`,
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_subtraction, columns = col_vars, data_names = name_col),
    vars(
      -first_col,
      `test val___tlang_delim___val2`,
      `test val___tlang_delim___val1`,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_subtraction_span, columns = col_vars, data_names = name_col),
    vars(
      -first_col,
      -`test val___tlang_delim___val1`,
      `test val___tlang_delim___val2`,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

  name_col_mixed_order <- c(
    "first_col",
    "extra_col",
    "test val___tlang_delim___val1",
    "test val___tlang_delim___val2"
  )

  expect_equal(
    create_col_order(cp = cp_keep, columns = col_vars, data_names = name_col_mixed_order),
    vars(
      first_col,
      `test val___tlang_delim___val2`,
      `test val___tlang_delim___val1`,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_drop, columns = col_vars, data_names = name_col_mixed_order),
    vars(
      first_col,
      `test val___tlang_delim___val2`,
      `test val___tlang_delim___val1`
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_subtraction, columns = col_vars, data_names = name_col_mixed_order),
    vars(
      -first_col,
      `test val___tlang_delim___val2`,
      `test val___tlang_delim___val1`,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_subtraction_span, columns = col_vars, data_names = name_col_mixed_order),
    vars(
      -first_col,
      -`test val___tlang_delim___val1`,
      `test val___tlang_delim___val2`,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

})

test_that("Test applying a col_plan - tidyselect",{

  col_vars <- vars(col1, col2)

  cp_keep <- col_plan(
    span_structure(col1 = starts_with("test"), col2 = c(val2, everything())),
    everything(),
    first_col
  )

  cp_drop <- col_plan(
    span_structure(col1 = starts_with("test"), col2 = c(val2,everything())),
    everything(),
    first_col,
    .drop = TRUE
  )

  name_col <- c(
    "first_col",
    "test val___tlang_delim___val1",
    "test val___tlang_delim___val2",
    "extra_col"
  )

  cp_vars_keep <- create_col_order(cp = cp_keep, columns = col_vars, data_names = name_col)

  expect_equal(
    cp_vars_keep,
    vars(
      `test val___tlang_delim___val2`,
      `test val___tlang_delim___val1`,
      extra_col,
      first_col
    ),
    ignore_attr = c(".Environment")
  )

  cp_vars_drop <- create_col_order(cp = cp_drop, columns = col_vars, data_names = name_col)

  expect_equal(
    cp_vars_drop,
    vars(
      `test val___tlang_delim___val2`,
      `test val___tlang_delim___val1`,
      extra_col,
      first_col
    ),
    ignore_attr = c(".Environment")
  )

  name_col_mixed_order <- c(
    "first_col",
    "extra_col",
    "test val___tlang_delim___val1",
    "test val___tlang_delim___val2"
  )

  cp_vars_keep_mixed_order <- create_col_order(cp = cp_keep, columns = col_vars, data_names = name_col_mixed_order)

  expect_equal(
    cp_vars_keep_mixed_order,
    vars(
      `test val___tlang_delim___val2`,
      `test val___tlang_delim___val1`,
      extra_col,
      first_col
    ),
    ignore_attr = c(".Environment")
  )

})

test_that("Test applying a col_plan - renaming",{

  col_vars <- vars(col1, col2)

  cp_keep <- col_plan(
    new_first_col = first_col,
    span_structure(
      col1 = c(`new header col` = "test val"),
      col2 = c(best_value = "val2", "val1")
    ),
    preserved_col
  )

  cp_drop <- col_plan(
    new_first_col = first_col,
    span_structure(
      col1 = c(`new header col` = "test val"),
      col2 = c(best_value = "val2", "val1")
    ),
    preserved_col,
    .drop = TRUE
  )

  cp_keep_rename_spanned_col <- col_plan(
    new_first_col = first_col,
    span_structure(
      col1 = c(`new header col` = "test val")
    ),
    best_value = "val2",
    val1,
    preserved_col
  )

  cp_drop_rename_spanned_col <- col_plan(
    new_first_col = first_col,
    span_structure(
      col1 = c(`new header col` = "test val")
    ),
    best_value = "val2",
    val1,
    preserved_col,
    .drop = TRUE
  )

  name_col <- c(
    "first_col",
    "test val___tlang_delim___val1",
    "test val___tlang_delim___val2",
    "preserved_col",
    "extra_col"
  )

  name_col_mixed_order <- c(
    "first_col",
    "preserved_col",
    "extra_col",
    "test val___tlang_delim___val1",
    "test val___tlang_delim___val2"
  )

  expect_equal(
    create_col_order(cp = cp_keep, columns = col_vars, data_names = name_col),
    vars(
      new_first_col = first_col,
      `new header col___tlang_delim___best_value` = `test val___tlang_delim___val2`,
      `new header col___tlang_delim___val1` = `test val___tlang_delim___val1`,
      preserved_col,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_drop, columns = col_vars, data_names = name_col),
    vars(
      new_first_col = first_col,
      `new header col___tlang_delim___best_value` = `test val___tlang_delim___val2`,
      `new header col___tlang_delim___val1` = `test val___tlang_delim___val1`,
      preserved_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_keep_rename_spanned_col, columns = col_vars, data_names = name_col),
    vars(
      new_first_col = first_col,
      `new header col___tlang_delim___best_value` = `test val___tlang_delim___val2`,
      `new header col___tlang_delim___val1` = `test val___tlang_delim___val1`,
      preserved_col,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_drop_rename_spanned_col, columns = col_vars, data_names = name_col),
    vars(
      new_first_col = first_col,
      `new header col___tlang_delim___best_value` = `test val___tlang_delim___val2`,
      `new header col___tlang_delim___val1` = `test val___tlang_delim___val1`,
      preserved_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_keep, columns = col_vars, data_names = name_col_mixed_order),
    vars(
      new_first_col = first_col,
      `new header col___tlang_delim___best_value` = `test val___tlang_delim___val2`,
      `new header col___tlang_delim___val1` = `test val___tlang_delim___val1`,
      preserved_col,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_drop, columns = col_vars, data_names = name_col_mixed_order),
    vars(
      new_first_col = first_col,
      `new header col___tlang_delim___best_value` = `test val___tlang_delim___val2`,
      `new header col___tlang_delim___val1` = `test val___tlang_delim___val1`,
      preserved_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_keep_rename_spanned_col, columns = col_vars, data_names = name_col_mixed_order),
    vars(
      new_first_col = first_col,
      `new header col___tlang_delim___best_value` = `test val___tlang_delim___val2`,
      `new header col___tlang_delim___val1` = `test val___tlang_delim___val1`,
      preserved_col,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_drop_rename_spanned_col, columns = col_vars, data_names = name_col_mixed_order),
    vars(
      new_first_col = first_col,
      `new header col___tlang_delim___best_value` = `test val___tlang_delim___val2`,
      `new header col___tlang_delim___val1` = `test val___tlang_delim___val1`,
      preserved_col
    ),
    ignore_attr = c(".Environment")
  )

})

test_that("Test applying a col_plan - renaming twice",{

  col_vars <- vars(col1, col2)

  cp_keep <- col_plan(
    new_first_col = first_col,
    span_structure(
      col1 = c(`new header col` = "test val"),
      col2 = c(best_value = "val2", "val1")
    ),
    new_best_value = val2,
    val1,
    preserved_col
  )

  cp_drop <- col_plan(
    new_first_col = first_col,
    span_structure(
      col1 = c(`new header col` = "test val"),
      col2 = c(best_value = "val2", "val1")
    ),
    new_best_value = val2,
    val1,
    preserved_col,
    .drop = TRUE
  )

  name_col <- c(
    "first_col",
    "test val___tlang_delim___val1",
    "test val___tlang_delim___val2",
    "preserved_col",
    "extra_col"
  )

  expect_equal(
    create_col_order(cp = cp_keep, columns = col_vars, data_names = name_col),
    vars(
      new_first_col = first_col,
      `new header col___tlang_delim___new_best_value` = `test val___tlang_delim___val2`,
      `new header col___tlang_delim___val1` = `test val___tlang_delim___val1`,
      preserved_col,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_drop, columns = col_vars, data_names = name_col),
    vars(
      new_first_col = first_col,
      `new header col___tlang_delim___new_best_value` = `test val___tlang_delim___val2`,
      `new header col___tlang_delim___val1` = `test val___tlang_delim___val1`,
      preserved_col
    ),
    ignore_attr = c(".Environment")
  )

  name_col_mixed_order <- c(
    "first_col",
    "preserved_col",
    "extra_col",
    "test val___tlang_delim___val1",
    "test val___tlang_delim___val2"
  )

  expect_equal(
    create_col_order(cp = cp_keep, columns = col_vars, data_names = name_col_mixed_order),
    vars(
      new_first_col = first_col,
      `new header col___tlang_delim___new_best_value` = `test val___tlang_delim___val2`,
      `new header col___tlang_delim___val1` = `test val___tlang_delim___val1`,
      preserved_col,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

  expect_equal(
    create_col_order(cp = cp_drop, columns = col_vars, data_names = name_col_mixed_order),
    vars(
      new_first_col = first_col,
      `new header col___tlang_delim___new_best_value` = `test val___tlang_delim___val2`,
      `new header col___tlang_delim___val1` = `test val___tlang_delim___val1`,
      preserved_col
    ),
    ignore_attr = c(".Environment")
  )

})

test_that("Test applying a col_plan - tidyselect with renaming",{

  col_vars <- vars(col1, col2)

  cp_keep <- col_plan(
    span_structure(col1 = c(new_col_name = starts_with("test")), col2 = c(val2, everything())),
    everything(),
    first_col,
    starts_with("col")
  )

  cp_drop <- col_plan(
    span_structure(col1 = c(new_col_name = starts_with("test")), col2 = c(val2, everything())),
    everything(),
    first_col,
    starts_with("col"),
    .drop = TRUE
  )

  name_col <- c(
    "first_col",
    "test one___tlang_delim___val1",
    "test two___tlang_delim___val2",
    "extra_col",
    "col1",
    "col2",
    "col3"
  )

  cp_vars_keep <- create_col_order(cp = cp_keep, columns = col_vars, data_names = name_col)

  expect_equal(
    cp_vars_keep,
    vars(
      new_col_name1___tlang_delim___val1 = `test one___tlang_delim___val1`,
      new_col_name2___tlang_delim___val2 = `test two___tlang_delim___val2`,
      extra_col,
      first_col,
      col1,
      col2,
      col3
    ),
    ignore_attr = c(".Environment")
  )

  cp_vars_drop <- create_col_order(cp = cp_drop, columns = col_vars, data_names = name_col)

  expect_equal(
    cp_vars_drop,
    vars(
      new_col_name1___tlang_delim___val1 = `test one___tlang_delim___val1`,
      new_col_name2___tlang_delim___val2 = `test two___tlang_delim___val2`,
      extra_col,
      first_col,
      col1,
      col2,
      col3
    ),
    ignore_attr = c(".Environment")
  )

  name_col_mixed_order <- c(
    "col2",
    "first_col",
    "test two___tlang_delim___val2",
    "test one___tlang_delim___val1",
    "extra_col",
    "col1",
    "col3"
  )

  cp_vars_keep_mixed_order <- create_col_order(cp = cp_keep, columns = col_vars, data_names = name_col_mixed_order)

  expect_equal(
    cp_vars_keep_mixed_order,
    vars(
      new_col_name1___tlang_delim___val2 = `test two___tlang_delim___val2`,
      new_col_name2___tlang_delim___val1 = `test one___tlang_delim___val1`,
      extra_col,
      first_col,
      col2,
      col1,
      col3
    ),
    ignore_attr = c(".Environment")
  )

})

test_that("Test applying a col_plan - ordering on multiple columns",{

  col_vars <- vars(c1, c2)

  cp <- col_plan(
    span_structure(c1 = c(`test val2`, `test val1`, `another val`), c2 = c(val2, val1, everything())),
    first_col,
    extra_col
  )

  name_col <- c(
    "first_col",
    "test val1___tlang_delim___val1",
    "test val1___tlang_delim___val2",
    "test val2___tlang_delim___val1",
    "test val2___tlang_delim___val2",
    "another val___tlang_delim___val1",
    "another val___tlang_delim___val2",
    "another val___tlang_delim___val3",
    "preserved_col",
    "extra_col"
  )

  expect_equal(
    create_col_order(cp = cp, columns = col_vars, data_names = name_col),
    vars(
      `test val2___tlang_delim___val2`,
      `test val2___tlang_delim___val1`,
      `test val1___tlang_delim___val2`,
      `test val1___tlang_delim___val1`,
      `another val___tlang_delim___val2`,
      `another val___tlang_delim___val1`,
      `another val___tlang_delim___val3`,
      first_col,
      extra_col,
      preserved_col
    ),
    ignore_attr = c(".Environment")
  )

  cp <- col_plan(
    span_structure(c1 = c(`test val2`, `test val1`, `another val`), c2 = c(val2, val1)),
    preserved_col,
    span_structure(c1 = c(`another val`), c2 = c(val3)),
    first_col,
    extra_col
  )

  cp_vars <- create_col_order(cp = cp, columns = col_vars, data_names = name_col)

  expect_equal(
    cp_vars,
    vars(
      `test val2___tlang_delim___val2`,
      `test val2___tlang_delim___val1`,
      `test val1___tlang_delim___val2`,
      `test val1___tlang_delim___val1`,
      `another val___tlang_delim___val2`,
      `another val___tlang_delim___val1`,
      preserved_col,
      `another val___tlang_delim___val3`,
      first_col,
      extra_col
    ),
    ignore_attr = c(".Environment")
  )

})

test_that("Unorthodox col_plans",{

  col_vars <- vars(c1, c2)

  cp <- col_plan(
    val4,
    span_structure(c1 = "test value", c2 = c(val1, val3)),
    val2, val6, val3,
    everything(),
    val5
  )

  name_col <- c(
    "test value___tlang_delim___val1",
    "test value___tlang_delim___val2",
    "test value___tlang_delim___val3",
    "val4",
    "val5",
    "val6",
    "val7"
  )

  expect_equal(
    create_col_order(cp = cp, columns = col_vars, data_names = name_col),
    vars(
      val4,
      `test value___tlang_delim___val1`,
      `test value___tlang_delim___val2`,
      val6,
      `test value___tlang_delim___val3`,
      val7,
      val5
    ),
    ignore_attr = c(".Environment")
  )

  cp2 <- col_plan(
    val4,
    span_structure(c2 = c(val1, val3, everything())),
    val5,
    span_structure(c2 = c(val1)),
    -val6,
    -val7,
    .drop = TRUE
  )

  expect_equal(
    create_col_order(cp = cp2, columns = col_vars, data_names = name_col),
    vars(
      val4,
      `test value___tlang_delim___val3`,
      `test value___tlang_delim___val2`,
      val5,
      `test value___tlang_delim___val1`,
      -val6,
      -val7
    ),
    ignore_attr = c(".Environment")
  )

})

test_that("Order is kept for multi-col columns",{

  test <- tibble(col_1 = "test",
                 col_2 = c("this", "other"),
                 col_3 = c("delm", "delm"),
                 label = "label",
                 param = "pam",
                 value = c(1.08089, 9.23948))


  tfrmt <- tfrmt(
    label = label,
    param = param,
    value = value,
    column = vars(col_1, col_2, col_3),
    body_plan = body_plan(
      frmt_structure(pam = frmt("x.xx"))
    ),
    col_style_plan = col_style_plan(
      col_style_structure(align = ".", col = delm),
      col_style_structure(align = "right", col = everything())
      )
  )


  new_name_ord <- apply_tfrmt(test, tfrmt) %>%
    select(-label) %>%
    names()

  new_name_ord_in_dat <- test %>%
    select(starts_with("col")) %>%
    unite("new", sep=.tlang_delim) %>%
    pull(new)

  expect_equal(new_name_ord, new_name_ord_in_dat)

})

test_that("Build simple tfrmt with multiple columns and apply to basic data and compare against spanning_structure",{

  basic_multi_column_template <- tfrmt(
    group = group,
    label = quo(label),
    param = parm,
    value = val,
    column = c(test1,test2),
    col_plan = col_plan(
      group,
      label,
      col4,
      span_structure(test1 = `span 1`, test2 = c(col1, col2)),
      col3,
      -col5
    ),
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(location = "spanning"))
  )

  basic_example_dataset <- tibble::tribble(
    ~group,     ~label,    ~test1, ~test2,    ~parm, ~val,
    "g1", "rowlabel1",  "span 1", "col1",  "value",    1,
    "g1", "rowlabel1",  "span 1", "col2",  "value",    1,
    "g1", "rowlabel1",        NA, "col3",  "value",    1,
    "g1", "rowlabel1",        NA, "col4",  "value",    1,
    "g1", "rowlabel1",        NA, "col5",  "value",    1,
    "g1", "rowlabel2",  "span 1", "col1",  "value",    2,
    "g1", "rowlabel2",  "span 1", "col2",  "value",    2,
    "g1", "rowlabel2",        NA, "col3",  "value",    2,
    "g1", "rowlabel2",        NA, "col4",  "value",    2,
    "g1", "rowlabel2",        NA, "col5",  "value",    2,
    "g2", "rowlabel3",  "span 1", "col1",  "value",    3,
    "g2", "rowlabel3",  "span 1", "col2",  "value",    3,
    "g2", "rowlabel3",        NA, "col3",  "value",    3,
    "g2", "rowlabel3",        NA, "col4",  "value",    3,
    "g2", "rowlabel3",        NA, "col5",  "value",    3,
  )

  suppressMessages({
    processed_gt <- print_to_gt(tfrmt = basic_multi_column_template, .data = basic_example_dataset)
  })

  expect_equal(
    processed_gt[["_boxhead"]]$column_label %>% map_chr(as.character),
    c("group", "label", "col4", "col1", "col2", "col3", "..tfrmt_row_grp_lbl"
    )
  )

  expect_equal(
    processed_gt[["_spanners"]]$spanner_label %>% map_chr(as.character),
    c("span 1")
  )

  expect_equal(
    processed_gt[["_spanners"]]$vars,
    list(
      c("span 1___tlang_delim___col1", "span 1___tlang_delim___col2")
    )
  )

})

test_that("Build simple tfrmt with multiple columns and apply to basic data and compare against spanning_structure - with renaming",{

  basic_multi_column_template <- tfrmt(
    group = group,
    label = quo(label),
    param = parm,
    value = val,
    column = c(test1,test2),
    col_plan = col_plan(
      new_col_4 = col4, new_col_1 = col1, col2, col3, -col5
    ),
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(location = "spanning"))
  )


  basic_example_dataset <- tibble::tribble(
    ~group,     ~label,    ~test1, ~test2,    ~parm, ~val,
    "g1", "rowlabel1",  "span 1", "col1",  "value",    1,
    "g1", "rowlabel1",  "span 1", "col2",  "value",    1,
    "g1", "rowlabel1",        NA, "col3",  "value",    1,
    "g1", "rowlabel1",        NA, "col4",  "value",    1,
    "g1", "rowlabel1",        NA, "col5",  "value",    1,
    "g1", "rowlabel2",  "span 1", "col1",  "value",    2,
    "g1", "rowlabel2",  "span 1", "col2",  "value",    2,
    "g1", "rowlabel2",        NA, "col3",  "value",    2,
    "g1", "rowlabel2",        NA, "col4",  "value",    2,
    "g1", "rowlabel2",        NA, "col5",  "value",    2,
    "g2", "rowlabel3",  "span 1", "col1",  "value",    3,
    "g2", "rowlabel3",  "span 1", "col2",  "value",    3,
    "g2", "rowlabel3",        NA, "col3",  "value",    3,
    "g2", "rowlabel3",        NA, "col4",  "value",    3,
    "g2", "rowlabel3",        NA, "col5",  "value",    3,
  )

  suppressMessages({
      processed_gt <- print_to_gt(tfrmt = basic_multi_column_template, .data = basic_example_dataset)
  })


  expect_equal(
    processed_gt[["_boxhead"]]$column_label %>% map_chr(as.character),
    c( "new_col_4", "new_col_1", "col2", "col3", "group", "label", "..tfrmt_row_grp_lbl" )
  )

  expect_equal(
    processed_gt[["_spanners"]]$spanner_label %>% map_chr(as.character),
    c("span 1")
  )

  expect_equal(
    processed_gt[["_spanners"]]$vars,
    list(
      c("span 1___tlang_delim___new_col_1", "span 1___tlang_delim___col2")
    )
  )

})

test_that("Build simple tfrmt with multiple columns and apply to basic data and compare against spanning_structure - with renaming multiple levels",{

  basic_multi_column_template <- tfrmt(
    group = group,
    label = quo(label),
    param = parm,
    value = val,
    column = c(test1,test2),
    col_plan = col_plan(
      new_col_4 = col4,
      span_structure(
        test1 = c(`new span name` = "span 1"),
        test2 = c(new_col_1 = col1, col2)
      ),
      col2, col3, -col5
    ),
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(location = "spanning"))
  )


  basic_example_dataset <- tibble::tribble(
    ~group,     ~label,    ~test1, ~test2,    ~parm, ~val,
    "g1", "rowlabel1",  "span 1", "col1",  "value",    1,
    "g1", "rowlabel1",  "span 1", "col2",  "value",    1,
    "g1", "rowlabel1",        NA, "col3",  "value",    1,
    "g1", "rowlabel1",        NA, "col4",  "value",    1,
    "g1", "rowlabel1",        NA, "col5",  "value",    1,
    "g1", "rowlabel2",  "span 1", "col1",  "value",    2,
    "g1", "rowlabel2",  "span 1", "col2",  "value",    2,
    "g1", "rowlabel2",        NA, "col3",  "value",    2,
    "g1", "rowlabel2",        NA, "col4",  "value",    2,
    "g1", "rowlabel2",        NA, "col5",  "value",    2,
    "g2", "rowlabel3",  "span 1", "col1",  "value",    3,
    "g2", "rowlabel3",  "span 1", "col2",  "value",    3,
    "g2", "rowlabel3",        NA, "col3",  "value",    3,
    "g2", "rowlabel3",        NA, "col4",  "value",    3,
    "g2", "rowlabel3",        NA, "col5",  "value",    3,
  )

  suppressMessages({
    processed_gt <- print_to_gt(tfrmt = basic_multi_column_template, .data = basic_example_dataset)
  })

  expect_equal(
    processed_gt[["_boxhead"]]$column_label %>% map_chr(as.character),
    c( "new_col_4", "new_col_1", "col2", "col3", "group","label","..tfrmt_row_grp_lbl")
  )

  expect_equal(
    processed_gt[["_spanners"]]$spanner_label %>% map_chr(as.character),
    c("new span name")
  )

  expect_equal(
    processed_gt[["_spanners"]]$vars,
    list(
      c("new span name___tlang_delim___new_col_1", "new span name___tlang_delim___col2")
    )
  )

})

test_that("Build simple tfrmt with multiple columns and with renaming duplicated colnames across spans",{

  multi_col_df <- tibble::tribble(
    ~label, ~col0, ~col1, ~col2, ~param, ~value,
    "A",    "A_",  "A",    "AA",  "p1", 123,
    "A",    "A_",  "A",    "AB",  "p1", 12,
    "A",    "A_",  "B",    "AA",  "p1", 1,
    "A",    "A_",  "B",    "AB",  "p1", 123,
    "A",    "B_",  "C",    "AA",  "p1", 12,
    "A",    "B_",  "C",    "AB",  "p1", 1,
    "A",    "B_",  "D",    "BB",  "p1", 123,
    "A",     NA,   NA,    "BB",  "p1", 12,
    "B",    "A_",  "A",    "AA",  "p1", 123,
    "B",    "A_",  "A",    "AB",  "p1", 12,
    "B",    "A_",  "B",    "AA",  "p1", 1,
    "B",    "A_",  "B",    "AB",  "p1", 123,
    "B",    "B_",  "C",    "AA",  "p1", 12,
    "B",    "B_",  "C",    "AB",  "p1", 1,
    "B",    "B_",  "D",    "BB",  "p1", 123,
    "B",     NA,   NA,    "BB",  "p1", 12,
    "B",     NA,   NA,    "CC",  "p1", 12
  )

  multi_column_template <- tfrmt(
    label = label,
    column = c(col0, col1, col2),
    param = param,
    value = value,
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("XXXX"))
    ),
    col_plan = col_plan(
      label,
      span_structure(
        col0 = c(starts_with("A_"), starts_with("B_")),
        col2 = c(renamed_A = AA, AB, renamed_BB = BB)
      ),
      renamed_BB = BB,
      renamed_CC = CC
      ),
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(location = "spanning"))
    )

  suppressMessages({
      processed_gt <- print_to_gt(tfrmt = multi_column_template, .data = multi_col_df)
  })


  expect_equal(
    processed_gt[["_boxhead"]]$column_label %>% map_chr(as.character),
    c("label", "renamed_A", "AB", "renamed_A", "AB", "renamed_A",
      "AB", "renamed_BB", "renamed_BB", "renamed_CC", "..tfrmt_row_grp_lbl"
    )
  )

  expect_equal(
    processed_gt[["_spanners"]]$spanner_label %>% map_chr(as.character),
    c("A",  "B",  "C" , "D" , "A_" ,"B_")
  )

  expect_equal(
    processed_gt[["_spanners"]]$spanner_level,
    c(1,1,1,1,2,2)
  )

  expect_equal(
    processed_gt[["_spanners"]]$vars,
    list(
      c(
        "A____tlang_delim___A___tlang_delim___renamed_A",
        "A____tlang_delim___A___tlang_delim___AB"
      ),
      c(
        "A____tlang_delim___B___tlang_delim___renamed_A",
        "A____tlang_delim___B___tlang_delim___AB"
      ),
      c(
        "B____tlang_delim___C___tlang_delim___renamed_A",
        "B____tlang_delim___C___tlang_delim___AB"
      ),
      "B____tlang_delim___D___tlang_delim___renamed_BB",
      c(
        "A____tlang_delim___A___tlang_delim___renamed_A",
        "A____tlang_delim___A___tlang_delim___AB",
        "A____tlang_delim___B___tlang_delim___renamed_A",
        "A____tlang_delim___B___tlang_delim___AB"
      ),
      c(
        "B____tlang_delim___C___tlang_delim___renamed_A",
        "B____tlang_delim___C___tlang_delim___AB",
        "B____tlang_delim___D___tlang_delim___renamed_BB"
      )
    )
  )

})

test_that("Build simple tfrmt with spans with child spans that are and are not spanned",{

  dat <- tibble::tribble(
    ~group,      ~label,     ~top_span, ~child_span,  ~my_col,    ~parm, ~val,
    "g1", "rowlabel1", "column cols",  "cols 1,2",   "col1",  "value",    1,
    "g1", "rowlabel1", "column cols",  "cols 1,2",   "col2",  "value",    1,
    "g1", "rowlabel1",     "my cols",          NA, "mycol3",  "value",    1,
    "g1", "rowlabel1", "column cols",          NA,   "col4",  "value",    1,
    "g1", "rowlabel1",     "my cols",          NA, "mycol5",  "value",    1,
    "g1", "rowlabel2", "column cols",  "cols 1,2",   "col1",  "value",    2,
    "g1", "rowlabel2", "column cols",  "cols 1,2",   "col2",  "value",    2,
    "g1", "rowlabel2",     "my cols",          NA, "mycol3",  "value",    2,
    "g1", "rowlabel2", "column cols",          NA,   "col4",  "value",    2,
    "g1", "rowlabel2",     "my cols",          NA, "mycol5",  "value",    2,
    "g2", "rowlabel3", "column cols",  "cols 1,2",   "col1",  "value",    3,
    "g2", "rowlabel3", "column cols",  "cols 1,2",   "col2",  "value",    3,
    "g2", "rowlabel3",     "my cols",          NA, "mycol3",  "value",    3,
    "g2", "rowlabel3", "column cols",          NA,   "col4",  "value",    3,
    "g2", "rowlabel3",     "my cols",          NA, "mycol5",  "value",    3
  )

  tfrmt_with_parial_child_span <- tfrmt(
    group = group,
    label = label,
    param = parm,
    value = val,
    column = c(top_span, child_span, my_col),
    col_plan = col_plan(
      group,
      label,
      span_structure(
        top_span = "column cols",
        child_span = "cols 1,2",
        my_col = c("col1", "col2")
      ),
      span_structure(
        top_span = "column cols",
        my_col = "col4"
      ),
      span_structure(
        top_span = "my cols",
        my_col = c(new_col_3 = mycol3, mycol5)
      )
    ),
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(location = "spanning"))
  )


  suppressMessages({
      processed_gt <- print_to_gt(tfrmt = tfrmt_with_parial_child_span, .data = dat)
  })

  expect_equal(
    processed_gt[["_boxhead"]]$column_label %>% map_chr(as.character),
    c("group", "label", "col1", "col2", "col4", "new_col_3", "mycol5",
      "..tfrmt_row_grp_lbl")
  )

  expect_equal(
    processed_gt[["_spanners"]]$spanner_label %>% map_chr(as.character),
    c("cols 1,2","column cols", "my cols" )
  )

  expect_equal(
    processed_gt[["_spanners"]]$spanner_level,
    c(1,2,1)
  )

  expect_equal(
    processed_gt[["_spanners"]]$vars,
    list(
      c(
        "column cols___tlang_delim___cols 1,2___tlang_delim___col1",
        "column cols___tlang_delim___cols 1,2___tlang_delim___col2"
      ),
      c(
        "column cols___tlang_delim___cols 1,2___tlang_delim___col1",
        "column cols___tlang_delim___cols 1,2___tlang_delim___col2",
        "column cols___tlang_delim___NA___tlang_delim___col4"
      ),
      c(
        "my cols___tlang_delim___NA___tlang_delim___new_col_3",
        "my cols___tlang_delim___NA___tlang_delim___mycol5"
      )
    )
  )

})

test_that("Build simple tfrmt with spans with child spans that are and are not spanned - removal",{

  dat <- tibble::tribble(
    ~group,      ~label,     ~top_span, ~child_span,  ~my_col,    ~parm, ~val,
    "g1", "rowlabel1", "column cols",  "cols 1,2",   "col1",  "value",    1,
    "g1", "rowlabel1", "column cols",  "cols 1,2",   "col2",  "value",    1,
    "g1", "rowlabel1",     "my cols",          NA, "mycol3",  "value",    1,
    "g1", "rowlabel1", "column cols",          NA,   "col4",  "value",    1,
    "g1", "rowlabel1",     "my cols",          NA, "mycol5",  "value",    1,
    "g1", "rowlabel2", "column cols",  "cols 1,2",   "col1",  "value",    2,
    "g1", "rowlabel2", "column cols",  "cols 1,2",   "col2",  "value",    2,
    "g1", "rowlabel2",     "my cols",          NA, "mycol3",  "value",    2,
    "g1", "rowlabel2", "column cols",          NA,   "col4",  "value",    2,
    "g1", "rowlabel2",     "my cols",          NA, "mycol5",  "value",    2,
    "g2", "rowlabel3", "column cols",  "cols 1,2",   "col1",  "value",    3,
    "g2", "rowlabel3", "column cols",  "cols 1,2",   "col2",  "value",    3,
    "g2", "rowlabel3",     "my cols",          NA, "mycol3",  "value",    3,
    "g2", "rowlabel3", "column cols",          NA,   "col4",  "value",    3,
    "g2", "rowlabel3",     "my cols",          NA, "mycol5",  "value",    3
  )


  tfrmt_with_parial_child_span <- tfrmt(
    group = group,
    label = label,
    param = parm,
    value = val,
    column = c(top_span, child_span, my_col),
    col_plan = col_plan(
      group,
      label,
      span_structure(
        top_span = "column cols",
        child_span = "cols 1,2",
        my_col = c("col1", "col2")
      ),
      span_structure(
        top_span = "column cols",
        my_col = "col4"
      ),
      span_structure(
        top_span = "my cols",
        my_col = c(new_col_3 = mycol3, -mycol5)
      )
    ),
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(location = "spanning"))
  )


  suppressMessages({
    processed_gt <- print_to_gt(tfrmt = tfrmt_with_parial_child_span, .data = dat)
  })

  expect_equal(
    processed_gt[["_boxhead"]]$column_label %>% map_chr(as.character),
    c("group", "label", "col1", "col2", "col4", "new_col_3",
      "..tfrmt_row_grp_lbl")
  )

  expect_equal(
    processed_gt[["_spanners"]]$spanner_label %>% map_chr(as.character),
    c("cols 1,2","column cols", "my cols" )
  )

  expect_equal(
    processed_gt[["_spanners"]]$spanner_level,
    c(1,2,1)
  )

  expect_equal(
    processed_gt[["_spanners"]]$vars,
    list(
      c(
        "column cols___tlang_delim___cols 1,2___tlang_delim___col1",
        "column cols___tlang_delim___cols 1,2___tlang_delim___col2"
      ),
      c(
        "column cols___tlang_delim___cols 1,2___tlang_delim___col1",
        "column cols___tlang_delim___cols 1,2___tlang_delim___col2",
        "column cols___tlang_delim___NA___tlang_delim___col4"
      ),
      c(
        "my cols___tlang_delim___NA___tlang_delim___new_col_3"
      )
    )
  )

})

test_that("col_plan returns correct errors",{

  expect_error(
    col_plan(
      func("invalid entry")
    ),
    paste0(
      "Invalid entry: `func(\"invalid entry\")`\n",
      "Only span_structures (`span_structure()`),",
      " selection helpers (See <https://tidyselect.r-lib.org/reference>),",
      "  or unquoted expressions representing variable names  can be entered",
      " as contents. Changing the names of individual variables using",
      " new_name = old_name syntax is allowable"
    ),
    fixed = TRUE
  )

})

test_that("span_structure misc, including errors",{

  ## unnamed values
  expect_error(
    span_structure(`blah blah blah`),
    "Entries of a span_stucture must be named:\n span_structure(`blah blah blah`)",
    fixed = TRUE
  )

  ## weird values
  expect_error(
    span_structure(
      name_col = 1i
    ),
    "Unexpected entry type in span_structure()",
    fixed = TRUE
  )

  ## other functions values
  expect_error(
    span_structure(
      name_col = vars(matrix())
    ),
    "Invalid entry: `matrix()`\nOnly selection helpers (See <https://tidyselect.r-lib.org/reference>),",
    fixed = TRUE
  )

})


test_that("Tidyselect subtraction with span_structure",{

  df <- crossing(label = c("label 1", "label 2", "label 3"),
                 column = c("trt1", "trt2", "pl", "trt1&trt2"),
                 param = c("count", "percent")) %>%
    mutate(ord1 = rep(seq(1:length(unique(.$label))), each = nrow(.)/length(unique(.$label)) )) %>%
    mutate(t_or_p = case_when(
      column %in% c("trt1", "trt2", "trt1&trt2") ~ "Treatment",
      column %in% c("pl") ~ "Placebo",
    ))

  df_fake_values <- df %>% mutate(value = runif(nrow(df)))

  tfrmt_minus_selection <-  tfrmt(
      # Specify columns in the data
      label = label,
      column = c(t_or_p, column),
      param = param,
      value = value,
      sorting_cols = c(ord1),
      # Specify body plan
      body_plan = body_plan(
        frmt_structure(group_val = ".default", label_val = ".default",
                       frmt_combine(
                         "{count} {percent}",
                         count = frmt("XXX"),
                         percent = frmt_when("==100"~ frmt(""),
                                             "==0"~ "",
                                             "TRUE" ~ frmt("(XX.X%)"))
                       ))
      ),
      # Remove extra cols and create spans
      col_plan = col_plan(
        -starts_with("ord"),
        span_structure(
          t_or_p = "Treatment",
          column = c(T1 = trt1,T2 = trt2,`T1&T2` = `trt1&trt2`)),
        span_structure(
          t_or_p = "Placebo",
          column = c(PL = pl))
      )
    )


  mock_gt <- print_mock_gt(tfrmt_minus_selection, df)

  ## keeps the spanners & original cols other than ones that start with "ord". renaming occurs as needed
  expect_equal(
    names(mock_gt$`_data`),
    c("label", "Placebo___tlang_delim___PL", "Treatment___tlang_delim___T1",
      "Treatment___tlang_delim___T1&T2", "Treatment___tlang_delim___T2",
      "..tfrmt_row_grp_lbl"
    )
  )

  real_gt <- print_to_gt(tfrmt_minus_selection, df_fake_values)

  ## keeps the spanners & original cols other than ones that start with "ord". renaming occurs as needed
  expect_equal(
    names(real_gt$`_data`),
    c("label", "Placebo___tlang_delim___PL", "Treatment___tlang_delim___T1",
      "Treatment___tlang_delim___T1&T2", "Treatment___tlang_delim___T2",
      "..tfrmt_row_grp_lbl"
    )
  )


  tfrmt_minus_selection_2 <- tfrmt_minus_selection %>%
    tfrmt(
      col_plan = col_plan(
        span_structure(
          t_or_p = "Treatment",
          column = c(T1 = trt1,T2 = trt2,`T1&T2` = `trt1&trt2`)),
        span_structure(
          t_or_p = "Placebo",
          column = c(PL = pl)),
        -starts_with("ord"),
        .drop = TRUE
      )
    )

  mock_gt2 <- tfrmt_minus_selection_2 %>%
    print_mock_gt(df)


  ## keeps only the spanners, label is dropped
  expect_equal(
    names(mock_gt2$`_data`),
    c("Treatment___tlang_delim___T1",
      "Treatment___tlang_delim___T2",
      "Treatment___tlang_delim___T1&T2",
      "Placebo___tlang_delim___PL",
      "..tfrmt_row_grp_lbl"
    )
  )

  real_gt2 <- print_to_gt(tfrmt_minus_selection_2, df_fake_values)

  ## keeps the spanners & original cols other than ones that start with "ord". renaming occurs as needed
  expect_equal(
    names(real_gt$`_data`),
    c("label", "Placebo___tlang_delim___PL", "Treatment___tlang_delim___T1",
      "Treatment___tlang_delim___T1&T2", "Treatment___tlang_delim___T2",
      "..tfrmt_row_grp_lbl"
    )
  )

})




test_that("Build simple tfrmt with stub header",{

  basic_multi_column_template <- tfrmt(
    group = group,
    label = quo(label),
    param = parm,
    value = val,
    column = c(test1,test2),
    col_plan = col_plan(
      grp = group,
      label,
      tst = col4,
      col3,
      col1,
      -col5,
      -col2
    ),
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(location = "indented"))
  )

  basic_example_dataset <- tibble::tribble(
    ~group,     ~label,    ~test1, ~test2,    ~parm, ~val,
    "g1", "rowlabel1",  "span 1", "col1",  "value",    1,
    "g1", "rowlabel1",  "span 1", "col2",  "value",    1,
    "g1", "rowlabel1",        NA, "col3",  "value",    1,
    "g1", "rowlabel1",        NA, "col4",  "value",    1,
    "g1", "rowlabel1",        NA, "col5",  "value",    1,
    "g1", "rowlabel2",  "span 1", "col1",  "value",    2,
    "g1", "rowlabel2",  "span 1", "col2",  "value",    2,
    "g1", "rowlabel2",        NA, "col3",  "value",    2,
    "g1", "rowlabel2",        NA, "col4",  "value",    2,
    "g1", "rowlabel2",        NA, "col5",  "value",    2,
    "g2", "rowlabel3",  "span 1", "col1",  "value",    3,
    "g2", "rowlabel3",  "span 1", "col2",  "value",    3,
    "g2", "rowlabel3",        NA, "col3",  "value",    3,
    "g2", "rowlabel3",        NA, "col4",  "value",    3,
    "g2", "rowlabel3",        NA, "col5",  "value",    3,
  )

  suppressMessages({
    processed_gt <- print_to_gt(tfrmt = basic_multi_column_template, .data = basic_example_dataset)
  })

  expect_equal(
    processed_gt[["_boxhead"]]$column_label %>% map_chr(as.character),
    c("label", "tst", "col3", "col1", "..tfrmt_row_grp_lbl"
    )
  )
  expect_equal(
    processed_gt[["_stubhead"]]$label,
    "grp"
  )


  # no stubhead if no group column
  basic_multi_column_template2 <- tfrmt(
    label = quo(label),
    param = parm,
    value = val,
    column = c(test1,test2),
    col_plan = col_plan(
      label,
      tst = col4,
      col3,
      col1,
      -col5,
      -col2
    )
  )

  basic_example_dataset2 <- tibble::tribble(
    ~label,    ~test1, ~test2,    ~parm, ~val,
    "rowlabel1",  "span 1", "col1",  "value",    1,
    "rowlabel1",  "span 1", "col2",  "value",    1,
    "rowlabel1",        NA, "col3",  "value",    1,
    "rowlabel1",        NA, "col4",  "value",    1,
    "rowlabel1",        NA, "col5",  "value",    1,
    "rowlabel2",  "span 1", "col1",  "value",    2,
    "rowlabel2",  "span 1", "col2",  "value",    2,
    "rowlabel2",        NA, "col3",  "value",    2,
    "rowlabel2",        NA, "col4",  "value",    2,
    "rowlabel2",        NA, "col5",  "value",    2,
    "rowlabel3",  "span 1", "col1",  "value",    3,
    "rowlabel3",  "span 1", "col2",  "value",    3,
    "rowlabel3",        NA, "col3",  "value",    3,
    "rowlabel3",        NA, "col4",  "value",    3,
    "rowlabel3",        NA, "col5",  "value",    3,
  )

  suppressMessages({
    processed_gt <- print_to_gt(tfrmt = basic_multi_column_template2, .data = basic_example_dataset2)
  })

  expect_equal(
    processed_gt[["_boxhead"]]$column_label %>% map_chr(as.character),
    c("label", "tst", "col3", "col1", "..tfrmt_row_grp_lbl"
    )
  )
  expect_equal(
    processed_gt[["_stubhead"]]$label,
    NULL
  )


})
