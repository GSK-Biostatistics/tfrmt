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

test_that("print_to_gt() works", {
  test_data <- tibble::tribble(
    ~group  , ~label  , ~span1   , ~span2  , ~lower     , ~param , ~val ,
    "mygrp" , "mylbl" , "span01" , "span1" , "lower1_a" , "prm"  ,    1 ,
    "mygrp" , "mylbl" , "span01" , "span1" , "lower1_b" , "prm"  ,    1 ,
    "mygrp" , "mylbl" , "span01" , "span2" , "lower2_a" , "prm"  ,    1 ,
    "mygrp" , "mylbl" , "span01" , "span2" , "lower2_b" , "prm"  ,    1 ,
    "mygrp" , "mylbl" , "span02" , "span3" , "lower2_a" , "prm"  ,    1 ,
    "mygrp" , "mylbl" , "span02" , "span3" , "lower2_b" , "prm"  ,    1
  )

  tfrmt_plan <- tfrmt(
    group = "group",
    label = "label",
    param = "param",
    column = c("span1", "span2", "lower"),
    value = "val",
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt("x.xx")
      )
    )
  )

  expect_no_error(
    print_to_gt(
      tfrmt_plan,
      .data = test_data
    )
  )

  expect_snapshot(
    print_to_gt(
      tfrmt_plan,
      .data = test_data
    )[["_data"]]
  )
})

test_that("print_to_gt() complains with incorrect inputs", {
  # complains when the first argument is not `tfrmt`
  expect_snapshot(
    error = TRUE,
    print_to_gt(mtcars)
  )

  # complains when the `.data` argument is not a data.frame
  tfrmt_plan <- tfrmt(
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
    print_to_gt(tfrmt_plan, "foo")
  )
})

test_that("print_mock_gt() messages when tfrmt$param is missing", {
  # no message when tfrmt$param is not empty
  tfrmt_plan <- tfrmt(
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
      tfrmt_plan
    )
  )

  # message when tfrmt$param is empty
  tfrmt_plan_no_param <- tfrmt(
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
      tfrmt_plan_no_param
    ),
    "`tfrmt` will need a `param` value to `print_to_gt` when data is available",
    fixed = TRUE
  )
  expect_snapshot(
    print_mock_gt(
      tfrmt_plan_no_param
    )[["_data"]]
  )
})

test_that("print_mock_gt() messages when tfrmt$column is missing", {
  # no message when tfrmt$column is not missing
  tfrmt_plan <- tfrmt(
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
      tfrmt_plan
    )
  )

  # message when tfrmt$column is missing
  tfrmt_plan_no_column <- tfrmt(
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
      tfrmt_plan_no_column
    ),
    "`tfrmt` will need `column` value(s) to `print_to_gt` when data is available",
    fixed = TRUE
  )

  expect_snapshot(
    print_mock_gt(
      tfrmt_plan_no_column
    )[["_data"]]
  )
})

test_that("print_mock_gt() messages when tfrmt$value is missing", {
  # no message when tfrmt$value is not missing
  tfrmt_plan <- tfrmt(
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
      tfrmt_plan
    )
  )

  # message when tfrmt$value is missing
  tfrmt_plan_no_value <- tfrmt(
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
      tfrmt_plan_no_value
    ),
    "`tfrmt` will need `value` value to `print_to_gt` when data is available",
    fixed = TRUE
  )

  expect_snapshot(
    print_mock_gt(
      tfrmt_plan_no_value
    )[["_data"]]
  )
})

test_that("print_mock_gt() with missing body_plan", {
  # when tfrmt$body_plan is missing, one is made up on the fly
  tfrmt_plan_no_body_plan <- tfrmt(
    label = label,
    column = column,
    param = param,
    value = value
  )

  expect_no_message(
    print_mock_gt(
      tfrmt_plan_no_body_plan
    )
  )

  tfrmt_plan_default_body_plan <- tfrmt(
    label = label,
    column = column,
    param = param,
    value = value,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt("X.X")
      )
    )
  )

  expect_snapshot(
    print_mock_gt(
      tfrmt_plan_no_body_plan
    )[["_data"]]
  )

  expect_identical(
    print_mock_gt(
      tfrmt_plan_no_body_plan
    )[["_data"]],
    print_mock_gt(
      tfrmt_plan_default_body_plan
    )[["_data"]],
    # .col_plan_vars is a list of quosures and their environments would not match
    ignore_attr = ".col_plan_vars"
  )
})

test_that("print_mock_data() removes `value` when it exists in the input data", {
  data <- tidyr::crossing(
    label = c(
      "Intent-To-Treat (ITT)",
      "Safety",
      "Efficacy",
      "Complete Week 24",
      "Complete Study"
    ),
    column = c(
      "Placebo\n(N=XX)",
      "Xanomeline\nLow Dose\n(N=XX)",
      "Xanomeline\nHigh Dose\n(N=XX)",
      "Total\n(N=XXX)"
    ),
    param = c(
      "n",
      "percent"
    )
  ) |>
    dplyr::mutate(
      value_to_remove = 1
    )

  plan <- tfrmt(
    label = "label",
    column = "column",
    param = "param",
    value = "value_to_remove",
    title = "Summary of Populations",
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{n} ({percent}%)",
          n = frmt("xx"),
          percent = frmt("xxx")
        )
      )
    )
  )

  expect_message(
    print_mock_gt(
      plan,
      data
    ),
    "Removing `value_to_remove` from input data for mocking.",
    fixed = TRUE
  )

  expect_snapshot(
    print_mock_gt(
      plan,
      data
    )["_data"]
  )
})

test_that("cleaned_data_to_gt() works", {
  tfrmt <- tfrmt(
    # specify columns in the data
    group = group,
    label = label,
    column = time,
    param = param,
    value = value,
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(
        "gtdefault"
      )
    )
  )

  test_data <- tibble::tibble(
    label = c("Obs", "Lev", "Lev+5FU"),
    group = c("A", "A", "A"),
    `0` = c("630", "620", "608"),
    `1000` = c("372", "360", "425"),
    `2000` = c("256", "266", "328"),
    `3000` = c("11", "8", "14")
  )

  # when the input is a data.frame the output is a `gt_tbl`
  expect_s3_class(
    cleaned_data_to_gt(
      test_data,
      tfrmt,
      .unicode_ws = TRUE
    ),
    "gt_tbl"
  )

  expect_snapshot(
    cleaned_data_to_gt(
      test_data,
      tfrmt,
      .unicode_ws = TRUE
    )[["_data"]]
  )
})

test_that("cleaned_data_to_gt.list() works", {
  tfrmt <- tfrmt(
    # specify columns in the data
    group = group,
    label = label,
    column = time,
    param = param,
    value = value,
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(
        "gtdefault"
      )
    )
  )

  test_data <- tibble::tibble(
    label = c("Obs", "Lev", "Lev+5FU"),
    group = c("A", "A", "A"),
    `0` = c("630", "620", "608"),
    `1000` = c("372", "360", "425"),
    `2000` = c("256", "266", "328"),
    `3000` = c("11", "8", "14")
  )

  test_data2 <- tibble::tibble(
    label = c("Obs2", "Lev2", "Lev+5FU"),
    group = c("B", "B", "B"),
    `0` = c("631", "621", "609"),
    `1000` = c("373", "361", "426"),
    `2000` = c("257", "267", "329"),
    `3000` = c("12", "9", "15")
  )

  # when the input is a list the output is a `gt_group`
  expect_s3_class(
    cleaned_tables <- cleaned_data_to_gt(
      list(
        test_data,
        test_data2
      ),
      tfrmt,
      .unicode_ws = TRUE
    ),
    "gt_group"
  )

  expect_length(
    cleaned_tables$gt_tbls$gt_tbl,
    2
  )

  expect_snapshot(
    cleaned_tables$gt_tbls$gt_tbl[[1]][["_data"]]
  )

  expect_snapshot(
    cleaned_tables$gt_tbls$gt_tbl[[2]][["_data"]]
  )
})

test_that("cleaned_data_to_gt() with page_plan & note location in subtitle", {
  # Create the original data frame
  original_data <- tibble::tibble(
    Group = rep(c("Age (y)", "Sex"), c(3, 3)),
    Label = rep("n", 6),
    Column = rep(c("Placebo", "Treatment", "Total"), times = 2),
    Param = rep("n", 6),
    Value = c(12, 14, 31, 20, 32, 18)
  ) |>
    dplyr::mutate(
      ord1 = dplyr::if_else(Group == "Age (y)", 1, 2)
    )

  # Duplicate the data and add the `by group` column
  data_101 <- original_data |>
    dplyr::mutate(
      `by group` = "101"
    )
  data_102 <- original_data |>
    dplyr::mutate(
      `by group` = "102"
    )

  # Combine the two data frames
  data <- dplyr::bind_rows(data_101, data_102)

  # Create mock big Ns
  big_ns <- data |>
    dplyr::group_by(
      `by group`,
      Column
    ) |>
    dplyr::summarise(
      Value = sum(Value)
    ) |>
    dplyr::mutate(
      Param = "bigN"
    ) |>
    dplyr::filter(
      `by group` == "101"
    ) |>
    dplyr::ungroup()

  data <- data |>
    dplyr::bind_rows(
      big_ns
    ) |>
    dplyr::arrange(
      dplyr::desc(
        Group
      )
    )

  # Define the tfrmt object with two grouping variables and a page_plan
  # note location in subtitle
  tfrmt_two_groups <- tfrmt(
    group = c(`by group`, Group),
    label = Label,
    column = Column,
    value = Value,
    param = Param,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = "n",
        frmt("xx")
      )
    ),
    row_grp_plan = row_grp_plan(
      row_grp_structure(
        group_val = ".default",
        element_block(
          post_space = " "
        )
      )
    ),
    page_plan = page_plan(
      page_structure(
        group_val = list(
          `by group` = ".default"
        )
      ),
      note_loc = "subtitle"
    ),
    big_n = big_n_structure(
      param_val = "bigN"
    ),
    col_plan = col_plan(
      -`by group`
    )
  )

  expect_no_error(
    gt_tables <- data |>
      apply_tfrmt(
        tfrmt_two_groups,
        mock = FALSE
      ) |>
      cleaned_data_to_gt(
        tfrmt_two_groups,
        .unicode_ws = TRUE
      )
  )

  # confirm note is located in subtitle
  expect_identical(
    gt_tables$gt_tbls$gt_tbl[[1]]$`_heading`$subtitle,
    "by group: 101"
  )

  expect_identical(
    gt_tables$gt_tbls$gt_tbl[[2]]$`_heading`$subtitle,
    "by group: 102"
  )

  # change note location to preheader
  tfrmt_two_groups$page_plan$note_loc <- "preheader"

  expect_no_error(
    gt_tables <- data |>
      apply_tfrmt(
        tfrmt_two_groups,
        mock = FALSE
      ) |>
      cleaned_data_to_gt(
        tfrmt_two_groups,
        .unicode_ws = TRUE
      )
  )

  # confirm note is located in preheader
  expect_identical(
    gt_tables$gt_tbls$gt_tbl[[1]]$`_heading`$preheader,
    "by group: 101"
  )

  expect_identical(
    gt_tables$gt_tbls$gt_tbl[[2]]$`_heading`$preheader,
    "by group: 102"
  )

  # change note location to source_note
  tfrmt_two_groups$page_plan$note_loc <- "source_note"

  expect_no_error(
    gt_tables <- data |>
      apply_tfrmt(
        tfrmt_two_groups,
        mock = FALSE
      ) |>
      cleaned_data_to_gt(
        tfrmt_two_groups,
        .unicode_ws = TRUE
      )
  )

  # confirm note is located in source_note
  expect_identical(
    gt_tables$gt_tbls$gt_tbl[[1]]$`_source_notes`[[1]],
    "by group: 101"
  )

  expect_identical(
    gt_tables$gt_tbls$gt_tbl[[2]]$`_source_notes`[[1]],
    "by group: 102"
  )
})

test_that("cleaned_data_to_gt() with col_style_plan", {
  test_data <- tibble::tribble(
    ~g1   , ~g2  , ~one       , ~param   , ~column , ~value  ,
    "G1"  , "g3" , "n (%)"    , "n"      , "trt1"  , 12      ,
    "G1"  , "g3" , "n (%)"    , "pct"    , "trt1"  , 34      ,
    "G2_" , "g3" , "mean"     , "mean"   , "trt1"  , 12.3    ,
    "G2_" , "g3" , "sd"       , "sd"     , "trt1"  ,  4.34   ,
    "G2_" , "g3" , "median"   , "median" , "trt1"  , 14      ,
    "G3"  , "g3" , "(q1, q3)" , "q1"     , "trt1"  , 10      ,
    "G3"  , "g3" , "(q1, q3)" , "q3"     , "trt1"  , 20      ,
    "G1"  , "g3" , "n (%)"    , "n"      , "trt2"  , 24      ,
    "G1"  , "g3" , "n (%)"    , "pct"    , "trt2"  , 58      ,
    "G2_" , "g3" , "mean"     , "mean"   , "trt2"  , 15.4    ,
    "G2_" , "g3" , "sd"       , "sd"     , "trt2"  ,  8.25   ,
    "G2_" , "g3" , "median"   , "median" , "trt2"  , 16      ,
    "G3"  , "g3" , "(q1, q3)" , "q1"     , "trt2"  , 22      ,
    "G3"  , "g3" , "(q1, q3)" , "q3"     , "trt2"  , 22      ,
    "G1"  , "g3" , "mean"     , "pval"   , "four"  ,  0.0001
  )

  plan <- tfrmt(
    label = one,
    group = c(g1, g2),
    column = vars(column),
    value = value,
    param = param,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt("xx.xx")
      ),
      frmt_structure(
        group_val = ".default",
        label_val = "n (%)",
        frmt_combine(
          "{n} ({pct}%)",
          n = frmt("x"),
          pct = frmt("xx.x")
        )
      ),
      frmt_structure(
        group_val = ".default",
        label_val = "(q1, q3)",
        frmt_combine(
          "({q1}, {q3})",
          q1 = frmt("xx"),
          q3 = frmt("xx")
        )
      ),
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        pval = frmt_when(
          "<.001" ~ "<.001",
          TRUE ~ frmt("x.xxx")
        )
      )
    ),
    row_grp_plan = row_grp_plan(
      row_grp_structure(
        group_val = list(
          g1 = c("G1", "G2_"),
          g2 = ".default"
        ),
        element_block = element_block(
          post_space = "----"
        )
      ),
      label_loc = element_row_grp_loc(
        location = "spanning"
      )
    ),
    col_style_plan = col_style_plan(
      col_style_structure(
        align = "right",
        col = g1
      ),
      col_style_structure(
        align = "right",
        col = one
      ),
      col_style_structure(
        align = "right",
        width = 4,
        col = vars(
          starts_with(
            "trt"
          )
        )
      ),
      col_style_structure(
        align = "left",
        col = trt1
      ),
      col_style_structure(
        width = 10,
        col = four
      )
    )
  )

  # apply_col_style_plan() handles the alignment -> we snapshot the data and
  # not a gt property
  expect_snapshot(
    test_data |>
      apply_tfrmt(
        plan,
        mock = FALSE
      ) |>
      cleaned_data_to_gt(
        plan,
        .unicode_ws = TRUE
      ) |>
      purrr::pluck(
        "_data"
      )
  )
})

test_that("cleaned_data_to_gt() with row_grp_plan and location = 'column'", {
  test_data <- tibble::tribble(
    ~grp1 , ~grp2 , ~lbl , ~prm , ~column , ~val , ~ord ,
    "d"   , "c"   , "n"  , "n"  ,       1 ,    1 ,    1 ,
    "a"   , "b"   , "m"  , "n"  ,       1 ,    2 ,    2 ,
    "q"   , "v"   , "s"  , "n"  ,       1 ,    3 ,    3 ,
    "b"   , "p"   , "e"  , "n"  ,       1 ,    4 ,    4
  )

  tfrmt_plan <- tfrmt(
    group = c(grp1, grp2),
    label = lbl,
    column = column,
    value = val,
    param = prm,
    sorting_cols = ord,
    col_plan = col_plan(-ord),
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt("x")
      )
    ),
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(
        location = "column"
      )
    )
  )

  expect_snapshot(
    test_data |>
      apply_tfrmt(
        tfrmt_plan,
        mock = FALSE
      ) |>
      cleaned_data_to_gt(
        tfrmt_plan,
        .unicode_ws = TRUE
      ) |>
      purrr::pluck(
        "_data"
      )
  )
})
