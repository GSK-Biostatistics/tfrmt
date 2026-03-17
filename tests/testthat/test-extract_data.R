test_that("extract_data works for a single gt_tbl object", {

data_demog_test <- data_demog |>
  filter(rowlbl1 %in% c("Age (y)" , "Sex"),
         column != "p-value")

tfrmt_dem <- tfrmt(
  # specify columns in the data
  group = c(rowlbl1, grp),
  label = rowlbl2,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                n = frmt("xxx"),
                                                                                pct = frmt_when(
                                                                                  "==100" ~ "",
                                                                                  "==0" ~ "",
                                                                                  TRUE ~ frmt("(xx.x %)")
                                                                                )
    )),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
    frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min", "Max"), frmt("xxx.x")),
    frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx"))
  ),
  # remove extra cols
  col_plan = col_plan(
    -grp,
    -starts_with("ord")
  )
  )|>
  print_to_gt(data_demog_test)

tfrmt_data_extracted <- extract_data(tfrmt_dem)
tfrmt_data_manual <- tfrmt_dem[["_data"]]

expect_s3_class(tfrmt_data_extracted, "data.frame")
expect_equal(tfrmt_data_extracted, tfrmt_data_manual)
expect_true(nrow(tfrmt_data_extracted) > 0)
})



test_that("extract_data works for a gt_group object (paged tables)", {

data_demog_test <- data_demog |>
    filter(rowlbl1 %in% c("Age (y)" , "Sex"),
           column != "p-value")

tfrmt_paged <- tfrmt(
  # specify columns in the data
  group = c(rowlbl1, grp),
  label = rowlbl2,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      frmt_combine(
        "{n} {pct}",
        n = frmt("xxx"),
        pct = frmt_when(
          "==100" ~ "",
          "==0" ~ "",
          TRUE ~ frmt("(xx.x %)")
        )
      )
    ),
    frmt_structure(
      group_val = ".default",
      label_val = "n",
      frmt("xxx")
    ),
    frmt_structure(
      group_val = ".default",
      label_val = c("Mean", "Median", "Min", "Max"),
      frmt("xxx.x")
    ),
    frmt_structure(
      group_val = ".default",
      label_val = "SD",
      frmt("xxx.xx")
    )
  ),
  # remove extra cols
  col_plan = col_plan(
    -grp,
    -starts_with("ord")
  ),
  page_plan = page_plan(
    page_structure(
      group_val = list(
        rowlbl1 = ".default"
      )
    ),
    note_loc = "source_note"
  )
) |>
  print_to_gt(data_demog_test)

#  Extract using function vs manual (using purrr::map)
gt_group_data_extracted <- extract_data(tfrmt_paged)
gt_group_manual <- purrr::map(tfrmt_paged$gt_tbls$gt_tbl, ~ .x[["_data"]])

#  Assertions
expect_type(gt_group_data_extracted, "list")
expect_length(gt_group_data_extracted, length(gt_group_manual))
expect_equal(gt_group_data_extracted, gt_group_manual)

# Check that each element in the list is a data frame
expect_s3_class(gt_group_data_extracted[[1]], "data.frame")
})

test_that("extract_data throws an error for invalid inputs", {
  expect_error(extract_data(iris), "Input must be a 'gt_tbl' or 'gt_group' object")
  expect_error(extract_data(NULL), "Input must be a 'gt_tbl' or 'gt_group' object")
})

test_that("extract_data works for a table with bigN values", {
data <- tibble::tibble(
    Group = c("N", "N", "N", rep(c("Age (y)", "Sex", "Age (y)", "Sex"), c(3, 3, 6, 12))),
    Label = c("N", "N", "N", rep(c("n", "Mean (SD)", "Male", "Female"), c(6, 6, 6, 6))),
    Column = c("Placebo", "Treatment", "Total", rep(c("Placebo", "Treatment", "Total"), times = 8)),
    Param = c("bigN", "bigN", "bigN", rep(c("n", "mean", "sd", "n", "pct", "n", "pct"), c(6, 3, 3, 3, 3, 3, 3))),
    Value = c(30, 40, 60, 15, 13, 28, 14, 13, 27, 73.56, 74.231, 71.84, 9.347, 7.234, 8.293,
              8, 7, 15, 8 / 14, 7 / 13, 15 / 27, 6, 6, 12, 6 / 14, 6 / 13, 12 / 27)
  ) |>
    dplyr::mutate(
      Value = dplyr::case_when(
        Param == "pct" ~ Value * 100,
        TRUE ~ Value
      ),
      ord1 = dplyr::if_else(Param == "bigN", 0, 1),
      ord2 = dplyr::if_else(Param == "bigN", 0, 1)
    )


bign <- tfrmt(
    group = Group,
    label = Label,
    column = Column,
    value = Value,
    param = Param,
    sorting_cols = c(ord1, ord2),
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine("{n} {pct}",
                     n = frmt("X"),
                     pct = frmt("(xx.x%)", missing = " ")
        )
      ),
      frmt_structure(
        group_val = "Age (y)", label_val = "Mean (SD)",
        frmt_combine("{mean} ({sd})",
                     mean = frmt("XX.X"),
                     sd = frmt("x.xx")
        )
      ),
      frmt_structure(group_val = ".default", label_val = "n", frmt("xx"))
    ),
    col_plan = col_plan(everything(), -starts_with("ord"), "Total"),
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " "))
    ),
    big_n = big_n_structure(param_val = "bigN", n_frmt = frmt("\nN = xx"))
  ) |>
    print_to_gt(data)


extracted<-extract_data(bign)
manual <- bign[["_data"]]

#check expected data is equal to manual extraction
expect_s3_class(extracted, "data.frame")
expect_equal(extracted, manual)
expect_true(nrow(extracted) > 0)

#check that the bigns are present in the column names of extracted data
column_labels <- colnames(extracted)

# Define big N values
expected_patterns <- c("N = 30", "N = 40", "N = 60")

# Assert that the labels contain your expected strings
expect_true(any(grepl("N = 30", column_labels)))
expect_true(any(grepl("N = 40", column_labels)))
expect_true(any(grepl("N = 60", column_labels)))
})

test_that("extract_data handles various spanning header depths", {
  data <- tibble::tribble(
    ~group, ~label, ~span2, ~span1, ~my_col, ~parm, ~val,
    "g1", "rowlabel1", "column cols", "cols 1,2", "col1", "value", 1,
    "g1", "rowlabel1", "column cols", "cols 1,2", "col2", "value", 1,
    "g1", "rowlabel1", NA, NA, "mycol3", "value", 1,
    "g1", "rowlabel1", "column cols", "col 4", "col4", "value", 1,
    "g1", "rowlabel1", NA, NA, "mycol5", "value", 1,
    "g1", "rowlabel2", "column cols", "cols 1,2", "col1", "value", 2,
    "g1", "rowlabel2", "column cols", "cols 1,2", "col2", "value", 2,
    "g1", "rowlabel2", NA, NA, "mycol3", "value", 2,
    "g1", "rowlabel2", "column cols", "col 4", "col4", "value", 2,
    "g1", "rowlabel2", NA, NA, "mycol5", "value", 2,
    "g2", "rowlabel3", "column cols", "cols 1,2", "col1", "value", 3,
    "g2", "rowlabel3", "column cols", "cols 1,2", "col2", "value", 3,
    "g2", "rowlabel3", NA, NA, "mycol3", "value", 3,
    "g2", "rowlabel3", "column cols", "col 4", "col4", "value", 3,
    "g2", "rowlabel3", NA, NA, "mycol5", "value", 3,
  )

  # 2 layers of spanning headers
  spanning_tfrmt <- tfrmt(
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
      starts_with("col")
    )
  )|> print_to_gt(data)


  res_layer <- extract_data(spanning_tfrmt, col_delim="_")

  expect_contains(colnames(res_layer), "column cols_______cols 1,2_______col1")
  expect_contains(colnames(res_layer), "column cols_______col 4_______col4")

  # 1 layer of spanning headers
  data2 <- data |>
       select(-span2)

  spanning_tfrmt2 <- tfrmt(
    group = group,
    label = label,
    param = parm,
    value = val,
    column = c(span1, my_col),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("x"))
    ),
    col_plan = col_plan(
      group,
      label,
      starts_with("col")
    )
  )|> print_to_gt(data2)

  res_layer2 <- extract_data(spanning_tfrmt2, col_delim="_")

  expect_contains(colnames(res_layer2), "cols 1,2_______col1")
  expect_contains(colnames(res_layer2), "col 4_______col4")

  # 1 layer with page plan

  spanning_tfrmt_page <- tfrmt(
    group = group,
    label = label,
    param = parm,
    value = val,
    column = c(span1, my_col),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("x"))
    ),
    col_plan = col_plan(
      group,
      label,
      starts_with("col")
    ),
    page_plan = page_plan(
      max_rows = 3
    )
  )|> print_to_gt(data2)

  res_layer_page <- extract_data(spanning_tfrmt_page, col_delim="_")

  expect_contains(colnames(res_layer_page[[1]]), "cols 1,2_______col1")
  expect_contains(colnames(res_layer_page[[1]]), "col 4_______col4")

})
