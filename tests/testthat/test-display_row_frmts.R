test_that("Display row formats for tfrmt with <frmt>", {

  df <- crossing(
    label = c("label 1", "label 2", "label 3"),
    column = c("PL", "T1", "T2"),
    param = c("count")) %>%
    dplyr::arrange_all() %>%
    mutate(value = seq(1:nrow(.)))

  my_tfrmt <- tfrmt(
    label = label,
    column = column,
    param = param,
    value = value,
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default",
                        element_block(post_space = "   ")) ),

    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default",
                     frmt("xx.x"))
    ))

  row_frmts_table_true <- display_row_frmts(tfrmt = my_tfrmt,
                                            .data = df,
                                            convert_to_txt = TRUE)
  row_frmts_table_false <- display_row_frmts(tfrmt = my_tfrmt,
                                             .data = df,
                                             convert_to_txt = FALSE)

  # expect error when convert_to_txt not Boolean
  expect_error(display_row_frmts(tfrmt = my_tfrmt, .data = df, convert_to_txt = "hello"),
               "Please pass a boolean value into the `convert_to_txt` parameter")

  # expect dataframe and tfrmt input
  expect_s3_class(my_tfrmt, "tfrmt")
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))

  # expect dataframe output
  expect_equal(class(row_frmts_table_true), c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(row_frmts_table_false), c("tbl_df", "tbl", "data.frame"))

})

test_that("Display row formats for tfrmt with <frmt> <frmt_combine>", {

  df <- bind_rows(
    crossing(
      label = c("label 1"),
      column = c("PL", "T1", "T2"),
      param = c("count")),
    crossing(
      label = c("label 2", "label 3"),
      column = c("PL", "T1", "T2"),
      param = c("count", "percent"))
  ) %>%
    dplyr::arrange_all() %>%
    mutate(value = seq(1:nrow(.)))

  my_tfrmt <- tfrmt(
    label = label,
    column = column,
    param = param,
    value = value,
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default",
                        element_block(post_space = "   ")) ),

    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = "label 1",
                     frmt("xxx")),
      frmt_structure(group_val = ".default", label_val = c("label 2", "label 3"),
                     frmt_combine("{count} ({percent})",
                                  count = frmt("xx"),
                                  percent = frmt("xx.x")))
    ))

  row_frmts_table_true <- display_row_frmts(tfrmt = my_tfrmt,
                                            .data = df,
                                            convert_to_txt = TRUE)
  row_frmts_table_false <- display_row_frmts(tfrmt = my_tfrmt,
                                             .data = df,
                                             convert_to_txt = FALSE)

  # expect error when convert_to_txt not Boolean
  expect_error(display_row_frmts(tfrmt = my_tfrmt, .data = df, convert_to_txt = "hello"),
               "Please pass a boolean value into the `convert_to_txt` parameter")

  # expect dataframe and tfrmt input
  expect_s3_class(my_tfrmt, "tfrmt")
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))

  # expect dataframe output
  expect_equal(class(row_frmts_table_true), c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(row_frmts_table_false), c("tbl_df", "tbl", "data.frame"))

})

test_that("Display row formats for tfrmt with <frmt> <frmt_combine> <frmt_when>", {

  df <- bind_rows(
    crossing(
      label = c("label 1"),
      column = c("PL", "T1", "T2"),
      param = c("n")),
    crossing(
      label = c("label 2"),
      column = c("PL", "T1", "T2"),
      param = c("median", "sd")),
    crossing(
      label = c("label 3"),
      column = c("PL", "T1", "T2"),
      param = c("pval"))
  ) %>%
    dplyr::arrange_all() %>%
    mutate(value = seq(1:nrow(.)))

  my_tfrmt <- tfrmt(
    label = label,
    column = column,
    param = param,
    value = value,

    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = "label 1",
                     frmt("xx")
      ),
      frmt_structure(group_val = ".default", label_val = "label 2",
                     frmt_combine("{median} ({sd})",
                                  median = frmt("xx"),
                                  sd = frmt("xx.x"))
      ),
      frmt_structure(group_val = ".default", label_val = "label 3",
                     frmt_when(">0.99" ~ ">0.99",
                               "<0.001" ~ "<0.001",
                               TRUE ~ frmt("x.xxx", missing = ""))
      )
    ))

  row_frmts_table_true <- display_row_frmts(tfrmt = my_tfrmt,
                                            .data = df,
                                            convert_to_txt = TRUE)
  row_frmts_table_false <- display_row_frmts(tfrmt = my_tfrmt,
                                             .data = df,
                                             convert_to_txt = FALSE)

  # expect error when convert_to_txt not Boolean
  expect_error(display_row_frmts(tfrmt = my_tfrmt, .data = df, convert_to_txt = "hello"),
               "Please pass a boolean value into the `convert_to_txt` parameter")

  # expect dataframe and tfrmt input
  expect_s3_class(my_tfrmt, "tfrmt")
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))

  # expect dataframe output
  expect_equal(class(row_frmts_table_true), c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(row_frmts_table_false), c("tbl_df", "tbl", "data.frame"))

})
