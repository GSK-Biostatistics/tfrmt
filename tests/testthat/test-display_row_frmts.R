test_that("Display row formats for tfrmt with one type of frmt", {

  df <- crossing(
    label = c("label 1", "label 2", "label 3"),
    column = c("PL", "T1", "T2"),
    param = c("count", "percent")) %>%
    dplyr::arrange_all() %>%
    mutate(values = seq(1:nrow(.)))

  my_tfrmt <- tfrmt(
    label = label,
    column = column,
    param = param,
    values = values,
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default",
                        element_block(post_space = "   ")) ),

    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default",
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

  # expect dataframe and tfrmt input
  expect_s3_class(my_tfrmt, "tfrmt")
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))

  # expect dataframe output
  expect_equal(class(row_frmts_table_true), c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(row_frmts_table_false), c("tbl_df", "tbl", "data.frame"))

  # expect error when convert_to_txt not true/false
  expect_error(display_row_frmts(tfrmt = my_tfrmt, .data = df, convert_to_txt = "hello"),
               "Please pass a boolean value into the `convert_to_txt` parameter")

  })

