test_that("element_col width are respected even with row_gtp_struct",{

  dat <- tibble::tribble(
    ~row_label2,                ~row_label3, ~column, ~param, ~value,
    "G1", "Long label entry #1 no new line",  "col1",    "n",     10,
    "G1", "Long label entry #1 no new line",  "col1",  "pct",     10,
    "G1", "Long label entry #1 no new line",  "col2",    "n",     11,
    "G1", "Long label entry #1 no new line",  "col2",  "pct",     11,
    "G1", "Long label entry #1 no new line",  "col3",    "n",     12,
    "G1", "Long label entry #1 no new line",  "col3",  "pct",     12,
    "G1",                     "short label",  "col1",    "n",     13,
    "G1",                     "short label",  "col1",  "pct",     13,
    "G1",                     "short label",  "col2",    "n",     14,
    "G1",                     "short label",  "col2",  "pct",     14,
    "G1",                     "short label",  "col3",    "n",     15,
    "G1",                     "short label",  "col3",  "pct",     15,
    "G2", "Long label entry #1 \n new line",  "col1",    "n",     16,
    "G2", "Long label entry #1 \n new line",  "col1",  "pct",     16,
    "G2", "Long label entry #1 \n new line",  "col2",    "n",     17,
    "G2", "Long label entry #1 \n new line",  "col2",  "pct",     17,
    "G2", "Long label entry #1 \n new line",  "col3",    "n",     18,
    "G2", "Long label entry #1 \n new line",  "col3",  "pct",     18
  )

  pre_made_tfrmt <- tfrmt(
    # specify columns in the data
    group = row_label2,
    label = row_label3,
    column = column,
    param = param,
    values = value,
    # specify value formatting
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default",
                     frmt_combine("{n} {pct}",
                                  n = frmt("xxx"),
                                  pct = frmt_when("==100" ~ "",
                                                  "==0" ~ "",
                                                  TRUE ~ frmt("(xx.x %)"))))
    ),
    # # Specify column alignment plan
    col_style_plan = col_style_plan(
      element_col(align = c(".",","," "), col = c("col1", "col2", "col3")),
      element_col(width = 200, col = c(row_label3))
    ),
    # # Specify row group plan
    row_grp_plan = row_grp_plan(
      # When this line is commented out width works when it isn't width breaks
      row_grp_structure(group_val = ".default", element_block = element_block(post_space = "")),
      label_loc = element_row_grp_loc(location = "indent")
    )
  )


  dat_gt <- print_to_gt(pre_made_tfrmt, dat %>% filter(row_label3 != "Long label entry #1 no new line"))

  expect_snapshot(
    dat_gt
  )

})
