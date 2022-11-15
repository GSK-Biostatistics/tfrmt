test_that("Testing error messages for missing argument col_style_structure",{

  expect_error(
    tfrmt(
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
      col_style_structure(align = c(".",","," "))
    ),

    # Specify row group plan
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "column")
    )
    ),
    "Column element is missing from col_style_structure[.] Note: col here refers to the values within the column variable in your data, rather than the variable name itself")
})
