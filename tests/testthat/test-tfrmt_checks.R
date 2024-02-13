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


test_that("Testing error message for invalid input to plan parameters, specifically for col_style_plan",{
  expect_error(
    # create tfrmt object
    tfrmt_object <- tfrmt(
      label = label,
      column = column,
      param = param,
      value = value,
      sorting_cols = c(ord),
      col_plan = col_plan(-ord),
      body_plan = body_plan(
        frmt_structure(group_val = ".default", label_val = ".default",
                       frmt("xx", missing = " ")),
        frmt_structure(group_val = ".default", label_val = ".default",
                       frmt_combine("{mean} ({sd})",
                                    mean = frmt("xx.x"),
                                    sd = frmt("xx.xx"),
                                    missing=" ")),
        frmt_structure(group_val = ".default", label_val = ".default",
                       pval = frmt_when(">0.99" ~ ">0.99",
                                        "<0.001" ~ "<0.001",
                                        "<0.05" ~ frmt("x.xxx*"),
                                        TRUE ~ frmt("x.xxx", missing = "")))
      ),
      # col_style_structure() supplied to col_style_plan param instead of col_style_plan() funciton
      col_style_plan = col_style_structure(col = `p-value`, align = c("."), type = "char")
    ),
    "Invalid input supplied to the `col_style_plan` parameter. Please supply a `col_style_plan()`.",
    fixed=TRUE
  )
})


test_that("Testing error message for invalid input to big_n parameter",{
  expect_error(
    # create tfrmt object
    tfrmt_object <- tfrmt(
      group = Group,
      label = Label,
      column = Column,
      value = Value,
      param = Param,
      sorting_cols = c(ord1, ord2),
      body_plan = body_plan(
        frmt_structure(group_val = ".default",
                       label_val = ".default",
                       frmt_combine("{n} {pct}",
                                    n = frmt("X"),
                                    pct = frmt("(xx.x%)", missing = " ")
                       )
        ),
        frmt_structure(group_val = "Age (y)", label_val = "Mean (SD)",
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
      # col_plan() supplied to big_n parameter instead of big_n_structure() function
      big_n = col_plan()
    ),
    "Invalid input supplied to the `big_n` parameter. Please supply a `big_n_structure()`.",
    fixed=TRUE
  )
})

