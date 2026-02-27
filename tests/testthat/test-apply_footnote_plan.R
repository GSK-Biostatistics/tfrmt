test_that("applying footnote plan",{

  # set up data
  es_data <-tibble::tibble(rowlbl1 =c(rep("Completion Status",12),rep("Primary reason for withdrawal",28)),
                           rowlbl2 =c(rep("Completed",4),rep("Adverse Event",4),rep("Unknown",4),rep("Adverse Event",4),rep("Lost to follow-up",4),rep("Protocol violation",4),rep("Subject decided to withdraw",4),rep("Protocol Violation",4),rep("Pre-Operative Dose[1]",4),rep("Other",4)),
                           param=c(rep(c("n","n","pct","pct"),10)),
                           trt=c(rep(c("Placebo","Treatment"),20)),
                           value=c(24,19,2400/48,1900/38,5,1,500/48,100/38,19,18,1900/48,1800/38,1,1,100/48,100/38,0,0,0,0,0,0,0,0,1,1,100/48,100/38,1,4,100/48,400/38,1,0,100/48,0,2,3,200/48,300/38)

  )


  gt_start <-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = trt,
    param = param,
    value = value,
    # set formatting for values
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),

    # Specify row group plan
    # Indent the rowlbl2
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "spanning")),
  ) %>%
    print_to_gt(es_data)


  # no footnote plan ###########################################################

  tfrmt1<- tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = trt,
    param = param,
    value = value,
    # set formatting for values
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),

    # Specify row group plan
    # Indent the rowlbl2
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "column")),
  )

  gt1<- apply_footnote_plan(gt_start,tfrmt1,list())

  expect_equal(nrow(gt1$`_footnotes`),0)

  # source footnote #################################################################

  tfrmt2<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = trt,
    param = param,
    value = value,
    # set formatting for values
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),

    # Specify row group plan
    # Indent the rowlbl2
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "column")),
    footnote_plan = footnote_plan(
      footnote_structure("Source Note"),
      marks="letters"

    )
  )

  gt2<- apply_footnote_plan(gt_start,tfrmt2,list(list(col=NULL,spanning=FALSE,note="Source Note")))

  expect_equal(gt2$`_footnotes`$footnotes,list("Source Note"))

  # column footnote ##################################################################

  tfrmt3<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = trt,
    param = param,
    value = value,
    # set formatting for values
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),

    # Specify row group plan
    # Indent the rowlbl2
    # row_grp_plan = row_grp_plan(
    #   row_grp_structure(group_val = ".default", element_block(post_space = " ")),
    #   label_loc = element_row_grp_loc(location = "column")),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote 2",column_val ="Placebo"),
      marks="letters"

    )
  )

  gt3<- apply_footnote_plan(gt_start,tfrmt3,list(list(col="Placebo",spanning=FALSE,note="Test foontote 2")))
  expect_equal(nrow(gt3$`_footnotes`),1)


  # spanner footnote ##################################################################

  es_data2<- es_data %>%
    mutate(col2 = "Treatment column")

  gt_start2 <- tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column =c(col2,trt),
    param = param,
    value = value,
    # set formatting for values
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),

    # Specify row group plan
    # Indent the rowlbl2
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "column")),
  ) %>%
    print_to_gt(es_data2)

  tfrmt4<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = c(col2,trt),
    param = param,
    value = value,
    # set formatting for values
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),

    # Specify row group plan
    # Indent the rowlbl2
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "column")),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote 3",column_val=list(col2 = "Treatment column")),
      marks="numbers"

    )
  )

  gt4<- apply_footnote_plan(gt_start2,tfrmt4,list(list(col="Treatment column",spanning=TRUE,note="Test foontote 3")))
  expect_equal(nrow(gt4$`_footnotes`),1)

  # body footnote ##################################################################


  tfrmt5<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = trt,
    param = param,
    value = value,
    # set formatting for values
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),

    # Specify row group plan
    # Indent the rowlbl2
    # row_grp_plan = row_grp_plan(
    #   row_grp_structure(group_val = ".default", element_block(post_space = " ")),
    #   label_loc = element_row_grp_loc(location = "column")),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote 2",
                         column_val ="Placebo",
                         label_val = "Adverse Event",
                         group_val = "Completion Status"),
      marks="letters"

    )
  )

  gt5<- apply_footnote_plan(gt_start,tfrmt5,list(list(col="Placebo",spanning=TRUE,row=1,note="Test foontote 2")))
  expect_equal(nrow(gt5$`_footnotes`),1)


  # stub footnote ##################################################################
  tfrmt6<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = c(trt),
    param = param,
    value = value,
    # set formatting for values
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),

    # Specify row group plan
    # Indent the rowlbl2
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "column")),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote",label_val ="Adverse Event"),
      marks="letters"

    )
  )

  gt6<- apply_footnote_plan(gt_start,tfrmt6,list(list(col="rowlbl2",spanning=FALSE,row=c(1,2),note="Test foontote")))
  expect_equal(nrow(gt6$`_footnotes`),2)


  # group footnote ##################################################################

  tfrmt7<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = c(trt),
    param = param,
    value = value,
    # set formatting for values
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),

    # Specify row group plan
    # Indent the rowlbl2
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "spanning")),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote",group_val = "Completion Status"),
      marks="letters"

    )
  )

  gt7<- apply_footnote_plan(gt_start,tfrmt7,list(list(col="rowlbl1",spanning=FALSE,row=1,note="Test foontote")))
  expect_equal(nrow(gt7$`_footnotes`),1)
})



test_that("Check footnote order option works as expected", {

  #default marks_first
  tfrmt_ord <- tfrmt(
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
    ),
    footnote_plan = footnote_plan(
      footnote_structure("BMI: Body Mass Index"),
      footnote_structure("Placebo drug + standard of care", column_val = "column2"),
      footnote_structure("Source Note"),
      footnote_structure("label_1 footnote",  label_val="label_1"),
      marks = "numbers"
    )
  )

  gt_out <- print_mock_gt(tfrmt_ord)

  current_order_opt <- gt_out[["_options"]][["value"]][[1]]
  expect_equal(current_order_opt, "marks_first")

  #check the footnotes are all printed
  actual_fns <- gt_out$`_footnotes`$footnotes

  expected_fns <- c(
    "BMI: Body Mass Index",
    "Placebo drug + standard of care",
    "Source Note",
    "label_1 footnote"
  )

  expect_equal(as.character(actual_fns), expected_fns)

  #check preserve_order
  tfrmt_ord <- tfrmt_ord |>
    tfrmt(
    footnote_plan = footnote_plan(
      footnote_structure("BMI: Body Mass Index"),
      footnote_structure("Placebo drug + standard of care", column_val = "column2"),
      footnote_structure("Source Note"),
      footnote_structure("label_1 footnote",  label_val="label_1"),
      marks = "numbers",
      order = "preserve_order"
    )
  )

  gt_out <- print_mock_gt(tfrmt_ord)
  current_order_opt <- gt_out[["_options"]][["value"]][[1]]
  expect_equal(current_order_opt, "preserve_order")

  # check marks last
  tfrmt_ord <- tfrmt_ord |>
    tfrmt(
      footnote_plan = footnote_plan(
        footnote_structure("BMI: Body Mass Index"),
        footnote_structure("Placebo drug + standard of care", column_val = "column2"),
        footnote_structure("Source Note"),
        footnote_structure("label_1 footnote",  label_val="label_1"),
        marks = "numbers",
        order = "marks_last"
      )
    )

  gt_out <- print_mock_gt(tfrmt_ord)
  current_order_opt <- gt_out[["_options"]][["value"]][[1]]
  expect_equal(current_order_opt, "marks_last")

})
