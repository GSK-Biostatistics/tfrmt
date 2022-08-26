test_that("applying footnote meta column val",{

   # set up data

  es_data <-tibble::tibble(rowlbl1 =c(rep("Completion Status",12),rep("Primary reason for withdrawal",28)),
                           rowlbl2 =c(rep("Completed",4),rep("Adverse Event",4),rep("Unknown",4),rep("Adverse Event",4),rep("Lost to follow-up",4),rep("Protocol violation",4),rep("Subject decided to withdraw",4),rep("Protocol Violation",4),rep("Pre-Operative Dose[1]",4),rep("Other",4)),
                           param=c(rep(c("n","n","pct","pct"),10)),
                           trt=c(rep(c("Placebo","Treatment"),20)),
                           value=c(24,19,2400/48,1900/38,5,1,500/48,100/38,19,18,1900/48,1800/38,1,1,100/48,100/38,0,0,0,0,0,0,0,0,1,1,100/48,100/38,1,4,100/48,400/38,1,0,100/48,0,2,3,200/48,300/38)

  )

  # tfrmt with column val
  tfrmt<-tfrmt(
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
      footnote_structure("Test footnote 1",column_val ="Placebo"),
      marks="letters"

    )
  )

  expect_equal(
    attr(apply_tfrmt(es_data,tfrmt),".footnote_locs"),
    list(list("col"="Placebo","spanning"=FALSE,"note"="Test footnote 1"))

  )

  # spanning
  es_data2<- es_data %>%
    mutate(col2 = "Treatment column")

  tfrmt2<-tfrmt(
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

  expect_equal(
    attr(apply_tfrmt(es_data2,tfrmt2),".footnote_locs"),
    list(list("col"="Treatment column","spanning"=TRUE,"note"="Test footnote 2"))

  )

})
