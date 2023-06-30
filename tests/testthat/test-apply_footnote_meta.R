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
    # set formatting for value
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
  # named column vals
  tfrmt2<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = trt,
    param = param,
    value = value,
    # set formatting for value
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote 1",column_val =list(trt="Placebo")),
      marks="letters"

    )
  )

  expect_equal(
    attr(apply_tfrmt(es_data,tfrmt2),".footnote_locs"),
    list(list("col"="Placebo","spanning"=FALSE,"note"="Test footnote 1"))

  )

  # spanning
  es_data2<- es_data %>%
    mutate(col2 = "Treatment column")

  tfrmt3<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = c(col2,trt),
    param = param,
    value = value,
    # set formatting for value
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
    attr(apply_tfrmt(es_data2,tfrmt3),".footnote_locs"),
    list(list("col"="Treatment column","spanning"=TRUE,"note"="Test footnote 3"))

  )

  # spanned
  es_data3<-es_data2 %>%
    mutate(col2="Treatment column 2") %>%
    rbind(es_data2)

  tfrmt3<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = c(col2,trt),
    param = param,
    value = value,
    # set formatting for value
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
      footnote_structure("Test footnote 2",column_val=list(trt="Placebo")),
      marks="letters"

    )
  )

  expect_equal(
    attr(apply_tfrmt(es_data2,tfrmt3),".footnote_locs"),
    list(list("col"="Treatment column___tlang_delim___Placebo","spanning"=FALSE,"note"="Test footnote 2"))

  )


})


test_that("applying footnote meta group val",{

  # set up data

  es_data <-tibble::tibble(rowlbl1 =c(rep("Completion Status",12),rep("Primary reason for withdrawal",28)),
                           rowlbl2 =c(rep("Completed",4),rep("Adverse Event",4),rep("Unknown",4),rep("Adverse Event",4),rep("Lost to follow-up",4),rep("Protocol violation",4),rep("Subject decided to withdraw",4),rep("Protocol Violation",4),rep("Pre-Operative Dose[1]",4),rep("Other",4)),
                           param=c(rep(c("n","n","pct","pct"),10)),
                           trt=c(rep(c("Placebo","Treatment"),20)),
                           value=c(24,19,2400/48,1900/38,5,1,500/48,100/38,19,18,1900/48,1800/38,1,1,100/48,100/38,0,0,0,0,0,0,0,0,1,1,100/48,100/38,1,4,100/48,400/38,1,0,100/48,0,2,3,200/48,300/38)

  )


  # spanning
  es_data2<- es_data %>%
    mutate(col2 = "Treatment column")


  # test warnings

  tfrmt<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = c(col2,trt),
    param = param,
    value = value,
    # set formatting for value
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
      footnote_structure("Test footnote 3",column_val=list(col2 = "Treatment column"),group_val = "Completion Status"),
      marks="numbers"

    )
  )

  expect_warning(apply_tfrmt(es_data2,tfrmt),
                 "Cannot apply footnotes to rows when you have only specified a spanning column")

  tfrmt2<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = c(trt),
    param = param,
    value = value,
    # set formatting for value
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
      label_loc = element_row_grp_loc(location = "noprint")),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote 3",group_val = "Completion Status"),
      marks="numbers"

    )
  )

  expect_warning(apply_tfrmt(es_data2,tfrmt2),
                 "Can not apply footnotes to group columns when 'noprint' is set"
                 )


  # footnote in group column

  tfrmt3<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = c(trt),
    param = param,
    value = value,
    # set formatting for value
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
      footnote_structure("Test footnote",group_val = "Completion Status"),
      marks="letters"

    )
  )


  expect_equal(
    attr(apply_tfrmt(es_data2,tfrmt3),".footnote_locs"),
    list(list("col"="rowlbl1","spanning"=FALSE,"row" =1,"note"="Test footnote"))

  )

  # footnote in label column
  tfrmt4<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = c(trt),
    param = param,
    value = value,
    # set formatting for value
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),

    # Specify row group plan
    #Indent the rowlbl2
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "indented")),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote",group_val = list(rowlbl1="Completion Status")),
      marks="letters"

    )
  )

  expect_equal(
    attr(apply_tfrmt(es_data2,tfrmt4),".footnote_locs"),
    list(list("col"="rowlbl2","spanning"=FALSE,"row" =1,"note"="Test footnote"))

  )

  # no row group plan

  es_data3<- es_data %>%
    mutate(rowlbl0="Test group")


  tfrmt5<-tfrmt(
    # specify columns in the data
    group = c(rowlbl0,rowlbl1),
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
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "gtdefault")),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote",group_val=list(rowlbl0="Test group",rowlbl1="Completion Status"),
                         # label_val=list(rowlbl2="Adverse Event")
      ),
      marks="letters"
    )
  )


  expect_equal(
    attr(apply_tfrmt(es_data3,tfrmt5),".footnote_locs"),
    list(list("col"="rowlbl1","spanning"=FALSE,"row" =1,"note"="Test footnote"))
  )

  # 2 levels of grouping and row_grp_plan w/ spanning/column
  tfrmt6<-tfrmt(
    # specify columns in the data
    group = c(rowlbl0,rowlbl1),
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
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "spanning")),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote")
    )
  )
  expect_equal(
    attr(apply_tfrmt(es_data3,tfrmt6),".footnote_locs"),
    list(list("col"=NULL,"spanning"=FALSE,"note"="Test footnote"))
  )
})


test_that("If 1 group/column var, can pass an unnamed vector",{


  # set up data
  dd <- tibble::tibble(
      rowlbl1 = "Completion Status",
      rowlbl2 = c("Completed", "Ongoing", "Unknown")) %>%
    bind_rows(
      tibble(
        rowlbl1 = "Primary reason for withdrawal",
        rowlbl2 = c("Other", "Lost to follow-up"))
      ) %>%
    crossing(
    param = c("n","pct"),
    trt = c("Placebo", "Trt1","Trt2","Trt3")
  ) %>%
    bind_cols(
       value=c(24,19,2400/48,1900/38,5,1,500/48,100/38,19,18,1900/48,1800/38,1,1,100/48,100/38,0,0,0,0,0,0,0,0,1,1,100/48,100/38,1,4,100/48,400/38,1,0,100/48,0,2,3,200/48,300/38)
  )

  # column_val locs

  tfrmt<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = trt,
    param = param,
    value = value,
    # set formatting for value
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote 1",column_val = c("Trt1", "Trt2", "Trt3")),
      marks="letters"

    )
  )

  expect_equal(
    attr(apply_tfrmt(dd,tfrmt),".footnote_locs"),
    list(list("col"=c("Trt1","Trt2","Trt3"),"spanning"=FALSE,"note"="Test footnote 1"))

  )


  # group_val locs

  tfrmt<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = trt,
    param = param,
    value = value,
    # set formatting for value
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote 1",group_val = c("Completion Status", "Primary reason for withdrawal")),
      marks="letters"

    ),
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(location = "spanning"))
  )

  expect_equal(
    attr(apply_tfrmt(dd,tfrmt),".footnote_locs"),
    list(list("col"="rowlbl1","spanning"=FALSE,"row"=c(1,2), note="Test footnote 1"))

  )


})


