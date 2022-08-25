library(gt)
library(tidyverse)

####################################################################
# Source note
####################################################################
es_data <-tibble::tibble(rowlbl1 =c(rep("Completion Status",12),rep("Primary reason for withdrawal",28)),
                 rowlbl2 =c(rep("Completed",4),rep("Adverse Event",4),rep("Unknown",4),rep("Adverse Event",4),rep("Lost to follow-up",4),rep("Protocol violation",4),rep("Subject decided to withdraw",4),rep("Protocol Violation",4),rep("Pre-Operative Dose[1]",4),rep("Other",4)),
                 param=c(rep(c("n","n","pct","pct"),10)),
                 trt=c(rep(c("Placebo","Treatment"),20)),
                 value=c(24,19,2400/48,1900/38,5,1,500/48,100/38,19,18,1900/48,1800/38,1,1,100/48,100/38,0,0,0,0,0,0,0,0,1,1,100/48,100/38,1,4,100/48,400/38,1,0,100/48,0,2,3,200/48,300/38)

)


tfrmt<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = trt,
  param = param,
  values = value,
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

tfrmt %>%
  print_to_gt(es_data)

# Column headers

####################################################################
# normal column headers:
####################################################################



tfrmt2<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = trt,
  param = param,
  values = value,
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

tfrmt2 %>%
  print_to_gt(es_data)


####################################################################
# single cell
####################################################################



tfrmt2<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = trt,
  param = param,
  values = value,
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

tfrmt2 %>%
  print_to_gt(es_data)



####################################################################
# spanning column headers
####################################################################


es_data2<- es_data %>%
  mutate(col2 = "Treatment column")

tfrmt<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = c(col2,trt),
  param = param,
  values = value,
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
tfrmt %>%
  print_to_gt(es_data2)


####################################################################
# Spanned column headers
# Specify spanned only
####################################################################

es_data3<-es_data2 %>%
  mutate(col2="Treatment column 2") %>%
  rbind(es_data2)

tfrmt<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = c(col2,trt),
  param = param,
  values = value,
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
    footnote_structure("Test footnote 2",column_val="Placebo"),
    marks="letters"

  )
)

tfrmt %>%
  print_to_gt(es_data3)


####################################################################
# Spanned column headers
# Specify spanned and spanning
####################################################################

tfrmt<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = c(col2,trt),
  param = param,
  values = value,
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
    footnote_structure("Test footnote 2",column_val=list(col2="Treatment column",trt="Placebo")),
    footnote_structure("Test footnote 3",column_val=list(col2="Treatment column",trt="Treatment")),
    footnote_structure("Test footnote 1",column_val=list(col2="Treatment column")),
    marks="letters"

  )
)

tfrmt %>%
  print_to_gt(es_data3)


####################################################################
# Different combinations of spanned
####################################################################

es_data4<-es_data3 %>%
  mutate(col3 = "Treatment columns")



tfrmt<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = c(col3,col2,trt),
  param = param,
  values = value,
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
    footnote_structure("Test footnote 1",column_val=list(col2="Treatment column 2")),
    footnote_structure("Test footnote 2",column_val=list(col2="Treatment column")),
    footnote_structure("Test footnote 3",column_val=list(col2="Treatment columns")),
    footnote_structure("Test footnote 4",column_val=list(trt="Placebo")),
    footnote_structure("Test footnote 5",column_val=list(col2="Treatment column",trt="Treatment")),
    footnote_structure("Test footnote 6",column_val=list(col3="Treatment columns",col2="Treatment column 2",trt="Treatment")),




    marks="letters"


  )
)

tfrmt %>%
  print_to_gt(es_data4)


####################################################################
# Row labels - not indented
####################################################################


tfrmt2<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = c(trt),
  param = param,
  values = value,
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

tfrmt2 %>%
  print_to_gt(es_data)


####################################################################
# Row labels - indented
####################################################################

tfrmt<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = c(trt),
  param = param,
  values = value,
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
    label_loc = element_row_grp_loc(location = "indented")),
  footnote_plan = footnote_plan(
    footnote_structure("Test footnote",label_val ="Adverse Event"),
    marks="letters"

  )
)

tfrmt %>%
  print_to_gt(es_data)


####################################################################
# Label for specific group - not indented
####################################################################
tfrmt<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = c(trt),
  param = param,
  values = value,
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
    footnote_structure("Test footnote",group_val = "Completion Status",label_val ="Adverse Event"),
    marks="letters"

  )
)

tfrmt %>%
  print_to_gt(es_data)

####################################################################
# Label for specific group - indented
####################################################################
tfrmt<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = c(trt),
  param = param,
  values = value,
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
    label_loc = element_row_grp_loc(location = "indented")),
  footnote_plan = footnote_plan(
    footnote_structure("Test footnote",group_val = "Completion Status",label_val ="Adverse Event"),
    marks="letters"

  )
)

tfrmt %>%
  print_to_gt(es_data)

####################################################################
# Group
####################################################################

# spanning
tfrmt2<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = c(trt),
  param = param,
  values = value,
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

tfrmt2 %>%
  print_to_gt(es_data)

# indented
tfrmt2<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = c(trt),
  param = param,
  values = value,
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
    label_loc = element_row_grp_loc(location = "indented")),
  footnote_plan = footnote_plan(
    footnote_structure("Test footnote",group_val = "Completion Status"),
    marks="letters"

  )
)


tfrmt2 %>%
  print_to_gt(es_data)


# column
tfrmt<-tfrmt(
  # specify columns in the data
  group = c(rowlbl1),
  label = rowlbl2,
  column = c(trt),
  param = param,
  values = value,
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
    footnote_structure("Test footnote",group_val = "Completion Status"),
    marks="letters"

  )
)

tfrmt %>%
  print_to_gt(es_data)

####################################################################
# Multiple groups
####################################################################

es_data5<- es_data %>%
  mutate(rowlbl0="Test group",
         rowlbl_1="Test group 2")


tfrmt2<-tfrmt(
  # specify columns in the data
  group = c(rowlbl0,rowlbl1),
  label = rowlbl2,
  column = trt,
  param = param,
  values = value,
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
  #   label_loc = element_row_grp_loc(location = "indented")),
  footnote_plan = footnote_plan(
    footnote_structure("Test footnote",group_val=list(rowlbl0="Test group",rowlbl1="Completion Status"),
                       # label_val=list(rowlbl2="Adverse Event")
                       ),
    marks="letters"
  )
)

tfrmt2 %>%
  print_to_gt(es_data5)

tfrmt2<-tfrmt(
  # specify columns in the data
  group = c(rowlbl_1,rowlbl0,rowlbl1),
  label = rowlbl2,
  column = trt,
  param = param,
  values = value,
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
    label_loc = element_row_grp_loc(location = "indented"))

)


tfrmt2 %>%
  print_to_gt(es_data5)






