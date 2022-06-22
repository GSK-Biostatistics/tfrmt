#--------------------------- NO GROUPS (SAME FRMT) -----------------------------
df <- crossing(
  label = c("label 1", "label 2"),
  column = c("PL", "T1", "T2"),
  param = c("count", "percent")
  ) %>% dplyr::arrange_all()

tfrmt(
  label = label,
  column = column,
  param = param,
  values = value, # not sure why this is needed??

  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{count} ({percent})",
                                count = frmt("xx"),
                                percent = frmt("xx.x")))
  )) %>% print_mock_gt(df)

#------------------------- 1 GROUP LEVEL (SAME FRMT) ---------------------------

df <- crossing(
  group = c("group 1", "group 2"),
  label = c("label 1", "label 2"),
  column = c("PL", "T1", "T2"),
  param = c("count", "percent")
) %>% dplyr::arrange_all()

tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  values = value,
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default",
                      element_block(post_space = "   ")) ),

  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{count} ({percent})",
                                count = frmt("xx"),
                                percent = frmt("xx.x")))
    )) %>% print_mock_gt(df)

#------------------------- 2 GROUP LEVELS (SAME FRMT) --------------------------

df <- crossing(
  grp1 = c("group 1.1", "group 1.2"),
  grp2 = c("group 2.1", "group 2.2"),
  label = c("label 1", "label 2"),
  column = c("PL", "T1", "T2"),
  param = c("count", "percent")
) %>% dplyr::arrange_all()

tfrmt(
  group = c(grp1, grp2),
  label = label,
  column = column,
  param = param,
  values = value,
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default",
                      element_block(post_space = "   ")) ),

  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{count} ({percent})",
                                count = frmt("xx"),
                                percent = frmt("xx.x")))
    )) %>% print_mock_gt(df)

#----------------- 1 GROUP LEVEL (DIFFERENT FRMT, label_val) -------------------

df <- bind_rows(
  crossing(group = c("group 1", "group 2"),
           label = c("n"),
           column = c("PL", "T1", "T2"),
           param = c("count")),
  crossing(group = c("group 1", "group 2"),
           label = c("Mean (SD)"),
           column = c("PL", "T1", "T2"),
           param = c("mean", "sd")),
  crossing(group = c("group 1", "group 2"),
           label = c("Median (Range)"),
           column = c("PL", "T1", "T2"),
           param = c("min", "max", "median"))) %>%
  dplyr::arrange_all()

tfrmt(
  # Specify columns in the data
  group = group,
  label = label,
  column = column,
  param = param,
  values = value,
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default",
                      element_block(post_space = "   ")) ),

  # Specify body plan
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = "n", frmt("xx")),
    frmt_structure(group_val = ".default", label_val = "Median (Range)",
                   frmt_combine("{median} ({min},{max})",
                                median = frmt("xx.x"),
                                min = frmt("xx"),
                                max = frmt("xx"), missing = " ")),
    frmt_structure(group_val = ".default", label_val = "Mean (SD)",
                   frmt_combine("{mean} ({sd})",
                                mean = frmt("xx.x"),
                                sd = frmt("xx.xx"), missing = " "))
  )) %>%
  print_mock_gt(df)

#----------------- 1 GROUP LEVEL (DIFFERENT FRMT, group_val) -------------------

df <- bind_rows(
  crossing(group = c("group 1"),
           label = c("label 1", "label 2"),
           column = c("PL", "T1", "T2"),
           param = c("mean", "sd")),
  crossing(group = c("group 2"),
           label = c("label 1", "label 2"),
           column = c("PL", "T1", "T2"),
           param = c("median", "min", "max"))
) %>% dplyr::arrange_all()

tfrmt(
  # Specify columns in the data
  group = group,
  label = label,
  column = column,
  param = param,
  values = value,
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = list("group" = c("group 1", "group 2")),
                      element_block(post_space = "   ")) ),

  # Specify body plan
  body_plan = body_plan(
    frmt_structure(group_val = "group 1", label_val = ".default",
                   frmt_combine("{mean} ({sd})", mean = frmt("xx.x"), sd = frmt("xx.x"))),
    frmt_structure(group_val = "group 2", label_val = ".default",
                   frmt_combine("{median} ({min},{max})", median = frmt("xx"), min = frmt("xx"), max = frmt("xx"), missing = " "))
  )) %>%
  print_mock_gt(df)

#------------ 1 GROUP LEVEL (DIFFERENT FRMT, group_val, label_val) -------------

### SAME DF AS PREVIOUS

tfrmt(
  # Specify columns in the data
  group = group,
  label = label,
  column = column,
  param = param,
  values = value,
  #sorting_cols = c(ord1),
  row_grp_plan = row_grp_plan( row_grp_structure(group_val = list("group" = c("group 1", "group 2")), element_block(post_space = "   ")) ),

  # Specify body plan
  body_plan = body_plan(
    frmt_structure(group_val = "group 1", label_val = c("label 1"),
                   frmt_combine("{mean} ({sd})", mean = frmt("xx"), sd = frmt("xx"))),
    frmt_structure(group_val = "group 1", label_val = c("label 2"),
                   frmt_combine("{mean} ({sd})", mean = frmt("xx.x"), sd = frmt("xx.x"))),
    frmt_structure(group_val = "group 2", label_val = c("label 1", "label 2"),
                   frmt_combine("{median} ({min},{max})", median = frmt("xx"), min = frmt("xx"), max = frmt("xx"), missing = " "))
  )) %>%
  print_mock_gt(df)

#---------------- 2 GROUP LEVELS (DIFFERENT FRMT, group_val) -------------------

df <- bind_rows(
  crossing(grp1 = c("group 1.1"),
           grp2 = c("group 2.1", "group 2.2"),
           label = c("label 1", "label 2"),
           column = c("PL", "T1", "T2"),
           param = c("count")),
  crossing(grp1 = c("group 1.2"),
           grp2 = c("group 2.1", "group 2.2"),
           label = c("label 1", "label 2"),
           column = c("PL", "T1", "T2"),
           param = c("mean", "sd"))
) %>% dplyr::arrange_all()

tfrmt(
  group = c(grp1, grp2),
  label = label,
  column = column,
  param = param,
  values = value,
  row_grp_plan = row_grp_plan( row_grp_structure(group_val = ".default",
                                                 element_block(post_space = "   ")) ),

  body_plan = body_plan(
    frmt_structure(group_val = list("grp1" = "group 1.1", "grp2" = c("group 2.1", "group 2.2")), label_val = ".default",
                   frmt("xx")),
    frmt_structure(group_val = list("grp1" = "group 1.2", "grp2" = c("group 2.1", "group 2.2")), label_val = ".default",
                   frmt_combine("{mean} ({sd})",
                                mean = frmt("xx.x"),
                                sd = frmt("xx.x")))
    )) %>% print_mock_gt(df)

################################################################################

#------------------------------- ADD SPAN IN DF --------------------------------

df <- crossing(label = c("label 1", "label 2", "label 3"),
               column = c("PL", "T1", "T2"),
               param = c("count", "percent")) %>%
  mutate(span = case_when(column == "PL" ~ "Placebo",
                          column == "T1" ~ "Treatment",
                          column == "T2" ~ "Treatment"))


tfrmt(
  # Specify columns in the data
  label = label,
  column = c(span, column), # REFER TO COL_PLAN ARTICLE FOR MORE DETAILS
  param = param,
  values = value,

  # Specify body plan
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine(
                     "{count} {percent}",
                     count = frmt("xxx"),
                     percent = frmt_when("==100"~ frmt(""),
                                         "==0"~ "",
                                         "TRUE" ~ frmt("(xx.x%)"))))
  )) %>%
  print_mock_gt(df)



#---------------------------- ADD SPAN IN tfrmt() ------------------------------

df <- crossing(label = c("label 1", "label 2", "label 3"),
               column = c("PL", "T1", "T2"),
               param = c("count", "percent"))

tfrmt(
  # Specify columns in the data
  label = label,
  column = column,
  param = param,
  values = value,

  # Specify body plan
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine(
                     "{count} {percent}",
                     count = frmt("xxx"),
                     percent = frmt_when("==100"~ frmt(""),
                                         "==0"~ "",
                                         "TRUE" ~ frmt("(xx.x%)"))))
  ),
  col_plan = col_plan(
    label,
    span_structure("Placebo", PL),
    span_structure("Treatment", T1, T2)
  )
) %>%
  print_mock_gt(df)

################################################################################

#----------------------------- USING sorting_cols ------------------------------

### USING DF WITH 2 GROUP LEVELS

df <- crossing(
  grp1 = c("group 1.1", "group 1.2"),
  grp2 = c("group 2.1", "group 2.2"),
  label = c("label 1", "label 2"),
  column = c("PL", "T1", "T2"),
  param = c("count", "percent")) %>%
  dplyr::arrange_all() %>%
  mutate(ord1 = rep(seq( 1,length(unique(.$grp1)) ),
                    each = nrow(.)/length(unique(.$grp1)) ),
         ord2 = rep(seq( 1,length(unique(.$grp2)) ),
                    each = length(unique(.$column))*length(unique(.$param))*length(unique(.$label)),
                    times = length(unique(.$grp1)) )
         )

tfrmt(
  group = c(grp1, grp2),
  label = label,
  column = column,
  param = param,
  values = value,
  sorting_cols = vars(ord1, ord2),
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default",
                      element_block(post_space = "   ")) ),

  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{count} ({percent})",
                                count = frmt("xx"),
                                percent = frmt("xx.x")))
  )) %>% print_mock_gt(df)




