#===============================================================================
#                              CHEAT SHEET IMAGES
#===============================================================================

devtools::load_all()
library(dplyr) # for arrange_all

#------------------------------- tfrmt-groups ---------------------------------

# table 1
df <- crossing(
  label = c("label 1", "label 2"),
  column = c("PL", "T1", "T2"),
  param = c("count", "percent")) %>%
  arrange_all()

tfrmt(
  label = label,
  column = column,
  param = param,

  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{count} ({percent})",
                                count = frmt("xx"),
                                percent = frmt("xx.x")))
  )) %>%
  print_mock_gt(df)

# table 2
df <- crossing(
  group = c("group 1", "group 2"),
  label = c("label 1", "label 2"),
  column = c("PL", "T1", "T2"),
  param = c("count", "percent")) %>%
  dplyr::arrange_all()

my_tfrmt<-tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,

  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default",
                      element_block(post_space = "   ")) ),


  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{count} ({percent})",
                                count = frmt("xx"),
                                percent = frmt("xx.x")))
  )) %>%
  print_mock_gt(df)

# table 3
df <- crossing(
  grp1 = c("group 1.1", "group 1.2"),
  grp2 = c("group 2.1", "group 2.2"),
  label = c("label 1", "label 2"),
  column = c("PL", "T1", "T2"),
  param = c("count", "percent")) %>%
  arrange_all()

tfrmt(
  group = c(grp1, grp2),
  label = label,
  column = column,
  param = param,

  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default",
                      element_block(post_space = "   ")) ),


  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{count} ({percent})",
                                count = frmt("xx"),
                                percent = frmt("xx.x")))
  )) %>%
  print_mock_gt(df)

#----------------------------- tfrmt-row_grp_plan ------------------------------

df <- crossing(
  group = c("group 1", "group 2"),
  label = c("label 1", "label 2"),
  column = c("PL", "T1", "T2"),
  param = c("count", "percent")) %>%
  arrange_all()

###### types group/row labels
my_tfrmt <- tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,

  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{count} ({percent}%)",
                                count = frmt("xx"),
                                percent = frmt("xx.x")))
  ))

# no row group plan
my_tfrmt %>%
  print_mock_gt(df)

# spanning (default)
my_tfrmt %>%
  tfrmt(
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "spanning"))) %>%
  print_mock_gt(df)

# indented
my_tfrmt %>%
  tfrmt(
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented"))) %>%
  print_mock_gt(df)

# column
my_tfrmt %>%
  tfrmt(
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "column"))) %>%
  print_mock_gt(df)

# noprint
# NOTE: promise evaluation error
my_tfrmt %>%
  tfrmt(
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "noprint"))) %>%
  print_mock_gt(df)

# post-space/custom (indented)
my_tfrmt %>%
  tfrmt(
    row_grp_plan = row_grp_plan(row_grp_structure(group_val = ".default", element_block(post_space = "---")))) %>%
  print_mock_gt(df)

#---------------------------- tfrmt-span_structure -----------------------------

# span_structure in col_plan
tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,

  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{count} ({percent}%)",
                                count = frmt("xx"),
                                percent = frmt("xx.x")))),

  row_grp_plan = row_grp_plan(row_grp_structure(group_val = ".default", element_block(post_space = "     "))),

  col_plan = col_plan(
    group, label,
    span_structure("Placebo", PL),
    span_structure("Treatment", T1, T2)
  )) %>%
  print_mock_gt(df)

# add span column to df
span_df <- crossing(
  group = c("group 1", "group 2"),
  label = c("label 1", "label 2", "label 3"),
  column = c("PL", "T1", "T2"),
  param = c("count", "percent")) %>%
  arrange_all() %>%
  mutate(span = case_when(column == "PL" ~ "Placebo",
                          column == "T1" ~ "Treatment",
                          column == "T2" ~ "Treatment"))

tfrmt(
  group = group,
  label = label,
  column = c(span, column),
  param = param,

  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{count} ({percent}%)",
                                count = frmt("xx"),
                                percent = frmt("xx.x")))),

  row_grp_plan = row_grp_plan(row_grp_structure(group_val = ".default", element_block(post_space = "     ")))
) %>%
  print_mock_gt(span_df)


#--------------------------------- tfrmt-frmt ----------------------------------

#> frmt, frmt_combine, frmt_when combined into one tfrmt

df <- bind_rows(
  crossing(
    label = c("count"),
    column = c("PL", "T1", "T2"),
    param = c("n")),
  crossing(
    label = c("median (sd)"),
    column = c("PL", "T1", "T2"),
    param = c("median", "sd")),
  crossing(
    label = c("pval"),
    column = c("PL", "T1", "T2"),
    param = c("pval"))
) %>%
  dplyr::arrange_all()

my_tfrmt <- tfrmt(
  label = label,
  column = column,
  param = param,

  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = "count",
                   frmt("xx")
    ),
    frmt_structure(group_val = ".default", label_val = "median (sd)",
                   frmt_combine("{median} ({sd})",
                                median = frmt("xx"),
                                sd = frmt("xx.x"))
    ),
    frmt_structure(group_val = ".default", label_val = "pval",
                   frmt_when(">0.99" ~ ">0.99",
                             "<0.001" ~ "<0.001",
                             TRUE ~ frmt("x.xxx", missing = ""))
    )
  ))

my_tfrmt %>%
  print_mock_gt(df)

#> frmt, frmt_combine, frmt_when in separate tfrmts

# frmt()
df <- crossing(
  label = c("label 1", "label 2", "label 3"),
  column = c("PL", "T1", "T2"),
  param = c("n")) %>%
  mutate(values = c(50, 45, 52, 52, 49, 55, 50, 47, 43))

tfrmt(
  label = label,
  column = column,
  param = param,
  values = values,

  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt("xx")
                   )
  )) %>%
  print_to_gt(df)

# frmt_combine()
df <- crossing(
  label = c("label 1", "label 2", "label 3"),
  column = c("PL", "T1", "T2"),
  param = c("count", "pct")) %>%
  mutate(values = c(50, 4.49, 45, 4.77, 52, 3.29, 52, 4.71, 49, 3.82,
                    55, 2.56, 50, 5.38, 47, 2.83, 43, 3.07))

tfrmt(
  label = label,
  column = column,
  param = param,
  values = values,

  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{count} ({pct})",
                                count = frmt("xx"),
                                pct = frmt("x.x"))
                      )
  )) %>%
  print_to_gt(df)

# frmt_when()
df <- crossing(
  label = c("label 1", "label 2", "label 3"),
  column = c("PL", "T1", "T2"),
  param = c("n")) %>%
  mutate(values = c(50, 45, 52, 52, 49, 55, 50, 47, 43))


tfrmt(
  label = label,
  column = column,
  param = param,
  values = values,

  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_when(">50" ~ ">50",
                             "<45" ~ "<45",
                             TRUE ~ frmt("xx", missing = ""))
                   )
  )) %>%
  print_to_gt(df)

#------------------------------- tfrmt-col_plan --------------------------------
# NOTE: images not generated for the following code; kept as extra code examples

df <- bind_rows(
  crossing(label = c("label 1", "label 2", "label 3"),
           column = c("T1", "T2", "PL"),
           param = c("count", "percent")),
  crossing(label = c("label 1", "label 2", "label 3"),
           column = c("Risk Diff T1-PL", "Risk Diff T2-PL"),
           param = c("num", "lower", "upper"))) %>%
  arrange_all()

mock_tbl <- function(tfrmt_object){
  tfrmt(
    # Specify columns in the data
    label = "label",
    param = "param",
    column = "column",
    values = value,

    # Specify body plan
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default",
                     frmt_combine("{count} ({percent})",
                                  count = frmt("xx"),
                                  percent = frmt("xx.x"))),
      frmt_structure(group_val = ".default", label_val = ".default",
                     frmt_combine(
                       "{num} ({lower}, {upper})",
                       num = frmt("xx.x"),
                       lower = frmt_when("==100"~ frmt(""),
                                         "==0"~ "",
                                         "TRUE" ~ frmt("xx.x")),
                       upper = frmt_when("==100"~ frmt(""),
                                         "==0"~ "",
                                         "TRUE" ~ frmt("xx.x"))))
    ))
}

mock_tbl() %>%
  print_mock_gt(df)

# default
mock_tbl() %>%
  tfrmt(
    col_plan = col_plan(
      everything()
    )) %>%
  print_mock_gt(df)

# manual
mock_tbl() %>%
  tfrmt(
    col_plan = col_plan(
      label, PL, T1, T2
    )) %>%
  print_mock_gt(df)

# tidyselect (regex)
mock_tbl() %>%
  tfrmt(
    col_plan = col_plan(
      label, matches("^[PT]")
    )) %>%
  print_mock_gt(df)

# tidyselect (string)
mock_tbl() %>%
  tfrmt(
    col_plan = col_plan(
      -starts_with("Risk")
    )) %>%
  print_mock_gt(df)

#-------------------------------------------------------------------------------

