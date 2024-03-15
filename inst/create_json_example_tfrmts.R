
#------------------------------ Demography Table -------------------------------

tfrmt_demog <- tfrmt(
  group = c(rowlbl1,grp),
  label = rowlbl2,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
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

  col_plan = col_plan(-grp,
                      -starts_with("ord")
  )
  ,
  col_style_plan = col_style_plan(
    col_style_structure(align = c(".",","," "), col = c("Placebo", "Xanomeline Low Dose",
                                                        "Xanomeline High Dose", "Total", "p-value")),
    col_style_structure(align = "left", col = c("rowlbl1","rowlbl2"))
  ),

  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = " ")),
    label_loc = element_row_grp_loc(location = "column")
  )
)

#---------------------------------- AE Table -----------------------------------

tfrmt_ae <- tfrmt(
  group = AEBODSYS,
  label = AETERM,
  column = c(col2, col1),
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{n} {pct}",
                                n = frmt("XXX"),
                                pct = frmt_when(
                                  "==100" ~ "",
                                  "==0" ~ "",
                                  TRUE ~ frmt("(xx.x %)")))),
    frmt_structure(group_val = ".default", label_val = ".default",
                   AEs = frmt("[XXX]")),
    frmt_structure(group_val = ".default", label_val = ".default",
                   pval = frmt_when(">0.99" ~ ">0.99",
                                    "<0.001" ~ "<0.001",
                                    "<0.05" ~ frmt("x.xxx*"),
                                    TRUE ~ frmt("x.xxx", missing ="--")))
  ),

  row_grp_plan = row_grp_plan(
    label_loc = element_row_grp_loc(location = "indented")
  ),

  col_style_plan = col_style_plan(
    col_style_structure(align = c(".",","," "), col = c(p_low, p_high))
  ),

  col_plan = col_plan(
    -starts_with("ord"),
    span_structure(
      col2 = c(
        "Xanomeline High Dose (N=84)" = `Xanomeline High Dose`,
        "Xanomeline Low Dose (N=84)" = `Xanomeline Low Dose`,
        "Placebo (N=86)" = Placebo
      ),
      col1 = c(`n (%)` = `n_pct` ,
               `[AEs]` = `AEs`)
    ),
    span_structure(
      col2 = c("Fisher's Exact p-values" = fisher_pval),
      col1 = c(
        # add a line break to help with table formatting
        `Placebo vs.\n Low Dose` = `p_low` ,
        `Placebo vs.\n High Dose` = `p_high`
      )
    ))
)

#------------------------------ Efficacy Table -------------------------------

tfrmt_efficacy <- tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = "n",
                   frmt("xx")),  # we could also do: label_val = ".default", n = frmt("xx")
    frmt_structure(group_val = ".default", label_val = ".default",
                   p.value = frmt_when("<0.001" ~ "<0.001",
                                       ">0.99" ~ ">0.99",
                                       TRUE ~ frmt("x.xxx", missing = " "))),
    frmt_structure(group_val = ".default", label_val = "Median (Range)",
                   frmt_combine("{median} ({min};{max})",
                                median = frmt("xx.x"),
                                min = frmt("xx"),
                                max = frmt("xx"), missing = " ")),
    frmt_structure(group_val = ".default", label_val = "Mean (SD)",
                   frmt_combine("{mean} ({sd})",
                                mean = frmt("xx.x"),
                                sd = frmt("xx.xx"), missing = " ")),
    frmt_structure(group_val = ".default", label_val = "Diff of LS Means (SE)",
                   frmt_combine("{diff} ({diff_se})",
                                diff = frmt("xx.x"),
                                diff_se = frmt("xx.xx"), missing = " ")),
    frmt_structure(group_val = ".default", label_val = "95% CI",
                   frmt_combine("({diff_lcl};{diff_ucl})",
                                diff_lcl = frmt("xx.x"),
                                diff_ucl = frmt("xx.x"), missing = " "))
  ),
  col_plan = col_plan(
    group, label, Placebo, contains("Low"), contains("High"), -starts_with("ord")
  ),
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = list(group="Change from Baseline"),
                      element_block(post_space = " ")),
    row_grp_structure(group_val = list(group="p-value (Dose Response)"),
                      element_block(post_space = " ")),
    row_grp_structure(group_val = list(group="p-value (Xan - Placebo)"),
                      element_block(post_space = " ")),
    label_loc = element_row_grp_loc(location = "indented")
  )
)

#--------------------------- Print Examples To {gt} ----------------------------

data_ae2 <- data_ae %>%
  group_by(AEBODSYS, AETERM) %>%
  mutate(pct_high = value[col2=="Xanomeline High Dose" & param=="pct"]) %>%
  ungroup %>%
  filter(pct_high >10) %>%
  select(-pct_high)


tfrmt_demog %>% print_to_gt(data_demog)
tfrmt_ae %>% print_to_gt(data_ae2)
tfrmt_efficacy %>% print_to_gt(data_efficacy)

#------------------------------ As JSON Examples -------------------------------

tfrmt_demog %>% as_json()
tfrmt_ae %>% as_json()
tfrmt_efficacy %>% as_json()

#----------------------------- Make JSON Examples ------------------------------

tfrmt_demog %>% tfrmt_to_json(path = "inst/json_examples/tfrmt_demog.json")
tfrmt_ae %>% tfrmt_to_json(path = "inst/json_examples/tfrmt_ae.json")
tfrmt_efficacy %>% tfrmt_to_json(path = "inst/json_examples/tfrmt_efficacy.json")

#----------------------------- Read JSON Examples ------------------------------

json_to_tfrmt(path = "inst/json_examples/tfrmt_demog.json") %>% print_to_gt(data_demog)

json_to_tfrmt(path = "inst/json_examples/tfrmt_ae.json") %>% print_to_gt(data_ae2)

json_to_tfrmt(path = "inst/json_examples/tfrmt_efficacy.json") %>% print_to_gt(data_efficacy)


