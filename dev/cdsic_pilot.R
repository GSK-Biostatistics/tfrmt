# Table 14-1.02
test <- tibble(
  grp = rep(c("Completion Status:", "Reason for Early Termination (prior to Week 24):"), c(3,10) ),
  labels = c("Completed Week 24",
             "Early Termination (prior to Week 24)",
             "Missing",
             "Adverse Event",
             "Death",
             "Lack of Efficacy[2]",
             "Lost to Follow-up",
             "Subject decided to withdraw",
             "Physician decided to withdraw subject",
             "Protocol criteria not met",
             "Protocol violation",
             "Sponsor decision",
             "Missing")
) %>%
  crossing(param = c("n", "pct"),
           col = c("Placebo (N=86)",
                   "Xanomeline Low Dose (N=84)",
                   "Xanomeline High Dose (N=84)",
                   "Total (N=254)")) %>%
  bind_rows(
    tibble(grp = c("Completion Status:", "Reason for Early Termination (prior to Week 24):", "Reason for Early Termination (prior to Week 24):"),
           labels = c("Completed Week 24", "Adverse Event", "Lack of Efficacy[2]"),
           param = "pval",
           col = "p-value [1]")
  )


tfrmt(
  group = "grp",
  label = "labels",
  param = "param",
  column = "col",
  title = "Summary of End of Study Data",
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                n = frmt("xxx"),
                                                                                pct = frmt_when("==100" ~ "",
                                                                                                "==0" ~ "",
                                                                                                TRUE ~ frmt("(xx.x %)")))),
    frmt_structure(group_val = ".default", label_val = ".default", pval = frmt_when(">0.99" ~ ">0.99",
                                                                                 "<0.001" ~ "<0.001",
                                                                                 TRUE ~ frmt("x.xxx", missing = "")))
  ),
  row_grp_plan = row_grp_plan(row_grp_structure(group_val = ".default", element_block(post_space = " ")),
                              label_loc = element_row_grp_loc("indent", "     "))
) %>%
  print_mock_gt(test)


n_percent_tfrmt <- function(){
  tfrmt(
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)")))),
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {percent}",
                                                                                  n = frmt("xxx"),
                                                                                  percent = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))
    ),
    row_grp_plan = row_grp_plan(row_grp_structure(group_val = ".default", element_block(post_space = " ")),
                                label_loc = element_row_grp_loc("indent", "     "))
  )
}


data <- tribble(
  ~`Pooled Id`,  ~`Site Id`,
"701",  "701",
"703",  "703",
"704",  "704",
"705",  "705",
"708",  "708",
"709",  "709",
"710",  "710",
"713",  "713",
"716",  "716",
"718",  "718",
"900",  "702",
"900",  "706",
"900",  "707",
"900",  "711",
"900",  "714",
"900",  "715",
"900",  "717",
"Total", " ") %>%
  crossing(col1 = c("Placebo (N=86)",
                   "Xanomeline Low Dose (N=84)",
                   "Xanomeline High Dose (N=84)",
                   "Total (N=254)"),
           col2 = c("ITT", "Eff", "Com")) %>%
  mutate(val = rpois(216, 15),
         param = "val")

tfrmt(
  param = "param",
  value = "val",
  column = vars(col1, col2),
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt("XX"))
  ),
  row_grp_plan = row_grp_plan(label_loc =element_row_grp_loc("column"))
) %>%
  print_to_gt(data)


