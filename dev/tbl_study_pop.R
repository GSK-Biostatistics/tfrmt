data <- crossing(
  label = c("Intent-To-Treat (ITT)",
            "Safety",
            "Efficacy",
            "Complete Week 24",
            "Complete Study"
  ),
  column = c("Placebo\n(N=XX)",
             "Xanomeline\nLow Dose\n(N=XX)",
             "Xanomeline\nHigh Dose\n(N=XX)",
             "Total\n(N=XXX)"
  ),
  param = c("n", "percent")
)

plan <- tfrmt(
  label = "label",
  column = "column",
  param = "param",
  title = "Summary of Populations",
  body_style = table_body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{n} ({percent}%)",
                   n = frmt("xx"),
                   percent = frmt("xxx"))
    )
  )
)

#Make mock
print_mock_gt(plan, data)


data %>%
  rowwise() %>%
  mutate(val = ifelse(param == "n", rpois(1, 100),
                       rnorm(1, 75, 15)
                       )) %>%
  print_to_gt(plan, .)


data.frame(test_this_delm1 = "a", test_NA_delm2 = "b") %>% gt::gt() %>% gt::tab_spanner_delim("_")

data.frame(delm1 = "a", delm2 = "b") %>% gt::gt() %>% gt::tab_spanner("this", columns = c(delm1)) %>% gt::tab_spanner("test", columns = c(delm1, delm2))

data.frame(test_this_delm1 = "a", test__delm2 = "b") %>% gt::gt() %>% gt::tab_spanner_delim("_")
data.frame(test_this_delm1 = "a", test_delm2 = "b") %>% gt::gt() %>% gt::tab_spanner_delim("_")
