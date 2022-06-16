
test_that("setting param sigdig defaults", {

  defaults <- list(max = 0, median = 1, "{mean} ({sd})" = c(1,2), n = NA)
  expect_equal(param_set(), defaults)

  expect_equal(param_set(max = 1, "{mean} ({sd})" = c(2,3)),
              list(median = 1, n = NA, max = 1, "{mean} ({sd})" = c(2,3)))

  expect_equal(param_set(new_prm = 4),
              list(max = 0, median = 1, "{mean} ({sd})" = c(1,2), n = NA, new_prm = 4))

})

test_that("build frmt objects",{

  #frmt
  frmt_spec <- frmt_builder(param = c("mean","sd"), frmt_string = c("xx.x","xx.xx"))
  frmt_string <- "mean = frmt('xx.x'), sd = frmt('xx.xx')"

  expect_equal(frmt_spec, frmt_string)

  frmt_spec <- frmt_builder(param = c("mean","sd"), frmt_string = c("xx.x","xx.xx"), missing = "--")
  frmt_string <- "mean = frmt('xx.x', missing = '--'), sd = frmt('xx.xx', missing = '--')"

  expect_equal(frmt_spec, frmt_string)


  # frmt_combine
  frmt_spec <- frmt_combine_builder(param_combine = "{mean} ({sd})",
                                    param = c("mean","sd"), frmt_string = c("xx.x","xx.xx"), missing = "-")
  frmt_string <- "frmt_combine('{mean} ({sd})', mean = frmt('xx.x', missing = '-'), sd = frmt('xx.xx', missing = '-'), missing = '-')"

  expect_equal(frmt_spec, frmt_string)



  # frmt_structure
  frmt_list <- list(
    "frmt_combine('{mean} ({sd})', mean = frmt('xx.x'), sd = frmt('xx.xx'))",
    "median = frmt('xx.x')",
    "n = frmt('xxx')"
  )
  frmt_spec <- frmt_structure_builder(group_val = ".default", label_val = "ige", frmt_list)
  frmt_string <- c(
    "frmt_structure(group_val = '.default', label_val = 'ige', frmt_combine('{mean} ({sd})', mean = frmt('xx.x'), sd = frmt('xx.xx')))",
    "frmt_structure(group_val = '.default', label_val = 'ige', median = frmt('xx.x'))",
    "frmt_structure(group_val = '.default', label_val = 'ige', n = frmt('xxx'))")

  expect_equal(frmt_spec, frmt_string)

  # build contents of body_plan by row
  dat_sigdig <- tribble(
    ~group1,  ~ group2, ~ sig_dig,
    "CHEM",  "ALANINE AMINOTRANSFERASE", 1,
    "CHEM", "BILIRUBIN", 1,
    "CHEM", "CHOLESTEROL", 2,
    "CHEM", "EOSINOPHILS", 2

  )
  # 1 group, 1 label
  bp_1grp_1lbl <- body_plan_builder(dat_sigdig[1,], group = vars(group1), label = quo(group2), param_defaults = param_set())
  bp_1grp_1lbl_man <- c("frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', frmt_combine('{mean} ({sd})', mean = frmt('xxx.xx'), sd = frmt('xxx.xxx')))",
                        "frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', max = frmt('xxx.x'))",
                        "frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', median = frmt('xxx.xx'))",
                        "frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', n = frmt('xxx'))")

  expect_equal(bp_1grp_1lbl,
               bp_1grp_1lbl_man)

  # 2 groups, no label
  bp_2grp_0lbl <- body_plan_builder(dat_sigdig[1,], group = vars(group1, group2), label = quo(), param_defaults = param_set())
  bp_2grp_0lbl_man <- c("frmt_structure(group_val = list(group1 = \"CHEM\", group2 = \"ALANINE AMINOTRANSFERASE\"), label_val = '.default', frmt_combine('{mean} ({sd})', mean = frmt('xxx.xx'), sd = frmt('xxx.xxx')))",
                        "frmt_structure(group_val = list(group1 = \"CHEM\", group2 = \"ALANINE AMINOTRANSFERASE\"), label_val = '.default', max = frmt('xxx.x'))",
                        "frmt_structure(group_val = list(group1 = \"CHEM\", group2 = \"ALANINE AMINOTRANSFERASE\"), label_val = '.default', median = frmt('xxx.xx'))",
                        "frmt_structure(group_val = list(group1 = \"CHEM\", group2 = \"ALANINE AMINOTRANSFERASE\"), label_val = '.default', n = frmt('xxx'))")

  expect_equal(bp_2grp_0lbl,
               bp_2grp_0lbl_man)


  # custom params
  bp_prm <- body_plan_builder(dat_sigdig[1,], group = vars(group1), label = quo(group2), param_defaults = param_set(max = 0, "{pct}%" = 0))
  bp_prm_man <- c("frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', frmt_combine('{mean} ({sd})', mean = frmt('xxx.xx'), sd = frmt('xxx.xxx')))",
                  "frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', max = frmt('xxx.x'))",
"frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', median = frmt('xxx.xx'))",
                  "frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', n = frmt('xxx'))",
                  "frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', pct = frmt('xxx.x%'))")
  expect_equal(bp_prm,
               bp_prm_man)


  # custom params
  bp_prm <- body_plan_builder(dat_sigdig[1,], group = vars(group1), label = quo(group2), param_defaults = param_set(max = 0, "{n} ({pct}%)" = c(NA, 0)))
  bp_prm_man <- c("frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', frmt_combine('{mean} ({sd})', mean = frmt('xxx.xx'), sd = frmt('xxx.xxx')))",
                  "frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', frmt_combine('{n} ({pct}%)', n = frmt('xxx'), pct = frmt('xxx.x')))",
                  "frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', max = frmt('xxx.x'))",
                  "frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', median = frmt('xxx.xx'))",
                  "frmt_structure(group_val = list(group1 = \"CHEM\"), label_val = 'ALANINE AMINOTRANSFERASE', n = frmt('xxx'))")
  expect_equal(bp_prm,
               bp_prm_man)
})

test_that("tfrmt_sigdig returns a tfrmt", {

  dat_sigdig <- tribble(
    ~group1,  ~ group2, ~ sig_dig,
    "CHEM",  "ALANINE AMINOTRANSFERASE", 1,
    "CHEM", "BILIRUBIN", 1

  )
  t_frmt <- tfrmt_sigdig(
    data = dat_sigdig,
    group = group1,
    label = group2
  )

  expect_s3_class(t_frmt,"tfrmt")

  expect_equal( t_frmt$group, vars(group1), ignore_attr = TRUE)
  expect_equal( t_frmt$label, quo(group2), ignore_attr = TRUE)

  # body plan contains 1 frmt_structure per default param per # of rows in sigddig spec
  num_frmt_structures_act <- length(t_frmt$body_plan)
  num_frmt_structures_exp <-  nrow(dat_sigdig)*length(param_set())
  expect_equal(num_frmt_structures_act, num_frmt_structures_exp)

  myprms <- param_set(newprm=2)
  t_frmt <- tfrmt_sigdig(
    data = dat_sigdig,
    group = group1,
    label = group2,
    param_defaults = myprms
  )
  num_frmt_structures_act <- length(t_frmt$body_plan)
  num_frmt_structures_exp <-  nrow(dat_sigdig)*length(myprms)
  expect_equal(num_frmt_structures_act, num_frmt_structures_exp)

})

