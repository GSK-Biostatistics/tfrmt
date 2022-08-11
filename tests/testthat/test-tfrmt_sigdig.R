
test_that("setting param sigdig defaults", {

  defaults <- list(min = 1, max = 1, median = 1, "{mean} ({sd})" = c(1,2), n = NA)
  expect_equal(param_set(), defaults)

  expect_equal(param_set(max = 2, "{mean} ({sd})" = c(2,3)),
              list(min = 1, median = 1, n = NA, max = 2, "{mean} ({sd})" = c(2,3)))

  expect_equal(param_set(new_prm = 4),
              list(min = 1, max = 1, median = 1, "{mean} ({sd})" = c(1,2), n = NA, new_prm = 4))

  expect_equal(param_set(mean = 0),
               list(min = 1, max = 1, median = 1, n = NA, mean = 0))

  expect_equal(param_set("{n} ({pct}%)" = c(NA, 1)),
               list(min = 1, max = 1, median = 1, "{mean} ({sd})" = c(1,2), "{n} ({pct}%)" = c(NA, 1)))
})

test_that("build frmt objects",{

  #frmt
  frmt_spec <- frmt_builder(param = c("mean","sd"), frmt_string = c("xx.x","xx.xx"))
  expect_equal(frmt_spec, list( mean = frmt("xx.x"), sd = frmt('xx.xx')))

  frmt_spec <- frmt_builder(param = c("mean","sd"), frmt_string = c("xx.x","xx.xx"), missing = "--")
  expect_equal(frmt_spec, list(mean = frmt('xx.x', missing = '--'), sd = frmt('xx.xx', missing = '--')))

  frmt_spec <- frmt_builder(frmt_string = c("xx.x","xx.xx"), missing = "--")
  expect_equal(frmt_spec, list(frmt('xx.x', missing = '--'), frmt('xx.xx', missing = '--')))

  # frmt_combine
  frmt_spec <- frmt_combine_builder(param_combine = "{mean} ({sd})",
                                    param = c("mean","sd"), frmt_string = c("xx.x","xx.xx"), missing = "-")
  frmt_string <- list(frmt_combine('{mean} ({sd})', mean = frmt('xx.x', missing = '-'), sd = frmt('xx.xx', missing = '-'), missing = '-'))
  expect_equal(frmt_spec, frmt_string)



  # frmt_structure
  frmt_list <- list(
    frmt_combine('{mean} ({sd})', mean = frmt('xx.x'), sd = frmt('xx.xx')),
    median = frmt('xx.x'),
    n = frmt('xxx')
  )
  frmt_spec <- frmt_structure_builder(group_val = ".default", label_val = "ige", frmt_list)
  frmt_string <- list(
    frmt_structure(group_val = ".default", label_val = "ige", frmt_combine('{mean} ({sd})', mean = frmt('xx.x'), sd = frmt('xx.xx'))),
    frmt_structure(group_val = ".default", label_val = "ige", median = frmt('xx.x')),
    frmt_structure(group_val = ".default", label_val = "ige", n = frmt('xxx')))

  expect_equal(frmt_spec, frmt_string)


  frmt_spec <- frmt_structure_builder(group_val = ".default", label_val = c("ige","igg"), frmt_list)
  frmt_string <- list(
    frmt_structure(group_val = ".default", label_val = c("ige","igg"), frmt_combine('{mean} ({sd})', mean = frmt('xx.x'), sd = frmt('xx.xx'))),
    frmt_structure(group_val = ".default", label_val = c("ige","igg"), median = frmt('xx.x'))  ,
    frmt_structure(group_val = ".default", label_val = c("ige","igg"), n = frmt('xxx')))
  expect_equal(frmt_spec, frmt_string)


  # build contents of body_plan for a given sigdig value
  dat_sigdig <- tibble::tribble(
    ~group1,  ~ group2, ~ sigdig,
    "CHEM",  "ALANINE AMINOTRANSFERASE", 1,
    "CHEM", "CHOLESTEROL", 1
  )
  # 1 group, 1 label
  bp_1grp_1lbl <- body_plan_builder(dat_sigdig, group = vars(group1), label = quo(group2), param_defaults = param_set())
  bp_1grp_1lbl_man <- list(frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), min = frmt('x.xx')),
                           frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), max = frmt('x.xx')),
                           frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), median = frmt('x.xx')),
                           frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), frmt_combine('{mean} ({sd})', mean = frmt('x.xx'), sd = frmt('x.xxx'))),
                           frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), n = frmt('x')))
  expect_equal(bp_1grp_1lbl,
               bp_1grp_1lbl_man)

  # 2 groups, no label
  bp_2grp_0lbl <- body_plan_builder(dat_sigdig, group = vars(group1, group2), label = quo(), param_defaults = param_set())
  bp_2grp_0lbl_man <- list(frmt_structure(group_val = list(group1 = "CHEM", group2 = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL")), label_val = c(".default"), min = frmt('x.xx')),
                           frmt_structure(group_val = list(group1 = "CHEM", group2 = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL")), label_val = c(".default"), max = frmt('x.xx')),
                           frmt_structure(group_val = list(group1 = "CHEM", group2 = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL")), label_val = c(".default"), median = frmt('x.xx')),
                           frmt_structure(group_val = list(group1 = "CHEM", group2 = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL")), label_val = c(".default"), frmt_combine('{mean} ({sd})', mean = frmt('x.xx'), sd = frmt('x.xxx'))),
                           frmt_structure(group_val = list(group1 = "CHEM", group2 = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL")), label_val = c(".default"), n = frmt('x')) )
  expect_equal(bp_2grp_0lbl,
               bp_2grp_0lbl_man)


  # custom params
  bp_prm <- body_plan_builder(dat_sigdig, group = vars(group1), label = quo(group2), param_defaults = param_set(max = 0, "{pct}%" = 0))
  bp_prm_man <- list(frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), min = frmt('x.xx')),
                     frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), median = frmt('x.xx')),
                     frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), frmt_combine('{mean} ({sd})', mean = frmt('x.xx'), sd = frmt('x.xxx'))),
                     frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), n = frmt('x')),
                     frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), max = frmt('x.x')),
                     frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), pct = frmt('x.x%')))
  expect_equal(bp_prm,
               bp_prm_man)


  # custom params
  bp_prm <- body_plan_builder(dat_sigdig, group = vars(group1), label = quo(group2), param_defaults = param_set(max = 0, "{n} ({pct}%)" = c(NA, 0)))
  bp_prm_man <- list(frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), min = frmt('x.xx')),
                     frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), median = frmt('x.xx')),
                     frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), frmt_combine('{mean} ({sd})', mean = frmt('x.xx'), sd = frmt('x.xxx'))),
                     frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), max = frmt('x.x')),
                     frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "CHOLESTEROL"), frmt_combine('{n} ({pct}%)', n = frmt('x'), pct = frmt('x.x')))
                     )

  expect_equal(bp_prm,
               bp_prm_man)
})

test_that("no redundant frmt_structures",{

  dat_sigdig <- tibble::tribble(
    ~group1,  ~ group2, ~ sigdig,
    ".default", ".default", 1,
    "CHEM",  "ALANINE AMINOTRANSFERASE", 1,
    "CHEM", "BILIRUBIN", 1,
    "CHEM", "CHOLESTEROL", 2,
    "HEM", "EOSINOPHILS", 2
  )
  bp <- tfrmt_sigdig(dat_sigdig, group = vars(group1), label = quo(group2), param_defaults = param_set())$body_plan
  bp_man <- body_plan(frmt_structure(group_val = list(group1 = ".default"), label_val = c(".default"), min = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = ".default"), label_val = c(".default"), max = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = ".default"), label_val = c(".default"), median = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = ".default"), label_val = c(".default"), frmt_combine('{mean} ({sd})', mean = frmt('x.xx'), sd = frmt('x.xxx'))),
                      frmt_structure(group_val = list(group1 = ".default"), label_val = c(".default"), n = frmt('x')),
                      frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "BILIRUBIN"), min = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "BILIRUBIN"), max = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "BILIRUBIN"), median = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "BILIRUBIN"), frmt_combine('{mean} ({sd})', mean = frmt('x.xx'), sd = frmt('x.xxx'))),
                      frmt_structure(group_val = list(group1 = "CHEM"), label_val = c("ALANINE AMINOTRANSFERASE", "BILIRUBIN"), n = frmt('x')),
                      frmt_structure(group_val = list(group1 = c("CHEM", "HEM")), label_val = c("CHOLESTEROL", "EOSINOPHILS"), min = frmt('x.xxx')),
                      frmt_structure(group_val = list(group1 = c("CHEM", "HEM")), label_val = c("CHOLESTEROL", "EOSINOPHILS"), max = frmt('x.xxx')),
                      frmt_structure(group_val = list(group1 = c("CHEM", "HEM")), label_val = c("CHOLESTEROL", "EOSINOPHILS"), median = frmt('x.xxx')),
                      frmt_structure(group_val = list(group1 = c("CHEM", "HEM")), label_val = c("CHOLESTEROL", "EOSINOPHILS"), frmt_combine('{mean} ({sd})', mean = frmt('x.xxx'), sd = frmt('x.xxxx'))),
                      frmt_structure(group_val = list(group1 = c("CHEM", "HEM")), label_val = c("CHOLESTEROL", "EOSINOPHILS"), n = frmt('x')))
  expect_equal(bp,
               bp_man)

  dat_sigdig <- tibble::tribble(
    ~group1,  ~ group2, ~ lbl, ~ sigdig,
    "CHEM", "BILIRUBIN", "v1", 1,
    "CHEM",  ".default", "v2",1,
    "HEM", "CHOLESTEROL", "v1",2,
    ".default", "EOSINOPHILS", "v1",2
  )
  bp <- tfrmt_sigdig(dat_sigdig, group = vars(group1, group2), label = lbl, param_defaults = param_set())$body_plan
  bp_man <-  body_plan(
    frmt_structure(group_val = list(group1 = "CHEM", group2 = ".default"), label_val = c("v2"), min = frmt('x.xx')),
    frmt_structure(group_val = list(group1 = "CHEM", group2 = ".default"), label_val = c("v2"), max = frmt('x.xx')),
    frmt_structure(group_val = list(group1 = "CHEM", group2 = ".default"), label_val = c("v2"), median = frmt('x.xx')),
    frmt_structure(group_val = list(group1 = "CHEM", group2 = ".default"), label_val = c("v2"), frmt_combine('{mean} ({sd})', mean = frmt('x.xx'), sd = frmt('x.xxx'))),
    frmt_structure(group_val = list(group1 = "CHEM", group2 = ".default"), label_val = c("v2"), n = frmt('x')),
    frmt_structure(group_val = list(group1 = ".default", group2 = c("EOSINOPHILS")), label_val = c("v1"), min = frmt('x.xxx')),
    frmt_structure(group_val = list(group1 = ".default", group2 = c("EOSINOPHILS")), label_val = c("v1"), max = frmt('x.xxx')),
    frmt_structure(group_val = list(group1 = ".default", group2 = c("EOSINOPHILS")), label_val = c("v1"), median = frmt('x.xxx')),
    frmt_structure(group_val = list(group1 = ".default", group2 = c("EOSINOPHILS")), label_val = c("v1"), frmt_combine('{mean} ({sd})', mean = frmt('x.xxx'), sd = frmt('x.xxxx'))),
    frmt_structure(group_val = list(group1 = ".default", group2 = c("EOSINOPHILS")), label_val = c("v1"), n = frmt('x')),
    frmt_structure(group_val = list(group1 = "CHEM", group2 = "BILIRUBIN"), label_val = c("v1"), min = frmt('x.xx')),
    frmt_structure(group_val = list(group1 = "CHEM", group2 = "BILIRUBIN"), label_val = c("v1"), max = frmt('x.xx')),
    frmt_structure(group_val = list(group1 = "CHEM", group2 = "BILIRUBIN"), label_val = c("v1"), median = frmt('x.xx')),
    frmt_structure(group_val = list(group1 = "CHEM", group2 = "BILIRUBIN"), label_val = c("v1"), frmt_combine('{mean} ({sd})', mean = frmt('x.xx'), sd = frmt('x.xxx'))),
    frmt_structure(group_val = list(group1 = "CHEM", group2 = "BILIRUBIN"), label_val = c("v1"), n = frmt('x')),
    frmt_structure(group_val = list(group1 = "HEM", group2 = "CHOLESTEROL"), label_val = c("v1"), min = frmt('x.xxx')),
    frmt_structure(group_val = list(group1 = "HEM", group2 = "CHOLESTEROL"), label_val = c("v1"), max = frmt('x.xxx')),
    frmt_structure(group_val = list(group1 = "HEM", group2 = "CHOLESTEROL"), label_val = c("v1"), median = frmt('x.xxx')),
    frmt_structure(group_val = list(group1 = "HEM", group2 = "CHOLESTEROL"), label_val = c("v1"), frmt_combine('{mean} ({sd})', mean = frmt('x.xxx'), sd = frmt('x.xxxx'))),
    frmt_structure(group_val = list(group1 = "HEM", group2 = "CHOLESTEROL"), label_val = c("v1"), n = frmt('x'))
    )
  expect_equal(bp,
               bp_man)
})


test_that("tfrmt_sigdig returns a tfrmt", {

  dat_sigdig <- tibble::tribble(
    ~group1,  ~ group2, ~ sigdig,
    "CHEM",  "ALANINE AMINOTRANSFERASE", 1,
    "CHEM", "BILIRUBIN", 1

  )
  t_frmt <- tfrmt_sigdig(
    sigdig_df = dat_sigdig,
    group = group1,
    label = group2
  )

  expect_s3_class(t_frmt,"tfrmt")

  expect_equal( t_frmt$group, vars(group1), ignore_attr = TRUE)
  expect_equal( t_frmt$label, quo(group2), ignore_attr = TRUE)

  # body plan contains 1 frmt_structure per default param per # of rows in sigddig spec
  expect_equal(length(t_frmt$body_plan),
               length(param_set()))

  myprms <- param_set(newprm=2)
  t_frmt <- tfrmt_sigdig(
    sigdig_df = dat_sigdig,
    group = group1,
    label = group2,
    param_defaults = myprms
  )
  expect_equal(length(t_frmt$body_plan),
               length(param_set())+1)

})


test_that("varying group/label inputs",{

  dat_sigdig <- tibble::tribble(
    ~group1,  ~ group2, ~ lbl, ~ sigdig,
    "CHEM", "BILIRUBIN", "v1", 1,
    "CHEM",  ".default", "v2",1,
    "HEM", "CHOLESTEROL", "v1",2,
    ".default", "EOSINOPHILS", "v1",2
  )

  # if no group or label, assume all non-sigdig columns are groups
  t_out <- tfrmt_sigdig(sigdig_df = dat_sigdig)

  expect_equal(t_out$group, vars(group1, group2, lbl), ignore_attr = TRUE)

  expect_equal(t_out$label, quo())

  # if only some are specified, assume the rest are groups
  t_out <- tfrmt_sigdig(dat_sigdig, label = group2)

  expect_equal(t_out$group, vars(group1, lbl), ignore_attr = TRUE)
  expect_equal(t_out$label, quo(group2), ignore_attr = TRUE)

  # including a group or label that is not present in the data
  expect_warning(
    t_out <- tfrmt_sigdig(dat_sigdig, group = vars(lbl, grp3), label = mylab),
    paste0("`sigdig_df` input does not contain the following group params: grp3")
  )

  expect_equal(t_out$group, vars(lbl, grp3), ignore_attr = TRUE)
  expect_equal(t_out$label, quo(mylab), ignore_attr = TRUE)

  expect_error(
    tfrmt_sigdig(dat_sigdig, group = group4, label = group3),
    paste0("`sigdig_df` input does not contain any of the specified group/label params:\n",
           "group: group4\n",
           "label: group3")
  )


})




test_that("group vars specified in tfrmt but not sigdig data are represented in body_plan",{

  dat_sigdig <- tibble::tribble(
    ~group1,  ~ group2, ~ sigdig,
    "test1", ".default", 1
  )
  expect_warning(
    bp <- tfrmt_sigdig(dat_sigdig, group = vars(group1, newgrp), label = quo(group2), param_defaults = param_set())$body_plan,
    paste0("`sigdig_df` input does not contain the following group params: newgrp")
  )

    bp_man <- body_plan(frmt_structure(group_val = list(group1 = "test1", newgrp = ".default"), label_val = c(".default"), min = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = "test1", newgrp = ".default"), label_val = c(".default"), max = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = "test1", newgrp = ".default"), label_val = c(".default"), median = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = "test1", newgrp = ".default"), label_val = c(".default"), frmt_combine('{mean} ({sd})', mean = frmt('x.xx'), sd = frmt('x.xxx'))),
                      frmt_structure(group_val = list(group1 = "test1", newgrp = ".default"), label_val = c(".default"), n = frmt('x')))

  expect_equal(bp, bp_man)


  expect_warning(
    bp <- tfrmt_sigdig(dat_sigdig, group = vars(group1, newgrp, group2), label = quo(mylab), param_defaults = param_set())$body_plan,
    paste0("`sigdig_df` input does not contain the following group params: newgrp")
  )
  bp_man <- body_plan(frmt_structure(group_val = list(group1 = "test1", newgrp = ".default", group2 = ".default"), label_val = c(".default"), min = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = "test1", newgrp = ".default", group2 = ".default"), label_val = c(".default"), max = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = "test1", newgrp = ".default", group2 = ".default"), label_val = c(".default"), median = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = "test1", newgrp = ".default", group2 = ".default"), label_val = c(".default"), frmt_combine('{mean} ({sd})', mean = frmt('x.xx'), sd = frmt('x.xxx'))),
                      frmt_structure(group_val = list(group1 = "test1", newgrp = ".default", group2 = ".default"), label_val = c(".default"), n = frmt('x')))

  expect_equal(bp, bp_man)
})

test_that("tfrmt_sigdig can be layered onto another tfrmt",{

  prev_tfrmt <- tfrmt(group = vars(group1, group2),
                      label = lblvar,
                      body_plan = body_plan(
                        frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.xx"))
                      ))
  dat_sigdig <- tibble::tribble(
    ~group1,  ~ group2, ~ sigdig,
    "test1", "test2", 1
  )
  new_tfrmt <- tfrmt_sigdig(dat_sigdig,
                            tfrmt_obj =
                              prev_tfrmt)

  expect_equal(new_tfrmt$group, prev_tfrmt$group)
  expect_equal(new_tfrmt$lblvar, prev_tfrmt$lblvar)

  bp_man <- body_plan(frmt_structure(group_val = ".default", label_val = ".default", frmt('xx.xx')),
                      frmt_structure(group_val = list(group1 = "test1", group2 = "test2"), label_val = c(".default"), min = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = "test1", group2 = "test2"), label_val = c(".default"), max = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = "test1", group2 = "test2"), label_val = c(".default"), median = frmt('x.xx')),
                      frmt_structure(group_val = list(group1 = "test1", group2 = "test2"), label_val = c(".default"), frmt_combine('{mean} ({sd})', mean = frmt('x.xx'), sd = frmt('x.xxx'))),
                      frmt_structure(group_val = list(group1 = "test1", group2 = "test2"), label_val = c(".default"), n = frmt('x')))

  expect_equal(new_tfrmt$body_plan, bp_man)



  prev_tfrmt <- tfrmt(group = somegrp,
                      label = group2)
  dat_sigdig <- tibble::tribble(
    ~group1,  ~ group2, ~ sigdig,
    "test1", "test2", 1
  )
  new_tfrmt <- tfrmt_sigdig(dat_sigdig,
                            group = group1,
                            tfrmt_obj = prev_tfrmt)

  expect_equal(new_tfrmt$group, vars(group1), ignore_attr = TRUE)
  expect_equal(new_tfrmt$label, quo(group2), ignore_attr = TRUE)

})
