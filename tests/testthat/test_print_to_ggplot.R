test_that("inputs are as expected",{

  # create test data
  risk<-tibble(time=c(rep(c(0,1000,2000,3000),3)),
                 label=c(rep("Obs",4),rep("Lev",4),rep("Lev+5FU",4)),
                 value=c(630,372,256,11,620,360,266,8,608,425,328,14),
                 param=rep("n",12))

  riska<- risk %>%
    mutate(group="A")

  riskb<- risk %>%
    mutate(group="B",
           value=value+10)

  test_data<-riska %>%
    rbind(riskb)

  tfrmt_1<-tfrmt(
    # specify columns in the data
    group = group,
    label = label ,
    column = time,
    param = param,
    values = value)

  tfrmt_2<-tfrmt(
    # specify columns in the data
    group = c(rowlbl1,grp),
    label = rowlbl2,
    column = column,
    param = param,
    values = value,
    sorting_cols = c(ord1, ord2),
    # specify value formatting
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
    ),row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = "   "))
    ),
    # remove extra cols
    col_plan = col_plan(-grp,
                        -starts_with("ord") ))



  expect_error(print_to_ggplot(tfrmt_1,"test"),"Requires data")
  expect_error(print_to_ggplot(tfrmt="test",.data=test_data),"Requires a tfrmt object")
  expect_error(print_to_ggplot(tfrmt_2,test_data),"print_to_ggplot is not currently compatible with Body Plan, Row Group Plan, Column Plan or Column Alignment Plan. Please remove before continuing.")



})

test_that("group columns are created correctly",{
  tfrmt<-tfrmt(
    # specify columns in the data
    group = group,
    label = label ,
    column = time,
    param = param,
    values = value)

  test_data<-tibble(time=c(rep(c(0,1000,2000,3000),3)),
                    label=c(rep("Obs",4),rep("Lev",4),rep("Lev+5FU",4)),
                    value=c(630,372,256,11,620,360,266,8,608,425,328,14),
                    param=rep("n",12),
                    group="A")
  expected_data<-tibble(label=c("A",rep("  Obs",4),rep("  Lev",4),rep("  Lev+5FU",4)),
                        time=c(NA,rep(c(0,1000,2000,3000),3)),
                        value=c(NA,630,372,256,11,620,360,266,8,608,425,328,14),
                        param=c("",rep("n",12)))


  expect_equal(apply_grp_ggplot(test_data,tfrmt),expected_data)
})

test_that("tfrmt is as expected",{

  # create test data
  test_data<-tibble(time=c(rep(c(0,1000,2000,3000),3)),
               label=c(rep("Obs",4),rep("Lev",4),rep("Lev+5FU",4)),
               value=c(630,372,256,11,620,360,266,8,608,425,328,14),
               param=rep("n",12))



  tfrmt_1<-tfrmt(
    group = group,
    label = label ,
    param = param,
    values = value)

  tfrmt_2<-tfrmt(
    group = group,
    column = time,
    param = param,
    values = value)

  tfrmt_3<-tfrmt(
    group = group,
    label = label ,
    column = time,
    values = value)

  tfrmt_4<-tfrmt(
    group = group,
    label = label ,
    column = time,
    param= param)

  expect_error(print_to_ggplot(tfrmt_1,test_data),"column variable required for print_to_ggplot")
  expect_error(print_to_ggplot(tfrmt_2,test_data),"label variable required for print_to_ggplot")
  expect_error(print_to_ggplot(tfrmt_3,test_data),"param variable required for print_to_ggplot")
  expect_error(print_to_ggplot(tfrmt_4,test_data),"values variable required for print_to_ggplot")


})


