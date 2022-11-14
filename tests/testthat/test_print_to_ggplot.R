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
    value = value)

  # label loc not indented
  tfrmt_2<-tfrmt(
    # specify columns in the data
    group = group,
    label = label ,
    column = time,
    param = param,
    value = value,
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "column")))

  # two column variables
  tfrmt_4<-tfrmt(
    # specify columns in the data
    group = group,
    label = label ,
    column = c(time,time2),
    param = param,
    value = value
  )

  # col style plan
  tfrmt_5<-tfrmt(
    # specify columns in the data
    label = label ,
    column = time,
    param = param,
    value = value,
    col_style_plan =  col_style_plan(
      col_style_structure(align = "right", col = `1000`))
  )

  expect_error(print_to_ggplot(tfrmt_1,"test"),"Requires data")
  expect_error(print_to_ggplot(tfrmt="test",.data=test_data),"Requires a tfrmt object")
  expect_error(print_to_ggplot(tfrmt_2,risk),"print_to_ggplot must have label location 'indented' if row_group_plan is present")
  expect_error(print_to_ggplot(tfrmt_4,risk),"print_to_ggplot does not support multiple column variables")
  expect_error(print_to_ggplot(tfrmt_5,risk),"print_to_ggplot does not support col_style_plan elements")



})

test_that("group columns are created correctly",{
  tfrmt<-tfrmt(
    # specify columns in the data
    group = group,
    label = label ,
    column = time,
    param = param,
    value = value,
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc("gtdefault")))

  test_data<-tibble(time=c(rep(c(0,1000,2000,3000),3)),
                    label=c(rep("Obs",4),rep("Lev",4),rep("Lev+5FU",4)),
                    value=c(630,372,256,11,620,360,266,8,608,425,328,14),
                    param=rep("n",12),
                    group="A")
  expected_data<-tibble(label=c("A",rep("    Obs",4),rep("    Lev",4),rep("    Lev+5FU",4)),
                        time=c(NA,rep(c(0,1000,2000,3000),3)),
                        value=c(NA,630,372,256,11,620,360,266,8,608,425,328,14),
                        param=c(NA,rep("n",12)),
                        `..tfrmt_row_grp_lbl`=c(TRUE,rep(FALSE,12)))


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
    value = value)

  tfrmt_2<-tfrmt(
    group = group,
    column = time,
    param = param,
    value = value)

  tfrmt_3<-tfrmt(
    group = group,
    label = label ,
    column = time,
    value = value)

  tfrmt_4<-tfrmt(
    group = group,
    label = label ,
    column = time,
    param= param)

  expect_error(print_to_ggplot(tfrmt_1,test_data),"column variable required for print_to_ggplot")
  expect_error(print_to_ggplot(tfrmt_2,test_data),"label variable required for print_to_ggplot")
  expect_error(print_to_ggplot(tfrmt_3,test_data),"param variable required for print_to_ggplot")
  expect_error(print_to_ggplot(tfrmt_4,test_data),"value variable required for print_to_ggplot")


})


test_that("column type has been preserved",{
  # make discrete mock data
  df <- structure(
    list(
      City = structure(
        c(2L,
          3L, 1L),
        .Label = c("Minneapolis", "Phoenix",
                   "Raleigh"),
        class = "factor"
      ),
      January = c(52.1,
                  40.5, 12.2),
      February = c(55.1, 42.2, 16.5),
      March = c(59.7, 49.2, 28.3),
      April = c(67.7,
                59.5, 45.1),
      May = c(76.3, 67.4, 57.1),
      June = c(84.6, 74.4, 66.9),
      July = c(91.2,
               77.5, 71.9),
      August = c(89.1, 76.5,
                 70.2),
      September = c(83.8, 70.6, 60),
      October = c(72.2, 60.2, 50),
      November = c(59.8,
                   50, 32.4),
      December = c(52.5, 41.2,
                   18.6)
    ),
    .Names = c(
      "City",
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
    ),
    class = "data.frame",
    row.names = c(NA,-3L)
  )

  dfm <- df %>%
    pivot_longer(January:December) %>%
    rename("month" = name) %>%
    mutate(month=substr(month,1,3))

  dfm$month <- factor(dfm$month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

  table_data<- dfm %>%
    mutate(param="n") %>%
    rename("label"=City,"column"=month)

  x2<-tfrmt(
    # specify columns in the data
    label = label ,
    column = column,
    param = param,
    value = value) %>%
    print_to_ggplot(table_data)

  # continuous mock data
  risk<-tibble(time=c(rep(c(0,1000,2000,3000),3)),
               label=c(rep("Obs",4),rep("Lev",4),rep("Lev+5FU",4)),
               value=c(630,372,256,11,620,360,266,8,608,425,328,14),
               param=rep("n",12))

  p2<-tfrmt(
    # specify columns in the data
    label = label ,
    column = time,
    param = param,
    value = value) %>%
    print_to_ggplot(risk)

  expect_equal(class(table_data$column),class(x2$data$column))
  expect_equal(class(risk$time),class(p2$data$column))


})


