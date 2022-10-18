
test_that("Message is printed if order variables are causing mismatching rows",{
  data <- tibble::tibble(param = c("N","N","n","n","pct","pct"),
                         value = c(111,222,11,22,11,22),
                         order1=c(NA,NA,1,1,1,1),
                         order2=c(NA,NA,1,2,1,2),
                         order3=c(1,2,NA,NA,NA,NA),
                         label=c("Training set","Training set","Sex, n(%)","Sex, n(%)","Sex, n(%)","Sex, n(%)"),
                         column=c("Test","Train","Test","Train","Test","Train"))

  data2 <- data %>%
    rbind(data) %>%
    mutate(group= c(rep("group1",6),rep("group2",6)))

  data3 <- tibble::tibble(param = c("N","N","n","n","pct","pct"),
                          value = c(111,222,11,22,11,22),
                          order1=c(NA,NA,1,1,1,1),
                          order2=c(NA,NA,1,1,1,1),
                          order3=c(1,1,NA,NA,NA,NA),
                          label=c("Training set","Training set","Sex, n(%)","Sex, n(%)","Sex, n(%)","Sex, n(%)"),
                          column=c("Test","Train","Test","Train","Test","Train"))

  tfrmt = tfrmt(
    label = label,
    column = column,
    param = param,
    value=value,
    sorting_cols=c(order1,order2,order3),
    col_plan = col_plan(
      -order1,
      -order2,
      -order3
    ))

  tfrmt2 <- tfrmt(
    group = group,
    label = label,
    column = column,
    param = param,
    value=value,
    sorting_cols=c(order1,order2,order3),
    col_plan = col_plan(
      -order1,
      -order2,
      -order3
    ))

  # use quietly to grab messages from apply_tfrmt
  safe_check_order<-purrr::quietly(check_order_vars)

  expect_equal(
    safe_check_order(data, tfrmt)$messages[1],
    "Note: Some row labels have values printed over more than 1 line.\n This could be due to incorrect sorting variables. Each row in your output table should have only one sorting var combination assigned to it.\n")

  expect_equal(
    safe_check_order(data2, tfrmt2)$messages[1],
    "Note: Some row labels have values printed over more than 1 line.\n This could be due to incorrect sorting variables. Each row in your output table should have only one sorting var combination assigned to it.\n")

  expect_equal(
    safe_check_order(data3, tfrmt)$messages,
    character(0))

})
