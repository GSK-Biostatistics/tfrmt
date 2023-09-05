test_that("col_style_structure - quo/vars/bare", {

  element_l <- col_style_structure(align = "left", col = n_tot)
  element_r <- col_style_structure(align = "right", col = vars(p))
  element_c <- col_style_structure(align = ".", col = c(trt1, trt2))
  element_ts <- col_style_structure(align = ".", col = starts_with("hello"))

  expect_equal(element_l$col, vars(n_tot), ignore_attr = TRUE)
  expect_equal(element_r$col, vars(p), ignore_attr = TRUE)
  expect_equal(element_c$col, vars(trt1, trt2), ignore_attr = TRUE)
  expect_equal(element_ts$col, vars(starts_with("hello")), ignore_attr = TRUE)

})

test_that("col_style_structure - char", {

  element_l <- col_style_structure(align = "left", col = "n_tot")
  element_r <- col_style_structure(align = "right", col = "p")
  element_c <- col_style_structure(align = ".", col = c("trt1", "trt2"))

  expect_equal(element_l$col, vars(n_tot), ignore_attr = TRUE)
  expect_equal(element_r$col, vars(p), ignore_attr = TRUE)
  expect_equal(element_c$col, vars(trt1, trt2), ignore_attr = TRUE)

})

test_that("col_style_structure - errors", {

  expect_error(
    col_style_structure(col = "n_tot"),
    "`align` or `width` must be applied to create this col_style_structure",
    fixed = TRUE
  )

  expect_error(
    col_style_structure(col = "n_tot", width = "INVALID"),
    "`width` must be a value that can be converted into a number greater than 0",
    fixed = TRUE
  )

  expect_error(
    col_style_structure(col = "n_tot", width = "-12345"),
    "`width` must be a valid number greater than 0",
    fixed = TRUE
  )

})

test_that("col_style_structure - advanced", {

  element_1 <- col_style_structure(align = "left", col = c(n_tot,p,test))
  element_2 <- col_style_structure(align = "left", col = c(n_tot,p,"test"))
  element_3 <- col_style_structure(align = "right", col = span_structure(col = test, col2 = value))
  element_4 <- col_style_structure(align = ".", col = c(trt1, span_structure(col = test, col2 = value)))

  expect_equal(element_1$col, list(quo(n_tot),quo(p),quo(test)), ignore_attr = TRUE)
  expect_equal(element_2$col, list(quo(n_tot),quo(p),quo(test)), ignore_attr = TRUE)
  expect_equal(element_3$col, list(list(col = vars(test), col1 = vars(value))), ignore_attr = TRUE)
  expect_equal(element_4$col, list(quo(trt1),list(col = vars(test), col1 = vars(value))), ignore_attr = TRUE)

})

test_that("col_style_plan - basic", {

  csp <- col_style_plan(
    col_style_structure(align = "left", col = "n_tot"),
    col_style_structure(align = "right", col = "p"),
    col_style_structure(align = ".", col = c("trt1", "trt2"))
  )

  expect_equal(length(csp), 3)
  expect_equal(lapply(csp, `[[`, "cols") ,
               list(vars(n_tot), vars(p), vars(trt1, trt2)),
               ignore_attr = TRUE)

})

test_that("col_style_plan - error non-element_col", {

  expect_error({
    col_style_plan(
      element_block(post_space = " "),
      col_style_structure(align = "right", col = "p"),
      col_style_structure(align = ".", col = c("trt1", "trt2"))
    )
  },
  "Entry number 1 is not an object of class `col_style_structure`.",
  fixed = TRUE
  )

})

test_that("left & right align works", {

  vec <- c(" xx.xxx","xx", " x,  x")

  expect_equal(apply_col_alignment(vec, align = "left"),
               c(" xx.xxx","xx     ", " x,  x "))

  expect_equal(apply_col_alignment(vec, align = "right"),
               c(" xx.xxx","     xx", "  x,  x"))

})

test_that("decimal align works", {

  vec <- c(" xx.xx", " x, x", "xxx", " x (x.x)")

  expect_equal(apply_col_alignment(vec, align = c("."," ")),
               c(" xx.xx   ", " x, x    ", "xxx      ", "  x (x.x)"))

  expect_equal(apply_col_alignment(vec, align = c(".", ",", " ")),
               c(" xx.xx   ", "  x, x   ", "xxx      ", "  x (x.x)"))
})

test_that("alignment of multiple columns works", {

  dat <- tibble::tribble(
    ~ one, ~ column, ~ value,
    "n (%)",    "two"    ," 12 (34%)",
    "n (%)",    "three"  ," 24 (58%)",
    "n (%)",    "four"   ,""         ,
    "mean",     "two"    ," 12.3"    ,
    "mean",     "three"  ," 15.4"    ,
    "mean",     "four"   ,"<0.001"   ,
    "sd",       "two"    ,"  4.34"   ,
    "sd",       "three"  ,"  8.25"   ,
    "sd",       "four"   ,""         ,
    "median",   "two"    ," 14"      ,
    "median",   "three"  ," 16"      ,
    "median",   "four"   ,"0.05"     ,
    "(q1, q3)", "two"    ,"(10, 20)" ,
    "(q1, q3)", "three"  ,"(11, 22)" ,
    "(q1, q3)", "four"   ,""   )

  tfrmt_obj <- tfrmt(
    label = one,
    column = vars(column),
    value = value,
    col_style_plan = col_style_plan(
      col_style_structure(align = "left", col = vars(one)),
      col_style_structure(align = "right", col = vars(four)),
      col_style_structure(align = c(".", ",", " "), col = vars(two, three))
    )
  )

  dat_aligned_man <- tibble::tribble(
    ~ one, ~ column, ~ value,
    "n (%)   ","two"    ," 12 (34%)",
    "mean    ","two"    ," 12.3    ",
    "sd      ","two"    ,"  4.34   ",
    "median  ","two"    ," 14      ",
    "(q1, q3)","two"    ,"(10, 20) ",
    "n (%)   ","three"  ," 24 (58%)",
    "mean    ","three"  ," 15.4    ",
    "sd      ","three"  ,"  8.25   ",
    "median  ","three"  ," 16      ",
    "(q1, q3)","three"  ,"(11, 22) ",
    "n (%)   ","four"   ,"      ",
    "mean    ","four"   ,"<0.001",
    "sd      ","four"   ,"      ",
    "median  ","four"   ,"  0.05",
    "(q1, q3)","four"   ,"      "
  )

  dat_aligned <- dat %>%
    pivot_wider(
      names_from = column,
      values_from = value
      ) %>%
    apply_col_style_plan(tfrmt_obj)

  expect_equal(dat_aligned,
               dat_aligned_man %>%
                 pivot_wider(
                   names_from = column,
                   values_from = value
                 ) )


  plan <- tfrmt(
    label = one,
    column = vars(column),
    value = value,
    col_style_plan = col_style_plan(
      col_style_structure(align = "left", col = vars(one)),
      col_style_structure(align = "right", col = vars(two, three, four))
    )
  )

  dat_aligned_man <- tibble::tribble(
    ~ one, ~ column, ~ value,
    "n (%)   "  ,"two"    ," 12 (34%)",
    "mean    "  ,"two"    ,"     12.3",
    "sd      "  ,"two"    ,"     4.34",
    "median  "  ,"two"    ,"       14",
    "(q1, q3)"  ,"two"    ," (10, 20)",
    "n (%)   "  ,"three"  ," 24 (58%)",
    "mean    "  ,"three"  ,"     15.4",
    "sd      "  ,"three"  ,"     8.25",
    "median  "  ,"three"  ,"       16",
    "(q1, q3)"  ,"three"  ," (11, 22)",
    "n (%)   "  ,"four"   ,"      "   ,
    "mean    "  ,"four"   ,"<0.001"   ,
    "sd      "  ,"four"   ,"      "   ,
    "median  "  ,"four"   ,"  0.05"   ,
    "(q1, q3)"  ,"four"   ,"      "
  )

  dat_aligned <- dat  %>%
    pivot_wider(
      names_from = column,
      values_from = value
    ) %>%
    apply_col_style_plan( plan)

  expect_equal(dat_aligned,
               dat_aligned_man %>%
                 pivot_wider(
                   names_from = column,
                   values_from = value
                 ))


})


test_that("tidyselect works", {

  dat <- tibble::tribble(
    ~one      , ~column , ~ value,
    "n (%)"    ,"trt1" ," 12 (34%)",
    "n (%)"    ,"trt2" ," 24 (58%)",
    "n (%)"    ,"four" , ""         ,
    "mean"     ,"trt1" ," 12.3"    ,
    "mean"     ,"trt2" , " 15.4"    ,
    "mean"     ,"four" ,"<0.001"   ,
    "sd"       ,"trt1" , "  4.34"   ,
    "sd"       ,"trt2" ,"  8.25"   ,
    "sd"       ,"four" , ""         ,
    "median"   ,"trt1" ," 14"      ,
    "median"   ,"trt2" , " 16"      ,
    "median"   ,"four" ,"0.05"     ,
    "(q1, q3)" ,"trt1" , "(10, 20)" ,
    "(q1, q3)" ,"trt2" ,"(11, 22)" ,
    "(q1, q3)" ,"four" , ""         )


  starts_with_align_dot_plan <- tfrmt(
    label = one,
    column = vars(column),
    value = value,
    col_style_plan = col_style_plan(
      col_style_structure(align = c(".", ",", " "), col = vars(starts_with("trt"))),
      col_style_structure(align = "right", col = vars(four)))

  )

  dat_aligned_man <- tibble(
    one = c("n (%)", "mean", "sd", "median", "(q1, q3)"),
    trt1 = c(" 12 (34%)", " 12.3    ", "  4.34   ", " 14      ", "(10, 20) "),
    trt2 = c(" 24 (58%)", " 15.4    ", "  8.25   ", " 16      ", "(11, 22) "),
    four = c("      ", "<0.001", "      ", "  0.05", "      ")
    )

  dat_aligned <- dat %>%
    pivot_wider(
      names_from = column,
      values_from = value
      ) %>%
    apply_col_style_plan( starts_with_align_dot_plan)

  expect_equal(dat_aligned, dat_aligned_man)

  vars_starts_with_plan <- tfrmt(
    label = one,
    column = vars(column),
    value = value,
    col_style_plan = col_style_plan(
      col_style_structure(align = "right", col = vars(starts_with("trt")))
    ))

  dat_aligned_man <- tibble(
    one = c("n (%)", "mean", "sd", "median", "(q1, q3)"),
    trt1 = c(" 12 (34%)", "     12.3", "     4.34", "       14", " (10, 20)"),
    trt2 = c(" 24 (58%)", "     15.4", "     8.25", "       16", " (11, 22)"),
    four = c("", "<0.001", "", "0.05", "")
  )

  dat_aligned <-  dat %>%
    pivot_wider(
      names_from = column,
      values_from = value
    ) %>%
    apply_col_style_plan( vars_starts_with_plan)

  expect_equal(dat_aligned, dat_aligned_man)

  starts_with_plan <- tfrmt(
    label = one,
    column = vars(column),
    value = value,
    col_style_plan = col_style_plan(
      col_style_structure(align = "right", col = starts_with("trt"))
    ))

  dat_aligned <-  dat %>%
    pivot_wider(
      names_from = column,
      values_from = value
    ) %>%
    apply_col_style_plan( starts_with_plan)

  expect_equal(dat_aligned, dat_aligned_man)

  plan_everything <- tfrmt(
    label = one,
    column = vars(column),
    value = value,
    col_style_plan = col_style_plan(
      col_style_structure(align = "right", col = everything())
    ))

  dat_aligned_man <- tibble(
    one = c("   n (%)", "    mean", "      sd", "  median", "(q1, q3)"),
    trt1 = c(" 12 (34%)", "     12.3", "     4.34", "       14", " (10, 20)"),
    trt2 = c(" 24 (58%)", "     15.4", "     8.25", "       16", " (11, 22)"),
    four = c("      ", "<0.001", "      ", "  0.05", "      ")
  )

  dat_aligned <-  dat %>%
    pivot_wider(
      names_from = column,
      values_from = value
    ) %>%
    apply_col_style_plan( plan_everything, col_plan_vars = vars(one, trt1, trt2, four))

  expect_equal(dat_aligned, dat_aligned_man)

})

test_that("span_structure works", {

  dat <- tibble::tribble(
    ~one       ,  ~ span_col, ~column ,  ~ value,
    "n (%)"    , "Test Span1",  "trt1" , " 12 (34%)",
    "n (%)"    , "Test Span2",  "trt2" , " 24 (58%)",
    "n (%)"    ,          NA,  "four" , ""         ,
    "mean"     , "Test Span1",  "trt1" , " 12.3"    ,
    "mean"     , "Test Span2",  "trt2" , " 15.4"    ,
    "mean"     ,          NA,  "four" , "<0.001"   ,
    "sd"       , "Test Span1",  "trt1" , "  4.34"   ,
    "sd"       , "Test Span2",  "trt2" , "  8.25"   ,
    "sd"       ,          NA,  "four" , ""         ,
    "median"   , "Test Span1",  "trt1" , " 14"      ,
    "median"   , "Test Span2",  "trt2" , " 16"      ,
    "median"   ,          NA,  "four" , "0.05"     ,
    "(q1, q3)" , "Test Span1",  "trt1" , "(10, 20)" ,
    "(q1, q3)" , "Test Span2",  "trt2" , "(11, 22)" ,
    "(q1, q3)" ,          NA,  "four" , ""         )


  plan <- tfrmt(
    label = one,
    column = vars(span_col, column),
    value = value,
    col_style_plan = col_style_plan(
      col_style_structure(align = c(".", ",", " "), col = span_structure(span_col = "Test Span1")),
      col_style_structure(align = "right", col = vars(four)))
  )

  dat_aligned_man <- tibble(
    one = c("n (%)", "mean", "sd", "median", "(q1, q3)"),
    `Test Span1___tlang_delim___trt1` = c(" 12 (34%)", " 12.3    ", "  4.34   ", " 14      ", "(10, 20) "),
    `Test Span2___tlang_delim___trt2` = c(" 24 (58%)", " 15.4", "  8.25", " 16", "(11, 22)"),
    four = c("      ", "<0.001", "      ", "  0.05", "      ")
  )

  dat_aligned <- dat %>%
    pivot_wider(
      names_from = c(span_col,column),
      names_sep = .tlang_delim,
      values_from = value,
    ) %>%
    clean_spanning_col_names() %>%
    apply_col_style_plan( plan)

  expect_equal(dat_aligned, dat_aligned_man)



})

test_that("span_structure works on a renamed column", {

  dat <- tibble::tribble(
    ~one       ,   ~ span_col,  ~column,  ~ value,    ~param,
    "n (%)"    , "Test Span1",   "trt1", " 12 (34%)", "param",
    "n (%)"    , "Test Span2",   "trt2", " 24 (58%)", "param",
    "n (%)"    ,           NA,   "four", ""         , "param",
    "mean"     , "Test Span1",   "trt1", " 12.3"    , "param",
    "mean"     , "Test Span2",   "trt2", " 15.4"    , "param",
    "mean"     ,           NA,   "four", "<0.001"   , "param",
    "sd"       , "Test Span1",   "trt1", "  4.34"   , "param",
    "sd"       , "Test Span2",   "trt2", "  8.25"   , "param",
    "sd"       ,           NA,   "four", ""         , "param",
    "median"   , "Test Span1",   "trt1", " 14"      , "param",
    "median"   , "Test Span2",   "trt2", " 16"      , "param",
    "median"   ,           NA,   "four", "0.05"     , "param",
    "(q1, q3)" , "Test Span1",   "trt1", "(10, 20)" , "param",
    "(q1, q3)" , "Test Span2",   "trt2", "(11, 22)" , "param",
    "(q1, q3)" ,           NA,   "four", "",  "param"
    )


  plan <- tfrmt(
    label = one,
    column = vars(span_col, column),
    value = value,
    param = param,
    col_plan = col_plan(
      one,
      span_structure(
        span_col = `Test Span1`,
        column = trt1
      ),
      span_structure(
        span_col = c("New Test Span2" = "Test Span2"),
        column = trt2
      ),
      four
    ),
    col_style_plan = col_style_plan(
      col_style_structure(align = c(".", ",", " "), col = span_structure(span_col = "Test Span2")),
      col_style_structure(align = "right", col = vars(four))
    )
  )

  dat_aligned_man <- tibble(
    one = c("n (%)", "mean", "sd", "median", "(q1, q3)"),
    `Test Span1___tlang_delim___trt1` = c(" 12 (34%)", " 12.3", "  4.34", " 14", "(10, 20)"),
    `New Test Span2___tlang_delim___trt2` = c(" 24 (58%)", " 15.4    ", "  8.25   ", " 16      ", "(11, 22) "),
    four = c("      ", "<0.001", "      ", "  0.05", "      ")
  )

  suppressMessages({
  dat_aligned <- apply_tfrmt(dat, plan, mock = FALSE) %>%
    tibble::as_tibble()
  })

  expect_equal(dat_aligned, dat_aligned_man, ignore_attr = TRUE)

})

test_that("Overlapping col_style_structure favors last one",{

  dat <- tibble::tribble(
    ~one      , ~column , ~ value,
    "n (%)"    ,"trt1"  ," 12 (34%)",
    "mean"     ,"trt1"  ," 12.3    ",
    "sd"       ,"trt1"  ,"  4.34   ",
    "median"   ,"trt1"  ," 14      ",
    "(q1, q3)" ,"trt1"  ,"(10, 20) ",
    "n (%)"    ,"trt2"  ," 24 (58%)",
    "mean"     ,"trt2"  ," 15.4    ",
    "sd"       ,"trt2"  ,"  8.25   ",
    "median"   ,"trt2"  ," 16      ",
    "(q1, q3)" ,"trt2"  ,"(11, 22) ",
    "n (%)"    ,"four"  ,""         ,
    "mean"     ,"four"  ,"<0.001"   ,
    "sd"       ,"four"  ,""         ,
    "median"   ,"four"  ,"0.05"   ,
    "(q1, q3)" ,"four"  ,""  )

  plan <- tfrmt(
    label = one,
    column = vars(column),
    value = value,
    col_style_plan =  col_style_plan(
      col_style_structure(align = "right", col = vars(starts_with("trt"))),
      col_style_structure(align = c(".",","," "), col = trt1)))

  dat_aligned_man <- tibble::tribble(~one      , ~column , ~value ,
                             "n (%)" ,"trt1"   ," 12 (34%)",
                             "mean" ,"trt1"   ," 12.3    ",
                             "sd" ,"trt1"   ,"  4.34   ",
                             "median" ,"trt1"   ," 14      ",
                             "(q1, q3)" ,"trt1"   ,"(10, 20) ",
                             "n (%)" ,"trt2"   ," 24 (58%)",
                             "mean" ,"trt2"   ,"     15.4",
                             "sd" ,"trt2"   ,"     8.25",
                             "median" ,"trt2"   ,"       16",
                             "(q1, q3)" ,"trt2"   ," (11, 22)",
                             "n (%)" ,"four"   ,""         ,
                             "mean" ,"four"   ,"<0.001"   ,
                             "sd" ,"four"   ,""         ,
                             "median" ,"four"   ,"0.05"     ,
                             "(q1, q3)" ,"four"   ,""         )

  dat_aligned <- dat %>%
    pivot_wider(
      names_from = column,
      values_from = value
    ) %>%
    apply_col_style_plan(plan)

  expect_equal(dat_aligned, dat_aligned_man%>%
                 pivot_wider(
                   names_from = column,
                   values_from = value
                 ) )
})




test_that("Align strings >1 in length",{

  dat <- tibble::tribble(
    ~one      , ~column , ~ value,
    "n (%)"    ,"trt1"  ," 12 (34%)",
    "mean"     ,"trt1"  ," 12.3    ",
    "sd"       ,"trt1"  ,"  4.34   ",
    "median"   ,"trt1"  ," 14      ",
    "(q1, q3)" ,"trt1"  ,"(10, 20) ",
    "n (%)"    ,"trt2"  ," 24 (58%)",
    "mean"     ,"trt2"  ," 15.4    ",
    "sd"       ,"trt2"  ,"  8.25   ",
    "median"   ,"trt2"  ," 16      ",
    "(q1, q3)" ,"trt2"  ,"(11, 22) ",
    "n (%)"    ,"four"  ,""         ,
    "mean"     ,"four"  ,"<0.001"   ,
    "sd"       ,"four"  ,""         ,
    "median"   ,"four"  ,"0.05"   ,
    "(q1, q3)" ,"four"  ,""  )

  col_style_structure_message <- capture_messages({
   plan <- tfrmt(
    label = one,
    column = vars(column),
    value = value,
    col_style_plan =  col_style_plan(
      col_style_structure(align = "right", col = vars(starts_with("trt"))),
      col_style_structure(align = c("...",",,,,"," "), col = trt1)))
  })

  expect_true(
    !is_empty(col_style_structure_message)
  )

  dat_aligned_man <- tibble::tribble(~one      , ~column , ~value ,
                             "n (%)"    ,"trt1"   ," 12 (34%)",
                             "mean"     ,"trt1"   ," 12.3    ",
                             "sd"       ,"trt1"   ,"  4.34   ",
                             "median"   ,"trt1"   ," 14      ",
                             "(q1, q3)" ,"trt1"   ,"(10, 20) ",
                             "n (%)"    ,"trt2"   ," 24 (58%)",
                             "mean"     ,"trt2"   ,"     15.4",
                             "sd"       ,"trt2"   ,"     8.25",
                             "median"   ,"trt2"   ,"       16",
                             "(q1, q3)" ,"trt2"   ," (11, 22)",
                             "n (%)"    ,"four"   ,""         ,
                             "mean"     ,"four"   ,"<0.001"   ,
                             "sd"       ,"four"   ,""         ,
                             "median"   ,"four"   ,"0.05"     ,
                             "(q1, q3)" ,"four"   ,""         )

  # informs user
  dat_aligned <- dat %>%
    pivot_wider(
      names_from = column,
      values_from = value
    ) %>%
    apply_col_style_plan(plan)

  expect_equal(dat_aligned, dat_aligned_man%>%
                 pivot_wider(
                   names_from = column,
                   values_from = value
                 ))

})



test_that("Alphanumeric align string supplied",{

  dat <- tibble::tribble(
    ~one      , ~column , ~ value,
    "n (%)"    ,"trt1"  ," 12 (34%)",
    "mean"     ,"trt1"  ," 12.3    ",
    "sd"       ,"trt1"  ,"  4.34   ",
    "median"   ,"trt1"  ," 14      ",
    "(q1, q3)" ,"trt1"  ,"(10, 20) ",
    "n (%)"    ,"trt2"  ," 24 (58%)",
    "mean"     ,"trt2"  ," 15.4    ",
    "sd"       ,"trt2"  ,"  8.25   ",
    "median"   ,"trt2"  ," 16      ",
    "(q1, q3)" ,"trt2"  ,"(11, 22) ",
    "n (%)"    ,"four"  ,""         ,
    "mean"     ,"four"  ,"<0.001"   ,
    "sd"       ,"four"  ,""         ,
    "median"   ,"four"  ,"0.05"   ,
    "(q1, q3)" ,"four"  ,""  )

  col_style_structure_message <- capture_messages({

  plan <- tfrmt(
    label = one,
    column = vars(column),
    value = value,
    col_style_plan =  col_style_plan(
      col_style_structure(align = "right", col = vars(starts_with("trt"))),
      col_style_structure(align = c("2","4"), col = trt1)))

  })

  expect_true(
    !is_empty(col_style_structure_message)
  )

  dat_aligned_man <- tibble::tribble(~one      , ~column , ~value ,
                             "n (%)"    ,"trt1"   ,"    12 (34%)",
                             "mean"     ,"trt1"   ,"    12.3    ",
                             "sd"       ,"trt1"   ,"     4.34   ",
                             "median"   ,"trt1"   ,"    14      ",
                             "(q1, q3)" ,"trt1"   ,"(10, 20)    ",
                             "n (%)"    ,"trt2"   ," 24 (58%)",
                             "mean"     ,"trt2"   ,"     15.4",
                             "sd"       ,"trt2"   ,"     8.25",
                             "median"   ,"trt2"   ,"       16",
                             "(q1, q3)" ,"trt2"   ," (11, 22)",
                             "n (%)"    ,"four"   ,""         ,
                             "mean"     ,"four"   ,"<0.001"   ,
                             "sd"       ,"four"   ,""         ,
                             "median"   ,"four"   ,"0.05"     ,
                             "(q1, q3)" ,"four"   ,""         )

  # informs user
  dat_aligned <- dat %>%
    pivot_wider(
      names_from = column,
      values_from = value
    ) %>%
    apply_col_style_plan(plan)

  expect_equal(dat_aligned, dat_aligned_man %>%
                 pivot_wider(
                   names_from = column,
                   values_from = value
                 ))
})


test_that("multi-positional alignment", {

  # 1 position align
  vec <- c(" xx.x (xx.x)", " xx, xx", "xx (xx - xx)", " xx (xx %)")
  vec_aligned <- apply_col_alignment(vec,
                                     align = c(" xx.x |(xx.x)",
                                               " xx, |xx",
                                               "xx |(xx - xx)",
                                               " xx |(xx %)"),
                                     type = "pos")

  expect_equal(vec_aligned,
               c("xx.x (xx.x)   ",
                 " xx, xx       ",
                 "  xx (xx - xx)",
                 "  xx (xx %)   "))

  # 3 position align
  vec <- c(" xx.x", " x, x", "xx (xx - xx)", " x (x.x, x.x)")
  vec_aligned <- apply_col_alignment(vec,
                                     align = c(" xx|.x",
                                               " x|, |x",
                                               "xx| |(xx - |xx)",
                                               " x| |(x.x, |x.x)"),
                                     type = "pos")

  expect_equal(vec_aligned,
               c("xx.x          ",
                 " x, x         ",
                 "xx  (xx - xx) ",
                 " x  (x.x, x.x)"))

  # positional alignment not covering all cases - will treat all not covered as: "xxx|" (last char to be aligned with first position)
  vec <- c(" xx.x", " x, x", "xx (xx - xx)", " x (x.x, x.x)")
  vec_aligned <- suppressMessages({
    apply_col_alignment(vec,
                        align = c(" xx|.x",
                                  " x|, |x",
                                  "xx| |(xx - |xx)"),
                        type = "pos")
  })
  expect_equal(vec_aligned,
               c("          xx.x         ",
                 "           x, x        ",
                 "          xx  (xx - xx)",
                 "x (x.x, x.x)           ")
  )

  # align on 2 positions - full plan
  dat <- tibble::tribble(
    ~ lbl, ~ col, ~ val,
    "18-65"	, "trt", "15 (23.4 %)",
    "18-65" , "placebo", "5 (3.8 %)",
    "66-82" , "trt",	"34 (33.5 %)",
    "66-82" , "placebo",	"48 (82.2%)"
  )

  tfrmt_obj <- tfrmt(
    label = "lbl",
    column = "col",
    value = "val",
    col_style_plan = col_style_plan(
      col_style_structure(align = c("xx| (xx|.x %)",
                                    "xx| (xx|.x%)",
                                    "x| (x|.x %)"),
                          type = "pos",
                          col = vars(trt, placebo))
    )
  )

  dat_aligned <- dat %>%
    pivot_wider(
      names_from = col,
      values_from = val
    ) %>%
    apply_col_style_plan(tfrmt_obj)

  dat_aligned_man <- tibble::tribble(
    ~ lbl    , ~trt    , ~placebo,
    "18-65", "15 (23.4 %)", " 5  (3.8 %)",
    "66-82", "34 (33.5 %)", "48 (82.2%) ",
  )

  expect_equal(dat_aligned, dat_aligned_man)

  # align on 3 positions - full plan

  dat <- tibble::tribble(
    ~ one, ~ column, ~ value,
    "lbl1", "two"    ," 12 (34%)",
    "lbl1", "three"  ," 24 (58%)",
    "lbl2", "two"    ," 12.3 (2.3 - 15.3)",
    "lbl2", "three"  ," 15.4 (3.4 - 17.6)",
    "lbl3", "two"    ,"  4.34"   ,
    "lbl3", "three"  ,"  8.25"   ,
    "lbl4", "two"    ,"(10, 20)" ,
    "lbl4", "three"  ,"(11, 22)" )

  tfrmt_obj <- tfrmt(
    label = one,
    column = vars(column),
    value = value,
    col_style_plan = col_style_plan(
      col_style_structure(align = c(" xx| |(xx%)",
                                    " xx|.x |(x.x - |xx.x)",
                                    "||(xx, |xx)",
                                    "x|.xx"),
                          type = "pos",
                          col = vars(two, three))
    )
  )

  dat_aligned <- dat %>%
    pivot_wider(
      names_from = column,
      values_from = value
    ) %>%
    apply_col_style_plan(tfrmt_obj)

  dat_aligned_man <- tibble::tribble(
    ~one    , ~two               ,~three           ,
    "lbl1",  "12   (34%)       ", "24   (58%)       ",
    "lbl2",  "12.3 (2.3 - 15.3)", "15.4 (3.4 - 17.6)",
    "lbl3",  " 4.34            ", " 8.25            ",
    "lbl4",  "     (10,   20)  ", "     (11,   22)  "
  )

  expect_equal(dat_aligned, dat_aligned_man)


})

test_that("multi-positional alignment detects inadequate inputs", {

  dat <- tibble::tribble(
    ~ lbl, ~ col, ~ val,
    "lbl1", "two"    ," 12 (34%)",
    "lbl1", "three"  ," 24 (58%)",
    "lbl2", "two"    ," 12.3 (2.3)",
    "lbl2", "three"  ," 15.4 (5.4)")

  tfrmt_obj <- tfrmt(
    label = lbl,
    column = vars(col),
    value = val,
    col_style_plan = col_style_plan(
      col_style_structure(align = c(" xx |(xx)"),
                          type = "pos",
                          col = vars(two, three))
    )
  )
  dat_wide <- dat %>%
    pivot_wider(
      names_from = col,
      values_from = val
    )

  msgs <- capture_messages(apply_col_style_plan(dat_wide, tfrmt_obj))

  expect_equal(msgs, c("`align` input for `type`=\"pos\" in col_style_structure does not cover all possible values. Some cells may not be aligned.\n",
                       "`align` input for `type`=\"pos\" in col_style_structure does not cover all possible values. Some cells may not be aligned.\n"))


  dat <- tibble::tribble(
    ~ lbl, ~ col, ~ val,
    "lbl1", "two"    ," 12(34%)",
    "lbl1", "three"  ," 24(58%)",
    "lbl2", "two"    ," 12.3(12.3%)",
    "lbl2", "three"  ," 15.4(15.4%)")

  tfrmt_obj <- tfrmt(
    label = lbl,
    column = vars(col),
    value = val,
    col_style_plan = col_style_plan(
      col_style_structure(align = c(" xx|(xx|%)",
                                    " xx|.x(xx|.x%)"),
                          type = "pos",
                          col = vars(two, three))
    )
  )
  dat_wide <- dat %>%
    pivot_wider(
      names_from = col,
      values_from = val
    )

  msgs <- capture_messages(apply_col_style_plan(dat_wide, tfrmt_obj))

  expect_equal(msgs, c("Unable to complete positional alignment in col_style_structure due to lack of whitespace available formatted value\n",
                       "Unable to complete positional alignment in col_style_structure due to lack of whitespace available formatted value\n"))
})


test_that("helper for constructing positional alignment works",{

  dat <- tibble::tribble(
    ~ lbl, ~ col, ~param, ~ val,
    "lbl1", "two"    ,"n",  12,
    "lbl1", "two"    ,"pct", 33.9999,
    "lbl1", "three"  ,"n",  24,
    "lbl1", "three"  ,"pct", 58.222,
    "lbl2", "two"    ,"n",  12,
    "lbl2", "two"    ,"pct", 12.4353,
    "lbl2", "three"  ,"n",  15,
    "lbl2", "three"  ,"pct", 15.354)

  tfrmt_obj <- tfrmt(
    label = lbl,
    column = vars(col),
    param = param,
    value = val,
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = "lbl1",
                     frmt_combine("{n} ({pct}%)",
                                  n = frmt("x"),
                                  pct = frmt("xx"))),
      frmt_structure(group_val = ".default", label_val = "lbl2",
                     frmt_combine("{n} ({pct}%)",
                                  n = frmt("xxx"),
                                  pct = frmt("xx.x")))
    )
  )

  expect_equal(
    display_val_frmts(tfrmt_obj, .data = dat, col = vars(everything())),
    "c(\"xx (xx%)\",
  \" xx (xx.x%)\")"
  )
})
