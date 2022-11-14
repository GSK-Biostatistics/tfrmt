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

  attr(dat_aligned, ".footnote_locs") <- NULL
  attr(dat_aligned, ".col_plan_vars") <- NULL

  expect_equal(dat_aligned, dat_aligned_man)

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

