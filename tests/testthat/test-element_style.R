test_that("element_col - quo/vars/bare", {

  element_l <- element_col(align = "left", col = n_tot)
  element_r <- element_col(align = "right", col = vars(p))
  element_c <- element_col(align = ".", col = c(trt1, trt2))
  element_ts <- element_col(align = ".", col = starts_with("hello"))

  expect_equal(element_l$col, vars(n_tot), ignore_attr = TRUE)
  expect_equal(element_r$col, vars(p), ignore_attr = TRUE)
  expect_equal(element_c$col, vars(trt1, trt2), ignore_attr = TRUE)
  expect_equal(element_ts$col, vars(starts_with("hello")), ignore_attr = TRUE)

})

test_that("element_col - char", {

  element_l <- element_col(align = "left", col = "n_tot")
  element_r <- element_col(align = "right", col = "p")
  element_c <- element_col(align = ".", col = c("trt1", "trt2"))

  expect_equal(element_l$col, vars(n_tot), ignore_attr = TRUE)
  expect_equal(element_r$col, vars(p), ignore_attr = TRUE)
  expect_equal(element_c$col, vars(trt1, trt2), ignore_attr = TRUE)

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
    values = value,
    col_style_plan = col_style_plan(
      # element_col(align = "left", col = vars(one)),
      element_col(align = "right", col = vars(four)),
      element_col(align = c(".", ",", " "), col = vars(two, three))
    )
  )

  dat_aligned_man <- tibble::tribble(
    ~ one, ~ column, ~ value,
    "n (%)"    ,"two"    ," 12 (34%)",
    "mean"     ,"two"    ," 12.3    ",
    "sd"       ,"two"    ,"  4.34   ",
    "median"   ,"two"    ," 14      ",
    "(q1, q3)" ,"two"    ,"(10, 20) ",
    "n (%)"    ,"three"  ," 24 (58%)",
    "mean"     ,"three"  ," 15.4    ",
    "sd"       ,"three"  ,"  8.25   ",
    "median"   ,"three"  ," 16      ",
    "(q1, q3)" ,"three"  ,"(11, 22) ",
    "n (%)"    ,"four"   ,"      ",
    "mean"     ,"four"   ,"<0.001",
    "sd"       ,"four"   ,"      ",
    "median"   ,"four"   ,"  0.05",
    "(q1, q3)" ,"four"   ,"      "
  )

  dat_aligned <- apply_col_style_plan_alignment_values(dat, tfrmt_obj)

  expect_equal(dat_aligned,
               dat_aligned_man)


  plan <- tfrmt(
    label = one,
    column = vars(column),
    values = value,
    col_style_plan = col_style_plan(
      # element_col(align = "left", col = vars(one)),
      element_col(align = "right", col = vars(two, three, four))
    )
  )

  dat_aligned_man <- tibble::tribble(
    ~ one, ~ column, ~ value,
    "n (%)"    ,"two"    ," 12 (34%)",
    "mean"     ,"two"    ,"     12.3",
    "sd"       ,"two"    ,"     4.34",
    "median"   ,"two"    ,"       14",
    "(q1, q3)" ,"two"    ," (10, 20)",
    "n (%)"    ,"three"  ," 24 (58%)",
    "mean"     ,"three"  ,"     15.4",
    "sd"       ,"three"  ,"     8.25",
    "median"   ,"three"  ,"       16",
    "(q1, q3)" ,"three"  ," (11, 22)",
    "n (%)"    ,"four"   ,"      "   ,
    "mean"     ,"four"   ,"<0.001"   ,
    "sd"       ,"four"   ,"      "   ,
    "median"   ,"four"   ,"  0.05"   ,
    "(q1, q3)" ,"four"   ,"      "

  )

  dat_aligned <- apply_col_style_plan_alignment_values(dat, plan)

  expect_equal(dat_aligned,
               dat_aligned_man)

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


  plan <- tfrmt(
    label = one,
    column = vars(column),
    values = value,
    col_style_plan = col_style_plan(
      element_col(align = c(".", ",", " "), col = vars(starts_with("trt"))),
      element_col(align = "right", col = vars(four)))

  )

  dat_aligned_man <- tibble::tribble(
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
    "n (%)"    ,"four"  ,"      "   ,
    "mean"     ,"four"  ,"<0.001"   ,
    "sd"       ,"four"  ,"      "   ,
    "median"   ,"four"  ,"  0.05"   ,
    "(q1, q3)" ,"four"  ,"      "  )

  dat_aligned <- apply_col_style_plan_alignment_values(dat, plan)

  expect_equal(dat_aligned, dat_aligned_man)

  plan <- tfrmt(
    label = one,
    column = vars(column),
    values = value,
    col_style_plan = col_style_plan(
      element_col(align = "right", col = vars(starts_with("trt")))
    ))

  dat_aligned_man <- tibble::tribble(~one      , ~column , ~value ,
                             "n (%)"    ,"trt1"   ," 12 (34%)",
                             "mean"     ,"trt1"   ,"     12.3",
                             "sd"       ,"trt1"   ,"     4.34",
                             "median"   ,"trt1"   ,"       14",
                             "(q1, q3)" ,"trt1"   ," (10, 20)",
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
  dat_aligned <- apply_col_style_plan_alignment_values(dat, plan)

  expect_equal(dat_aligned, dat_aligned_man)

  plan <- tfrmt(
    label = one,
    column = vars(column),
    values = value,
    col_style_plan = col_style_plan(
      element_col(align = "right", col = starts_with("trt"))
    ))

  dat_aligned_man <- tibble::tribble(~one      , ~column , ~value ,
                                     "n (%)"    ,"trt1"   ," 12 (34%)",
                                     "mean"     ,"trt1"   ,"     12.3",
                                     "sd"       ,"trt1"   ,"     4.34",
                                     "median"   ,"trt1"   ,"       14",
                                     "(q1, q3)" ,"trt1"   ," (10, 20)",
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
  dat_aligned <- apply_col_style_plan_alignment_values(dat, plan)

  expect_equal(dat_aligned, dat_aligned_man)

})

test_that("Overlapping element_cols favors last one",{

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
    values = value,
    col_style_plan =  col_style_plan(
      element_col(align = "right", col = vars(starts_with("trt"))),
      element_col(align = c(".",","," "), col = trt1)))

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
  dat_aligned <- apply_col_style_plan_alignment_values(dat, plan)

  expect_equal(dat_aligned, dat_aligned_man)
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

  plan <- tfrmt(
    label = one,
    column = vars(column),
    values = value,
    col_style_plan =  col_style_plan(
      element_col(align = "right", col = vars(starts_with("trt"))),
      element_col(align = c("...",",,,,"," "), col = trt1)))

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
  expect_message(apply_col_style_plan_alignment_values(dat, plan))

  dat_aligned <- suppressMessages(apply_col_style_plan_alignment_values(dat, plan))
  expect_equal(dat_aligned, dat_aligned_man)
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

  plan <- tfrmt(
    label = one,
    column = vars(column),
    values = value,
    col_style_plan =  col_style_plan(
      element_col(align = "right", col = vars(starts_with("trt"))),
      element_col(align = c("2","4"), col = trt1)))

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
  expect_warning(apply_col_style_plan_alignment_values(dat, plan))

  dat_aligned <- suppressWarnings(apply_col_style_plan_alignment_values(dat, plan))
  expect_equal(dat_aligned, dat_aligned_man)
})

test_that("Col width assignment in gt",{

  raw_dat <- tibble::tribble(
           ~one,   ~param, ~column, ~ value,
    "n (%)"    ,      "n",  "trt1",      12,
    "n (%)"    ,    "pct",  "trt1",      34,
    "mean"     ,   "mean",  "trt1",    12.3,
    "sd"       ,     "sd",  "trt1",    4.34,
    "median"   , "median",  "trt1",      14,
    "(q1, q3)" ,     "q1",  "trt1",      10,
    "(q1, q3)" ,     "q3",  "trt1",      20,
    "n (%)"    ,      "n",  "trt2",      24,
    "n (%)"    ,    "pct",  "trt2",      58,
    "mean"     ,   "mean",  "trt2",    15.4,
    "sd"       ,     "sd",  "trt2",    8.25,
    "median"   , "median",  "trt2",      16,
    "(q1, q3)" ,     "q1",  "trt2",      22,
    "(q1, q3)" ,     "q3",  "trt2",      22,
    "mean"     ,   "pval","four"  ,   0.0001
  )

  plan <- tfrmt(
    label = one,
    column = vars(column),
    values = value,
    param = param,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",label_val = ".default",
        frmt_combine("{n} ({pct})",
                     n = frmt("x"),
                     pct = frmt("xx%"))
      ),
      frmt_structure(
        group_val = ".default",label_val = ".default",
        frmt_combine("({q1}, {q3})",
                     q1 = frmt("xx"),
                     q3 = frmt("xx"))
      ),
      frmt_structure(
        group_val = ".default",label_val = ".default",
        pval = frmt_when(
          "<.001" ~ "<.001",
          TRUE ~ frmt("x.xxx")
        )
      ),
      frmt_structure(
        group_val = ".default",label_val = ".default",
        frmt("xx.xx")
      )

    ),
    col_style_plan =  col_style_plan(
      element_col(align = "right", width = 200, col = vars(starts_with("trt"))),
      element_col(align = c("2","4"), col = trt1),
      element_col(width = 100, col = four)
    )
  )

  ## suppressing warning from alignment using multiple values. Not pertinent to this test
  suppressWarnings({
  tfrmt_gt <- print_to_gt(plan, raw_dat)
  })

  expect_equal(
    tfrmt_gt$`_boxhead`[,c("var","column_width")] %>% filter(!var=="..tfrmt_row_grp_lbl"),
    tibble(
      var = c("one","trt1","trt2","four"),
      column_width = list(list(""),list("200px"),list("200px"),list("100px"))
    )
  )

  tfrmt_gt2 <- plan %>%
    tfrmt(
      col_style_plan =  col_style_plan(
        element_col(align = "right", width = 200, col = starts_with("trt")),
        element_col(align = c("2","4"), col = trt1),
        element_col(width = 500, col = c(trt2, one)), # updating trt2 from 200 to 500
        element_col(width = "50%", col = four)
      )) %>%
    print_to_gt(raw_dat) %>%
    ## suppressing warning from alignment using multiple values. Not pertinent to this test
    suppressWarnings()

  expect_equal(
    tfrmt_gt2$`_boxhead`[,c("var","column_width")] %>% filter(!var=="..tfrmt_row_grp_lbl"),
    tibble(
      var = c("one","trt1","trt2","four"),
      column_width = list(list("500px"),list("200px"),list("500px"),list("50%"))
    )
  )

})


