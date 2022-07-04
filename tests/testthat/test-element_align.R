test_that("element_align - quo/vars/bare", {

  element_l <- element_align(align = "left", col = n_tot)
  element_r <- element_align(align = "right", col = vars(p))
  element_c <- element_align(align = ".", col = c(trt1, trt2))

  expect_equal(element_l$col, vars(n_tot), ignore_attr = TRUE)
  expect_equal(element_r$col, vars(p), ignore_attr = TRUE)
  expect_equal(element_c$col, vars(trt1, trt2), ignore_attr = TRUE)

})

test_that("element_align - char", {

  element_l <- element_align(align = "left", col = "n_tot")
  element_r <- element_align(align = "right", col = "p")
  element_c <- element_align(align = ".", col = c("trt1", "trt2"))

  expect_equal(element_l$col, vars(n_tot), ignore_attr = TRUE)
  expect_equal(element_r$col, vars(p), ignore_attr = TRUE)
  expect_equal(element_c$col, vars(trt1, trt2), ignore_attr = TRUE)

})


test_that("left & right align works", {

  vec <- c(" xx.xxx","xx", " x,  x")

  expect_equal(apply_col_align(vec, align = "left"),
               c(" xx.xxx","xx     ", " x,  x "))

  expect_equal(apply_col_align(vec, align = "right"),
               c(" xx.xxx","     xx", "  x,  x"))

})

test_that("decimal align works", {

  vec <- c(" xx.xx", " x, x", "xxx", " x (x.x)")

  expect_equal(apply_col_align(vec, align = c("."," ")),
               c(" xx.xx   ", " x, x    ", "xxx      ", "  x (x.x)"))

  expect_equal(apply_col_align(vec, align = c(".", ",", " ")),
               c(" xx.xx   ", "  x, x   ", "xxx      ", "  x (x.x)"))
})

test_that("error if column doesn't exist", {

  dat <- tibble(
    one = c("n (%)", "mean", "sd", "median", "(q1, q3)"),
    two = c(" 12 (34%)"," 12.3", "  4.34", " 14", "(10, 20)"),
    three = c(" 24 (58%)"," 15.4", "  8.25", " 16", "(11, 22)"),
    four = c("","<0.001","","0.05","")
  )

  plan <- col_align_plan(
    element_align(align = "left", col = "my_var"),
    element_align(align = "right", col = vars(four)),
    element_align(align = c(".", ",", " "), col = vars(two, three))
  )

  expect_error(col_align_all(dat, plan))
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

  plan <- tfrmt(
    label = one,
    column = vars(column),
    values = value,
    col_align_plan = col_align_plan(
      # element_align(align = "left", col = vars(one)),
      element_align(align = "right", col = vars(four)),
      element_align(align = c(".", ",", " "), col = vars(two, three))
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

  dat_aligned <- apply_col_align_plan(dat, plan$col_align_plan, plan$column, plan$values)

  expect_equal(dat_aligned,
               dat_aligned_man)


  plan <- tfrmt(
    label = one,
    column = vars(column),
    values = value,
    col_align_plan = col_align_plan(
      # element_align(align = "left", col = vars(one)),
      element_align(align = "right", col = vars(two, three, four))
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

  dat_aligned <- apply_col_align_plan(dat, plan$col_align_plan, plan$column, plan$values)

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
    col_align_plan = col_align_plan(
      element_align(align = c(".", ",", " "), col = vars(starts_with("trt"))),
      element_align(align = "right", col = vars(four)))

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

  dat_aligned <- apply_col_align_plan(dat, plan$col_align_plan, plan$column, plan$values)

  expect_equal(dat_aligned, dat_aligned_man)

  plan <- tfrmt(
    label = one,
    column = vars(column),
    values = value,
    col_align_plan = col_align_plan(
      element_align(align = "right", col = vars(starts_with("trt")))
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
  dat_aligned <- apply_col_align_plan(dat, plan$col_align_plan, plan$column, plan$values)

  expect_equal(dat_aligned, dat_aligned_man)

})

test_that("Overlapping element_aligns favors last one",{

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
    col_align_plan =  col_align_plan(
      element_align(align = "right", col = vars(starts_with("trt"))),
      element_align(align = c(".",","," "), col = trt1)))

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
  dat_aligned <- apply_col_align_plan(dat, plan$col_align_plan, plan$column, plan$values)

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
    col_align_plan =  col_align_plan(
      element_align(align = "right", col = vars(starts_with("trt"))),
      element_align(align = c("...",",,,,"," "), col = trt1)))

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
  expect_message(apply_col_align_plan(dat, plan$col_align_plan, plan$column, plan$values))

  dat_aligned <- suppressMessages(apply_col_align_plan(dat, plan$col_align_plan, plan$column, plan$values))
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
    col_align_plan =  col_align_plan(
      element_align(align = "right", col = vars(starts_with("trt"))),
      element_align(align = c("2","4"), col = trt1)))

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
  expect_warning(apply_col_align_plan(dat, plan$col_align_plan, plan$column, plan$values))

  dat_aligned <- suppressWarnings(apply_col_align_plan(dat, plan$col_align_plan, plan$column, plan$values))
  expect_equal(dat_aligned, dat_aligned_man)
})
