library(tidyr)


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

  dat <- tibble(
    one = c("n (%)", "mean", "sd", "median", "(q1, q3)"),
    two = c(" 12 (34%)"," 12.3", "  4.34", " 14", "(10, 20)"),
    three = c(" 24 (58%)"," 15.4", "  8.25", " 16", "(11, 22)"),
    four = c("","<0.001","","0.05","")
  )

  plan <- col_align_plan(
    element_align(align = "left", col = vars(one)),
    element_align(align = "right", col = vars(four)),
    element_align(align = c(".", ",", " "), col = vars(two, three))
  )

  dat_aligned_man <- tibble(one =   c("n (%)   ",  "mean    ",  "sd      ",  "median  ",  "(q1, q3)"),
                            two =   c(" 12 (34%)", " 12.3    ", "  4.34   ", " 14      ", "(10, 20) "),
                            three = c(" 24 (58%)", " 15.4    ", "  8.25   ", " 16      ", "(11, 22) "),
                            four =  c("      ",    "<0.001",    "      ",    "  0.05",    "      "))

  dat_aligned <- apply_col_align_plan(dat, plan)

  expect_equal(dat_aligned,
               dat_aligned_man)



  plan <- col_align_plan(
    element_align(align = "left", col = vars(one)),
    element_align(align = "right", col = vars(two, three, four))
  )

  dat_aligned_man <- tibble(one =   c("n (%)   ",  "mean    ",  "sd      ",  "median  ",  "(q1, q3)"),
                            two =   c(" 12 (34%)", "     12.3", "     4.34", "       14", " (10, 20)"),
                            three = c(" 24 (58%)", "     15.4", "     8.25", "       16", " (11, 22)"),
                            four =  c("      ",    "<0.001",    "      ",    "  0.05",    "      "))

  dat_aligned <- apply_col_align_plan(dat, plan)

  expect_equal(dat_aligned,
               dat_aligned_man)

})


test_that("tidyselect works", {

  dat <- tibble(
    one = c("n (%)", "mean", "sd", "median", "(q1, q3)"),
    trt1 = c(" 12 (34%)"," 12.3", "  4.34", " 14", "(10, 20)"),
    trt2 = c(" 24 (58%)"," 15.4", "  8.25", " 16", "(11, 22)"),
    four = c("","<0.001","","0.05","")
  )

  plan <- col_align_plan(
    element_align(align = "left", col = vars(one)),
    element_align(align = c(".", ",", " "), col = vars(starts_with("trt"))),
    element_align(align = "right", col = vars(four)))

  dat_aligned_man <- tribble(~one,      ~trt1,        ~trt2,      ~four,
                             "n (%)   " , " 12 (34%)", " 24 (58%)", "      ",
                             "mean    " , " 12.3    ", " 15.4    ", "<0.001",
                             "sd      " , "  4.34   ", "  8.25   ", "      ",
                             "median  " , " 14      ", " 16      ", "  0.05" ,
                             "(q1, q3)" , "(10, 20) ", "(11, 22) ", "      ")
  dat_aligned <- apply_col_align_plan(dat, plan)

  expect_equal(dat_aligned, dat_aligned_man)


  plan <- col_align_plan(
    element_align(align = "right", col = vars(starts_with("trt")))
                  )

  dat_aligned_man <- tribble(~one,      ~trt1,        ~trt2,      ~four,
                             "n (%)"   , " 12 (34%)", " 24 (58%)", "",
                             "mean"    , "     12.3", "     15.4" , "<0.001",
                             "sd"      , "     4.34", "     8.25" , ""   ,
                             "median"  , "       14", "       16" , "0.05"  ,
                             "(q1, q3)", " (10, 20)", " (11, 22)" , ""   )
  dat_aligned <- apply_col_align_plan(dat, plan)

  expect_equal(dat_aligned, dat_aligned_man)

})

test_that("Overlapping element_aligns favors last one",{

  dat <- tibble(
    one = c("n (%)", "mean", "sd", "median", "(q1, q3)"),
    trt1 = c(" 12 (34%)","12.35", " 4.345", "14", "(10, 20)"),
    trt2 = c(" 24 (58%)","15.44", " 8.258", "16", "(11, 22)"),
    four = c("","<0.001","","0.05","")
  )

  plan <- col_align_plan(
    element_align(align = "right", col = vars(starts_with("trt"))),
    element_align(align = c(".",","," "), col = trt1))

  dat_aligned_man <- tribble(
    ~one      , ~trt1      , ~trt2      ,  ~four  ,
    "n (%)"   , " 12 (34%)", " 24 (58%)", ""      ,
    "mean"    , " 12.35   ", "    15.44", "<0.001",
    "sd"      , "  4.345  ", "    8.258", ""      ,
    "median"  , " 14      ", "       16", "0.05"  ,
    "(q1, q3)", "(10, 20) ", " (11, 22)", ""
  )
  dat_aligned <- apply_col_align_plan(dat, plan)

  expect_equal(dat_aligned, dat_aligned_man)
})
