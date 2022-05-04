test_that("element_align - quo/vars/bare", {

  element <- element_align(left = n_tot,
                           right = vars(p),
                           char = c(trt1, trt2))

  expect_equal(element$left, vars(n_tot), ignore_attr = TRUE) # on hold til quo_get fixed
  expect_equal(element$right, vars(p), ignore_attr = TRUE)
  expect_equal(element$char, vars(trt1, trt2), ignore_attr = TRUE)
})

test_that("element_align - char", {


  element <- element_align(left = "n_tot",
                          right = "p",
                          char = c("trt1", "trt2"))

  expect_equal(element$left, vars(n_tot), ignore_attr = TRUE)
  expect_equal(element$right, vars(p), ignore_attr = TRUE)
  expect_equal(element$char, vars(trt1, trt2), ignore_attr = TRUE)

})

test_that("left & right align works", {

  vec <- c(" xx.xxx","xx", " x,  x")

  expect_equal(col_align_lr(vec, side = "left"),
               c(" xx.xxx","xx     ", " x,  x "))

  expect_equal(col_align_lr(vec, side = "right"),
               c(" xx.xxx","     xx", "  x,  x"))

})

test_that("decimal align works", {

  vec <- c(" xx.xx", " x, x", "xxx", " x (x.x)")

  expect_equal(col_align_char(vec, char_val = c("."," ")),
               c(" xx.xx   ", " x, x    ", "xxx      ", "  x (x.x)"))

  expect_equal(col_align_char(vec, char_val = c(".", ",", " ")),
               c(" xx.xx   ", "  x, x   ", "xxx      ", "  x (x.x)"))
})

test_that("alignment of multiple columns works", {

  dat <- tibble(
    one = c("n (%)", "mean", "sd", "median", "(q1, q3)"),
    two = c(" 12 (34%)"," 12.3", "  4.34", " 14", "(10, 20)"),
    three = c(" 24 (58%)"," 15.4", "  8.25", " 16", "(11, 22)"),
    four = c("","<0.001","","0.05","")
  )

  element <- element_align(left = vars(one),
                      char = vars(two, three),
                      right = vars(four),
                      char_val = c(".", ",", " "))

  expect_equal(col_align_all(dat, element),
               tibble(one =   c("n (%)   ",  "mean    ",  "sd      ",  "median  ",  "(q1, q3)"),
                      two =   c(" 12 (34%)", " 12.3    ", "  4.34   ", " 14      ", "(10, 20) "),
                      three = c(" 24 (58%)", " 15.4    ", "  8.25   ", " 16      ", "(11, 22) "),
                      four =  c("      ",    "<0.001",    "      ",    "  0.05",    "      ")))

  element <- element_align(left = vars(one),
                      right = vars(two, three, four))

  expect_equal(col_align_all(dat, element),
               tibble(one =   c("n (%)   ",  "mean    ",  "sd      ",  "median  ",  "(q1, q3)"),
                      two =   c(" 12 (34%)", "     12.3", "     4.34", "       14", " (10, 20)"),
                      three = c(" 24 (58%)", "     15.4", "     8.25", "       16", " (11, 22)"),
                      four =  c("      ",    "<0.001",    "      ",    "  0.05",    "      ")))

})
