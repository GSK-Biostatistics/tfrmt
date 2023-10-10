
test_that("Page plan with defined split", {

  df <- tibble::tribble(
    ~ grp, ~ lbl, ~ prm, ~ trt,
    "A"  ,  "a" , "n"  , 22,
    "A"  ,  "b" , "n"  , 11,
    "B"  ,  "a" , "n"  , 24,
    "B"  ,  "b" , "n"  , 55,
    "C"  ,  "a" , "n"  , 12,
    "C"  ,  "b" , "n"  , 19,
  )
  my_page_plan <- page_plan(
    page_structure(group_val = "A")
  )

  auto_split <- apply_page_plan(df, my_page_plan, vars(grp), quo(lbl))
  man_split <- list(
    tibble::tribble(
      ~ grp, ~ lbl, ~ prm, ~ trt,
      "A"  ,  "a" , "n"  , 22,
      "A"  ,  "b" , "n"  , 11
    ),
    tibble::tribble(
      ~ grp, ~ lbl, ~ prm, ~ trt,
      "B"  ,  "a" , "n"  , 24,
      "B"  ,  "b" , "n"  , 55,
      "C"  ,  "a" , "n"  , 12,
      "C"  ,  "b" , "n"  , 19,
    )
  )

  expect_equal(auto_split, man_split)
})



test_that("Page plan with grouped split", {

  # single grouping var
  df <- tibble::tribble(
    ~ grp, ~ lbl, ~ prm, ~ trt,
    "A"  ,  "a" , "n"  , 22,
    "A"  ,  "b" , "n"  , 11,
    "B"  ,  "a" , "n"  , 24,
    "B"  ,  "b" , "n"  , 55,
    "C"  ,  "a" , "n"  , 12,
    "C"  ,  "b" , "n"  , 19,
  )
  my_page_plan <- page_plan(
    page_structure(group_val = ".default")
  )

  auto_split <- apply_page_plan(df, my_page_plan, vars(grp), quo(lbl))
  man_split <- list(
    tibble::tribble(
      ~ grp, ~ lbl, ~ prm, ~ trt,
      "A"  ,  "a" , "n"  , 22,
      "A"  ,  "b" , "n"  , 11
    ),
    tibble::tribble(
      ~ grp, ~ lbl, ~ prm, ~ trt,
      "B"  ,  "a" , "n"  , 24,
      "B"  ,  "b" , "n"  , 55
    ),
    tibble::tribble(
      ~ grp, ~ lbl, ~ prm, ~ trt,
      "C"  ,  "a" , "n"  , 12,
      "C"  ,  "b" , "n"  , 19,
    )
  )

  expect_equal(auto_split, man_split, ignore_attr = c(".page_note", ".page_grp_vars"))

  expect_equal(
    map_chr(auto_split, ~ attr(.x, ".page_note")),
    c("grp: A","grp: B", "grp: C")
  )
  expect_equal(
    attr(auto_split, ".page_grp_vars"),
    "grp"
  )

  # multi grouping vars - 1 selected
  df <- tibble::tribble(
    ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
    "A"  ,  "a" ,   "n"  , "n"  , 22,
    "A"  ,  "b" ,   "n"  , "n"  , 11,
    "B"  ,  "a" ,   "n"  , "n"  , 24,
    "B"  ,  "b" ,   "n"  , "n"  , 55,
    "C"  ,  "a" ,   "n"  , "n"  , 12,
    "C"  ,  "b" ,   "n"  , "n"  , 19,
  )
  my_page_plan <- page_plan(
    page_structure(group_val = list(grp2 = ".default"))
  )

  auto_split <- apply_page_plan(df, my_page_plan, vars(grp1, grp2), quo(lbl))
  man_split <- list(
    tibble::tribble(
      ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
      "A"  ,  "a" ,   "n"  , "n"  , 22,
      "B"  ,  "a" ,   "n"  , "n"  , 24,
      "C"  ,  "a" ,   "n"  , "n"  , 12
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
      "A"  ,  "b" ,   "n"  , "n"  , 11,
      "B"  ,  "b" ,   "n"  , "n"  , 55,
      "C"  ,  "b" ,   "n"  , "n"  , 19,
    )
  )

  expect_equal(auto_split, man_split, ignore_attr = c(".page_note", ".page_grp_vars"))

  expect_equal(
    map_chr(auto_split, ~ attr(.x, ".page_note")),
    c("grp2: a","grp2: b")
  )
  expect_equal(
    attr(auto_split, ".page_grp_vars"),
    "grp2"
  )

  # multi grouping vars - 2 selected
  df <- tibble::tribble(
    ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
    "A"  ,  "a" ,   "n"  , "n"  , 22,
    "A"  ,  "b" ,   "n"  , "n"  , 11,
    "B"  ,  "a" ,   "n"  , "n"  , 24,
    "B"  ,  "b" ,   "n"  , "n"  , 55
  )
  my_page_plan <- page_plan(
    page_structure(group_val = ".default")
  )

  auto_split <- apply_page_plan(df, my_page_plan, vars(grp1, grp2), quo(lbl))

  man_split <- list(
    tibble::tribble(
      ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
      "A"  ,  "a" ,   "n"  , "n"  , 22
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
      "A"  ,  "b" ,   "n"  , "n"  , 11
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
      "B"  ,  "a" ,   "n"  , "n"  , 24
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
      "B"  ,  "b" ,   "n"  , "n"  , 55
    )
  )
  expect_equal(auto_split, man_split, ignore_attr = c(".page_note", ".page_grp_vars"))

  expect_equal(
    map_chr(auto_split, ~ attr(.x, ".page_note")),
    c("grp1: A,\ngrp2: a", "grp1: A,\ngrp2: b", "grp1: B,\ngrp2: a", "grp1: B,\ngrp2: b")
  )
  expect_equal(
    attr(auto_split, ".page_grp_vars"),
    c("grp1","grp2")
  )

  # multi grouping vars w/ lbl
  df <- tibble::tribble(
    ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
    "A"  ,  "a" ,   "n"  , "n"  , 22,
    "A"  ,  "a" ,   "pct", "pct"  , 11,
    "A"  ,  "b" ,   "n"  , "n"  , 24,
    "A"  ,  "b" ,   "pct", "pct"  , 55
  )
  my_page_plan <- page_plan(
    page_structure(label_val = ".default")
  )

  auto_split <- apply_page_plan(df, my_page_plan, vars(grp1, grp2), quo(lbl))
  man_split <- list(
    tibble::tribble(
      ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
      "A"  ,  "a" ,   "n"  , "n"  , 22,
      "A"  ,  "b" ,   "n"  , "n"  , 24
    ) ,
    tibble::tribble(
      ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
      "A"  ,  "a" ,   "pct", "pct"  , 11,
      "A"  ,  "b" ,   "pct", "pct"  , 55
    )
  )

  expect_equal(auto_split, man_split, ignore_attr = c(".page_note",".page_grp_vars"))

  expect_equal(
    map_chr(auto_split, ~ attr(.x, ".page_note")),
    c("lbl: n", "lbl: pct")
  )
  expect_equal(
    attr(auto_split, ".page_grp_vars"),
    "lbl"
  )

  # specific lbl value
  df <- tibble::tribble(
    ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
    "A"  ,  "a" ,   "lbl1"  , "n"  , 22,
    "A"  ,  "b" ,   "lbl1", "pct"  , 11,
    "A"  ,  "a" ,   "lbl2"  , "n"  , 34,
    "A"  ,  "b" ,   "lbl2", "pct"  , 65,
    "A"  ,  "c" ,   "lbl1"  , "n"  , 97,
    "A"  ,  "d" ,   "lbl1", "pct"  , 23,
    "A"  ,  "c" ,   "lbl2"  , "n"  , 34,
    "A"  ,  "d" ,   "lbl2", "pct"  , 42
  )
  my_page_plan <- page_plan(
    page_structure(label_val = "lbl1")
  )
  auto_split <- apply_page_plan(df, my_page_plan, vars(grp1, grp2), quo(lbl))

  man_split <- list(
    tibble::tribble(
      ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
      "A"  ,  "a" ,   "lbl1"  , "n"  , 22,
      "A"  ,  "b" ,   "lbl1", "pct"  , 11
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
      "A"  ,  "a" ,   "lbl2"  , "n"  , 34,
      "A"  ,  "b" ,   "lbl2", "pct"  , 65,
      "A"  ,  "c" ,   "lbl1"  , "n"  , 97,
      "A"  ,  "d" ,   "lbl1", "pct"  , 23
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~ lbl, ~ prm, ~ trt,
      "A"  ,  "c" ,   "lbl2"  , "n"  , 34,
      "A"  ,  "d" ,   "lbl2", "pct"  , 42
    )
  )

  expect_equal(auto_split, man_split, ignore_attr = c(".page_note",".page_grp_vars"))

})


test_that("page plan with mix of defined & group splits",{

  df <- tibble::tribble(
    ~ grp1, ~ grp2, ~lbl, ~prm, ~trt,
    "A"   , "a"   , "n" , "n" , 22,
    "A"   , "a"   , "pct" , "pct" , 11,
    "A"   , "b"   , "n" , "n" , 44,
    "A"   , "b"   , "pct" , "pct" , 22,
    "B"   , "a"   , "n" , "n" , 54,
    "B"   , "a"   , "pct" , "pct" , 67,
    "B"   , "b"   , "n" , "n" , 72,
    "B"   , "b"   , "pct" , "pct" , 12
  )
  my_page_plan <- page_plan(
    page_structure(group_val = list(grp1 = ".default", grp2 = "a"))
  )

  auto_split <- apply_page_plan(df, my_page_plan, vars(grp1, grp2), quo(lbl))

  ## CURRENT BEHAVIOR:
  # 1. split every level of grp1
  # 2. within 1, split after each consecutive set of grp2 ="a"

  man_split <- list(
    tibble::tribble(
      ~ grp1, ~ grp2, ~lbl, ~prm, ~trt,
      "A"   , "a"   , "n" , "n" , 22,
      "A"   , "a"   , "pct" , "pct" , 11
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~lbl, ~prm, ~trt,
      "A"   , "b"   , "n" , "n" , 44,
      "A"   , "b"   , "pct" , "pct" , 22
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~lbl, ~prm, ~trt,
      "B"   , "a"   , "n" , "n" , 54,
      "B"   , "a"   , "pct" , "pct" , 67
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~lbl, ~prm, ~trt,
      "B"   , "b"   , "n" , "n" , 72,
      "B"   , "b"   , "pct" , "pct" , 12
    )
  )

  expect_equal(auto_split, man_split, ignore_attr = c(".page_note",".page_grp_vars"))

  expect_equal(
    map_chr(auto_split, ~ attr(.x, ".page_note")),
    c("grp1: A", "grp1: A", "grp1: B", "grp1: B")
  )
  expect_equal(
    attr(auto_split, ".page_grp_vars"),
    "grp1"
  )
})

test_that("page plan with multiple structures", {

  df <- tibble::tribble(
    ~ grp1, ~ grp2, ~lbl, ~prm, ~trt,
    "A"   , "a"   , "n" , "n" , 22,
    "A"   , "b"   , "n" , "n" , 11,
    "A"   , "c"   , "n" , "n" , 44,
    "B"   , "a"   , "n" , "n" , 54,
    "B"   , "b"   , "n" , "n" , 67,
    "B"   , "c"   , "n" , "n" , 72
  )

  # valid
  my_page_plan <- page_plan(
    page_structure(label_val = ".default"),
    page_structure(group_val = list(grp1 = ".default", grp2 = "a")),
    page_structure(group_val = list(grp2 = "b"))
  )

  expect_message(
    auto_split <- apply_page_plan(df, my_page_plan, vars(grp1, grp2), quo(lbl)),
    paste0(
      c("`page_plan` contains multiple `page_structures` with values set to \".default\". ",
      "Only the last one specified will be used."),
      collapse = "\n"
  )
  )



  man_split <- list(
    tibble::tribble(
      ~ grp1, ~ grp2, ~lbl, ~prm, ~trt,
      "A"   , "a"   , "n" , "n" , 22
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~lbl, ~prm, ~trt,
      "A"   , "b"   , "n" , "n" , 11
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~lbl, ~prm, ~trt,
      "A"   , "c"   , "n" , "n" , 44
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~lbl, ~prm, ~trt,
      "B"   , "a"   , "n" , "n" , 54
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~lbl, ~prm, ~trt,
      "B"   , "b"   , "n" , "n" , 67
    ),
    tibble::tribble(
      ~ grp1, ~ grp2, ~lbl, ~prm, ~trt,
      "B"   , "c"   , "n" , "n" , 72
    )
  )

  expect_equal(auto_split, man_split, ignore_attr = c(".page_note", ".page_grp_vars"))

  expect_equal(
    map_chr(auto_split, ~ attr(.x, ".page_note")),
    c("grp1: A", "grp1: A", "grp1: A", "grp1: B", "grp1: B", "grp1: B")
  )
  expect_equal(
    attr(auto_split, ".page_grp_vars"),
    "grp1"
  )
})




test_that("Page plan with max_rows", {

  # one group - row_grp_plan label_loc = "indented"
  df <- tibble::tribble(
    ~ grp, ~ lbl, ~ prm, ~ trt,
    "A"  ,  "a" , "n"  , 22,
    "A"  ,  "b" , "n"  , 11,
    "B"  ,  "a" , "n"  , 24,
    "B"  ,  "b" , "n"  , 55,
    "C"  ,  "a" , "n"  , 12,
    "C"  ,  "b" , "n"  , 19,
  ) %>%
    pivot_longer(trt, names_to = "column", values_to = "value")
  mytfrmt <- tfrmt(
    group = "grp",
    label = "lbl",
    param = "prm",
    column = "column",
    value = "value",
    body_plan = body_plan(frmt_structure(group_val=".default", label_val=".default", frmt("xx"))),
    page_plan = page_plan(max_rows = 3)
  )

  auto_split <- apply_tfrmt(df, mytfrmt)
  man_split <- list(
    tibble::tribble(
      ~ lbl, ~ trt, ~`..tfrmt_row_grp_lbl`,
      "A"   , NA, TRUE,
      "  a" , '22', FALSE,
      "  b" , '11', FALSE
    ),
    tibble::tribble(
      ~ lbl, ~ trt, ~`..tfrmt_row_grp_lbl`,
      "B"   , NA, TRUE,
      "  a" , '24', FALSE,
      "  b" , '55', FALSE
    ),
    tibble::tribble(
      ~ lbl, ~ trt, ~`..tfrmt_row_grp_lbl`,
      "C"   , NA, TRUE,
      "  a" , '12', FALSE,
      "  b" , '19', FALSE
    )
  )

  expect_equal(auto_split, man_split, ignore_attr = TRUE)


  # one group - row_grp_plan label_loc = "column"
  mytfrmt <- tfrmt(
    group = "grp",
    label = "lbl",
    param = "prm",
    column = "column",
    value = "value",
    body_plan = body_plan(frmt_structure(group_val=".default", label_val=".default", frmt("xx"))),
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "column")),
    page_plan = page_plan(max_rows = 3)
  )
  auto_split <- apply_tfrmt(df, mytfrmt)
  man_split <- list(
      tibble::tribble(
        ~grp, ~ lbl, ~ trt, ~`..tfrmt_row_grp_lbl`,
        "A" ,  "a" , '22', FALSE,
        "A" ,  "b" , '11', FALSE,
        "B" ,  "a" , '24', FALSE
      ),
      tibble::tribble(
        ~grp, ~ lbl, ~ trt, ~`..tfrmt_row_grp_lbl`,
        "B" ,  "b" , '55', FALSE,
        "C" ,  "a" , '12', FALSE,
        "C" ,  "b" , '19', FALSE
      )
    )
  expect_equal(auto_split, man_split, ignore_attr = TRUE)

})

test_that("Page plan with max_rows & group-level summary rows",{

  dat_summ <- tibble::tribble(
    ~grp1  ,~grp2        ,~my_label     ,   ~prm , ~column, ~val,
    "cat_1" ,"cat_1"      , "cat_1"       , "pct" , "trt" , 34,
    "cat_2" ,"cat_2"      , "cat_2"       , "pct" , "trt" , 43,
    "cat_2" ,"sub_cat_2"  , "sub_cat_2"   , "pct" , "trt" , 12,
    "cat_2" ,"sub_cat_2"  , "sub_cat_3"   , "pct" , "trt" , 76,
    "cat_3" ,"cat_3"      , "cat_3"       , "pct" , "trt" , 56,
    "cat_3" ,"sub_cat_3a" , "sub_cat_3a"  , "pct" , "trt" , 98,
    "cat_3" ,"sub_cat_3b" , "sub_cat_3b_1", "pct" , "trt" , 11,
    "cat_3" ,"sub_cat_3b" , "sub_cat_3b_3", "pct" , "trt" , 5,
    "cat_1" ,"cat_1"      , "cat_1"       , "pct" , "pla" , 23,
    "cat_2" ,"cat_2"      , "cat_2"       , "pct" , "pla" , 15,
    "cat_2" ,"sub_cat_2"  , "sub_cat_2"   , "pct" , "pla" , 73,
    "cat_2" ,"sub_cat_2"  , "sub_cat_3"   , "pct" , "pla" , 10,
    "cat_3" ,"cat_3"      , "cat_3"       , "pct" , "pla" , 65,
    "cat_3" ,"sub_cat_3a" , "sub_cat_3a"  , "pct" , "pla" , 78,
    "cat_3" ,"sub_cat_3b" , "sub_cat_3b_1", "pct" , "pla" , 35,
    "cat_3" ,"sub_cat_3b" , "sub_cat_3b_3", "pct" , "pla" , 8,
  )
  mytfrmt <- tfrmt(
    group = c("grp1","grp2"),
    label = "my_label",
    param = "prm",
    column = "column",
    value = "val",
    body_plan = body_plan(frmt_structure(group_val=".default", label_val=".default", frmt("xx"))),
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented")),
    page_plan = page_plan(max_rows = 3)
  )
  auto_split <- apply_tfrmt(dat_summ, mytfrmt)

  man_split <- list(
       tibble::tribble(
         ~my_label     ,~trt  ,~pla  ,~`..tfrmt_row_grp_lbl`,
         "cat_1"       ,"34"  ,"23"   ,FALSE  ,
         "cat_2"       ,"43"  ,"15"   ,FALSE   ,
         "  sub_cat_2" ,"12"  ,"73"   ,FALSE
         ),
       tibble::tribble(
         ~my_label     ,~trt  ,~pla  ,~`..tfrmt_row_grp_lbl`,
         "cat_2"       ,"43"  ,"15"   ,FALSE   ,
         "  sub_cat_2" ,"12"  ,"73"   ,FALSE    ,
         "    sub_cat_3","76"  ,"10"  ,FALSE
       ),
       tibble::tribble(
         ~my_label     ,~trt  ,~pla  ,~`..tfrmt_row_grp_lbl`,
         "cat_3"            ,"56"  ,"65"  ,FALSE  ,
         "  sub_cat_3a"     ,"98"  ,"78"  ,FALSE
       ),
       tibble::tribble(
         ~my_label     ,~trt  ,~pla  ,~`..tfrmt_row_grp_lbl` ,
         "cat_3"            ,"56"  ,"65"  ,FALSE  ,
         "  sub_cat_3b"     ,NA    ,NA    ,TRUE   ,
         "    sub_cat_3b_1"  ,"11"  ,"35" ,FALSE
       ),
       tibble::tribble(
         ~my_label     ,~trt  ,~pla  ,~`..tfrmt_row_grp_lbl` ,
         "cat_3"            ,"56"  ,"65"  ,FALSE  ,
         "  sub_cat_3b"     ,NA    ,NA    ,TRUE   ,
         "    sub_cat_3b_3"  ," 5"  ," 8" ,FALSE
       )
  )
  expect_equal(auto_split, man_split, ignore_attr = TRUE)


  # increase the max_rows
  mytfrmt <- tfrmt(
    group = c("grp1","grp2"),
    label = "my_label",
    param = "prm",
    column = "column",
    value = "val",
    body_plan = body_plan(frmt_structure(group_val=".default", label_val=".default", frmt("xx"))),
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented")),
    page_plan = page_plan(max_rows = 8)
  )
  auto_split <- apply_tfrmt(dat_summ, mytfrmt)

  man_split <- list(
    tibble::tribble(
      ~my_label         ,~trt  ,~pla  ,~`..tfrmt_row_grp_lbl`,
      "cat_1"           ,"34"  ,"23"   ,FALSE  ,
      "cat_2"           ,"43"  ,"15"   ,FALSE   ,
      "  sub_cat_2"     ,"12"  ,"73"   ,FALSE    ,
      "    sub_cat_3"   ,"76"  ,"10"  ,FALSE    ,
      "cat_3"           ,"56"  ,"65"  ,FALSE  ,
      "  sub_cat_3a"    ,"98"  ,"78"  ,FALSE,
      "  sub_cat_3b"    ,NA    ,NA    ,TRUE     ,
      "    sub_cat_3b_1","11" ,"35"   , FALSE
    ),
    tibble::tribble(
      ~my_label     ,~trt  ,~pla  ,~`..tfrmt_row_grp_lbl`,
      "cat_3"            ,"56"    ,"65"    ,FALSE   ,
      "  sub_cat_3b"     ,NA    ,NA    ,TRUE   ,
      "    sub_cat_3b_3"  ," 5"  ," 8" ,FALSE
    )
  )
  expect_equal(auto_split, man_split, ignore_attr = TRUE)
})

test_that("page plan with both page_structure and max_rows",{

  dat_summ <- tibble::tribble(
    ~grp1  ,~grp2        ,~my_label     ,   ~prm , ~column, ~val,
    "cat_1" ,"cat_1"      , "cat_1"       , "pct" , "trt" , 34,
    "cat_2" ,"cat_2"      , "cat_2"       , "pct" , "trt" , 43,
    "cat_2" ,"sub_cat_2"  , "sub_cat_2"   , "pct" , "trt" , 12,
    "cat_2" ,"sub_cat_2"  , "sub_cat_3"   , "pct" , "trt" , 76,
    "cat_3" ,"cat_3"      , "cat_3"       , "pct" , "trt" , 56,
    "cat_3" ,"sub_cat_3a" , "sub_cat_3a"  , "pct" , "trt" , 98,
    "cat_3" ,"sub_cat_3b" , "sub_cat_3b_1", "pct" , "trt" , 11,
    "cat_3" ,"sub_cat_3b" , "sub_cat_3b_3", "pct" , "trt" , 5
  )

  mypp <- page_plan(
    page_structure(group_val = list(grp1 = ".default")),
    max_rows = 3)

  mytfrmt <- tfrmt(
    group = c("grp1","grp2"),
    label = "my_label",
    param = "prm",
    column = "column",
    value = "val",
    body_plan = body_plan(frmt_structure(group_val=".default", label_val=".default", frmt("xx"))),
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented")),
    page_plan = mypp)
  auto_split <- apply_tfrmt(dat_summ, mytfrmt)

  man_split <- list(
      tibble::tribble(
        ~my_label         ,~trt  ,~`..tfrmt_row_grp_lbl`,
        "cat_1"           ,"34"  ,FALSE  ),
      tibble::tribble(
        ~my_label         ,~trt  ,~`..tfrmt_row_grp_lbl`,
        "cat_2"           ,"43"  ,FALSE   ,
        "  sub_cat_2"     ,"12"  ,FALSE    ,
        "    sub_cat_3"   ,"76"  ,FALSE    ),
      tibble::tribble(
        ~my_label         ,~trt  ,~`..tfrmt_row_grp_lbl`,
        "cat_3"           ,"56"  ,FALSE  ,
        "  sub_cat_3a"    ,"98"  ,FALSE ),
      tibble::tribble(
        ~my_label         ,~trt  ,~`..tfrmt_row_grp_lbl`,
        "cat_3"           ,"56"  ,FALSE  ,
        "  sub_cat_3b"    ,NA    ,TRUE     ,
        "    sub_cat_3b_1","11" ,FALSE
      ),
      tibble::tribble(
        ~my_label     ,~trt  ,~`..tfrmt_row_grp_lbl`,
        "cat_3"            ,"56"  ,FALSE   ,
        "  sub_cat_3b"     ,NA    ,TRUE   ,
        "    sub_cat_3b_3" ," 5"  ,FALSE
      )
    )
  expect_equal(auto_split, man_split, ignore_attr = TRUE)

  expect_equal(
    map_chr(auto_split, ~ attr(.x, ".page_note")),
    c("grp1: cat_1", "grp1: cat_2", "grp1: cat_3", "grp1: cat_3", "grp1: cat_3")
  )


})
