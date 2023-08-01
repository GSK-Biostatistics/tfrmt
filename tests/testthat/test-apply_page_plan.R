
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

  auto_split <- apply_page_struct(df, my_page_plan$struct_list, vars(grp), quo(lbl))
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

  auto_split <- apply_page_struct(df, my_page_plan$struct_list, vars(grp), quo(lbl))
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

  auto_split <- apply_page_struct(df, my_page_plan$struct_list, vars(grp1, grp2), quo(lbl))
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

  auto_split <- apply_page_struct(df, my_page_plan$struct_list, vars(grp1, grp2), quo(lbl))

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

  auto_split <- apply_page_struct(df, my_page_plan$struct_list, vars(grp1, grp2), quo(lbl))
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

  auto_split <- apply_page_struct(df, my_page_plan$struct_list, vars(grp1, grp2), quo(lbl))

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
    auto_split <- apply_page_struct(df, my_page_plan$struct_list, vars(grp1, grp2), quo(lbl)),
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
