
test_that("Page plan with defined split", {

  df <- tibble::tibble(
    grp = c("A","A","B","B","C","C"),
    lbl = c("a", "b", "a", "b", "a", "b"),
    prm = "n",
    trt = c(22, 11, 24, 55, 12, 19)
  )
  my_page_plan <- page_plan(
    page_structure(group_val = "A", label_val = "a")
  )

  auto_split <- apply_page_struct(df, my_page_plan$struct_list, vars(grp), quo(lbl))
  man_split <- list(
    tibble::tibble(
      grp = c("A"),
      lbl = c("a"),
      prm = "n",
      trt = c(22)
    ),
    tibble::tibble(
      grp = c("A", "B","B","C","C"),
      lbl = c("b", "a", "b", "a", "b"),
      prm = "n",
      trt = c(11, 24, 55, 12, 19)
    )
  )

  expect_equal(auto_split, man_split)
})



test_that("Page plan with grouped split", {

  # single grouping var
  df <- tibble::tibble(
    grp = c("A","A","B","B","C","C"),
    lbl = c("a", "b", "a", "b", "a", "b"),
    prm = "n",
    trt = c(22, 11, 24, 55, 12, 19)
  )
  my_page_plan <- page_plan(
    page_structure(group_val = ".default")
  )

  auto_split <- apply_page_struct(df, my_page_plan$struct_list, vars(grp), quo(lbl))
  man_split <- list(
    `grp: A` = tibble::tibble(
      lbl = c("a", "b"),
      prm = "n",
      trt = c(22, 11)
    ),
    `grp: B` = tibble::tibble(
      lbl = c("a", "b"),
      prm = "n",
      trt = c(24, 55)
    ),
    `grp: C` = tibble::tibble(
      lbl = c("a", "b"),
      prm = "n",
      trt = c(12, 19)
    )
  )

  expect_equal(auto_split, man_split)

  # multi grouping vars
  df <- tibble::tibble(
    grp1 = c("A","A","B","B","C","C"),
    grp2 = c("a", "b", "a", "b", "a", "b"),
    lbl = "n",
    prm = "n",
    trt = c(22, 11, 24, 55, 12, 19)
  )
  my_page_plan <- page_plan(
    page_structure(group_val = list(grp2 = ".default"))
  )

  auto_split <- apply_page_struct(df, my_page_plan$struct_list, vars(grp1, grp2), quo(lbl))
  man_split <- list(
    `grp2: a` = tibble::tibble(
      grp1 = c("A", "B", "C"),
      lbl = "n",
      prm = "n",
      trt = c(22, 24, 12)
    ),
    `grp2: b` = tibble::tibble(
      grp1 = c("A", "B", "C"),
      lbl = "n",
      prm = "n",
      trt = c(11, 55, 19)
    )
  )

  expect_equal(auto_split, man_split)

  # multi grouping vars w/ lbl
  df <- tibble::tibble(
    grp1 = c("A","A","A","A"),
    grp2 = c("a", "a", "b", "b"),
    lbl = c("n","pct","n","pct"),
    prm = c("n","pct","n","pct"),
    trt = c(22, 11, 24, 55)
  )
  my_page_plan <- page_plan(
    page_structure(label_val = ".default")
  )

  auto_split <- apply_page_struct(df, my_page_plan$struct_list, vars(grp1, grp2), quo(lbl))
  man_split <- list(
    `lbl: n` = tibble::tibble(
      grp1 = c("A", "A"),
      grp2 = c("a", "b"),
      prm = "n",
      trt = c(22, 24)
    ),
    `lbl: pct` = tibble::tibble(
      grp1 = c("A", "A"),
      grp2 = c("a", "b"),
      prm = "pct",
      trt = c(11, 55)
    )
  )

  expect_equal(auto_split, man_split)

})

test_that("page plan with mix of defined & group splits",{

  df <- tibble::tibble(
    grp1 = c("A","A","A","A", "B","B","B","B"),
    grp2 = c("a", "a", "b", "b", "a", "a", "b", "b"),
    lbl = c("n","pct","n","pct", "n","pct","n","pct"),
    prm = c("n","pct","n","pct","n","pct","n","pct"),
    trt = c(22, 11, 24, 55, 33, 24, 53, 13)
  )
  my_page_plan <- page_plan(
    page_structure(group_val = list(grp1 = ".default", grp2 = "a"))
  )

  auto_split <- apply_page_struct(df, my_page_plan$struct_list, vars(grp1, grp2), quo(lbl))

  ## CURRENT BEHAVIOR:
  # 1. split every level of grp1
  # 2. within 1, split after each consecutive set of grp2 ="a"

  man_split <- list(
    `grp1: A` = tibble::tibble(
      grp2 = c("a", "a"),
      lbl = c("n", "pct"),
      prm = c("n", "pct"),
      trt = c(22, 11)
    ),
    `grp1: A` = tibble::tibble(
      grp2 = c("b", "b"),
      lbl = c("n", "pct"),
      prm = c("n", "pct"),
      trt = c(24, 55)
    ),
    `grp1: B` = tibble::tibble(
      grp2 = c("a", "a"),
      lbl = c("n", "pct"),
      prm = c("n", "pct"),
      trt = c(33, 24)
    ),
    `grp1: B` = tibble::tibble(
      grp2 = c("b", "b"),
      lbl = c("n", "pct"),
      prm = c("n", "pct"),
      trt = c(53, 13)
    )
  )

  expect_equal(auto_split, man_split)
})

test_that("page plan with multiple structures", {

  df <- tibble::tibble(
    grp1 = c("A","A","A","A", "B","B","B","B"),
    grp2 = c("a", "a", "b", "b", "a", "a", "b", "b"),
    lbl = c("n","pct","n","pct", "n","pct","n","pct"),
    prm = c("n","pct","n","pct","n","pct","n","pct"),
    trt = c(22, 11, 24, 55, 33, 24, 53, 13)
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
    `grp1: A` = tibble::tibble(
      grp2 = c("a", "a"),
      lbl = c("n", "pct"),
      prm = c("n", "pct"),
      trt = c(22, 11)
    ),
    `grp1: A` = tibble::tibble(
      grp2 = c("b", "b"),
      lbl = c("n", "pct"),
      prm = c("n", "pct"),
      trt = c(24, 55)
    ),
    `grp1: B` = tibble::tibble(
      grp2 = c("a", "a"),
      lbl = c("n", "pct"),
      prm = c("n", "pct"),
      trt = c(33, 24)
    ),
    `grp1: B` = tibble::tibble(
      grp2 = c("b", "b"),
      lbl = c("n", "pct"),
      prm = c("n", "pct"),
      trt = c(53, 13)
    )
  )

  expect_equal(auto_split, man_split)
})
