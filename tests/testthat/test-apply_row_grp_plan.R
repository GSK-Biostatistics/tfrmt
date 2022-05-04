test_that("insert post space - single grouping variable",{

  df <- tibble(
    grp1 = c("A","B","C","D"),
    trtA = rep("xx (xx%)", 4),
    trtB = rep("xx (xx%)", 4),
    trtC = rep("xx (xx%)", 4),
  )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = " "))
  )

  expect_equal(
    apply_row_grp_plan(df, sample_grp_plan, vars(grp1)),
    tribble(
      ~grp1, ~trtA,      ~trtB,      ~trtC,
       "A",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
       "A",  " "       , " "       , " ",
       "B",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
       "B",  " "       , " "       , " "     ,
       "C",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
       "C",  " "       , " "       , " "     ,
       "D",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
       "D",  " "       , " "       , " "
    ))

})

test_that("insert post space - two grouping variables",{

  df <- tibble(
    crossing(grp1 = c("A","B","C"),
             grp2 = c("a","b")),
    trtA = rep("xx (xx%)", 6),
    trtB = rep("xx (xx%)", 6),
    trtC = rep("xx (xx%)", 6),
  )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = list(grp2 = "b"), element_block(post_space = " "))
  )

  expect_equal(
    apply_row_grp_plan(df, sample_grp_plan, vars(grp1, grp2)),
    tribble(
      ~grp1,    ~grp2,   ~trtA,       ~trtB,     ~trtC,
      "A",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "b",     " "       , " "       , " "       ,
      "B",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",     "b",     " "       , " "       , " "       ,
      "C",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "b",     " "       , " "       , " "
  ))

})


test_that("insert mix - single grouping variable",{

  df <- tibble(
    grp1 = c("A","B","C","D"),
    trtA = rep("xx (xx%)", 4),
    trtB = rep("xx (xx%)", 4),
    trtC = rep("xx (xx%)", 4),
  )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = c("A","C"), element_block(post_space = "---")),
    row_grp_structure(group_val = c("B"), element_block(post_space = " "))
  )

  expect_equal(
    apply_row_grp_plan(df, sample_grp_plan, vars(grp1)),
    tribble(
      ~grp1, ~trtA,      ~trtB,      ~trtC,
      "A",  "xx (xx%)", "xx (xx%)",  "xx (xx%)",
      "A",  "---",      "---"      , "---",
      "B",  "xx (xx%)", "xx (xx%)" , "xx (xx%)",
      "B",  " " ,       " "     ,    " "     ,
      "C",  "xx (xx%)", "xx (xx%)" , "xx (xx%)",
      "C",  "---",      "---"     ,  "---"     ,
      "D",  "xx (xx%)", "xx (xx%)",  "xx (xx%)"
    ))
})



test_that("insert post space after specific value",{

  df <- tibble(
    crossing(grp1 = c("A","B","C"),
             grp2 = c("a","b")),
    trtA = rep("xx (xx%)", 6),
    trtB = rep("xx (xx%)", 6),
    trtC = rep("xx (xx%)", 6),
  )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = list(grp1 = "A", grp2 = "b"), element_block(post_space = " "))
  )

  expect_equal(
    apply_row_grp_plan(df, sample_grp_plan, vars(grp1, grp2)),
    tribble(
      ~grp1,    ~grp2,   ~trtA,       ~trtB,     ~trtC,
      "A",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "b",     " "       , " "       , " "       ,
      "B",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)"
    ))

})




test_that("overlapping row_grp_structures - prefers latest",{

  df <- tibble(
    crossing(grp1 = c("A","B","C"),
             grp2 = c("a","b")),
    trtA = rep("xx (xx%)", 6),
    trtB = rep("xx (xx%)", 6),
    trtC = rep("xx (xx%)", 6),
  )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = " ")),
    row_grp_structure(group_val = list(grp1 = "A", grp2 = "b"), element_block(post_space = "***"))
  )

  expect_equal(
    apply_row_grp_plan(df, sample_grp_plan, vars(grp1, grp2)),
    tribble(
      ~grp1,    ~grp2,   ~trtA,       ~trtB,     ~trtC,
      "A",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "a",     " "       , " "       , " "       ,
      "A",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "b",     "***"     , "***"     , "***"     ,
      "B",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",     "a",     " "       , " "       , " "       ,
      "B",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",     "b",     " "       , " "       , " "       ,
      "C",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "a",     " "       , " "       , " "       ,
      "C",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "b",     " "       , " "       , " "
    ))

})
