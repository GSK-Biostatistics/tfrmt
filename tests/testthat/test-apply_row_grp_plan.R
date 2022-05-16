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
       "A",  "        ", "        ", "        ",
       "B",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
       "B",  "        ", "        ", "        ",
       "C",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
       "C",  "        ", "        ", "        ",
       "D",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
       "D",  "        ", "        ", "        "
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
    row_grp_structure(group_val = list(grp1 = ".default", grp2 = "b"), element_block(post_space = " "))
  )

  expect_equal(
    apply_row_grp_plan(df, sample_grp_plan, vars(grp1, grp2)),
    tribble(
      ~grp1,    ~grp2,   ~trtA,       ~trtB,     ~trtC,
      "A",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "b",     "        ", "        ", "        ",
      "B",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",     "b",     "        ", "        ", "        ",
      "C",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "b",     "        ", "        ", "        "
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
      "A",  "--------", "--------",  "--------",
      "B",  "xx (xx%)", "xx (xx%)",  "xx (xx%)",
      "B",  "        ", "        ",  "        ",
      "C",  "xx (xx%)", "xx (xx%)",  "xx (xx%)",
      "C",  "--------", "--------",  "--------",
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
      "A",     "b",     "        ", "        ", "        ",
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
      "A",     "a",     "        ", "        ", "        ",
      "A",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "b",     "********", "********", "********",
      "B",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",     "a",     "        ", "        ", "        ",
      "B",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",     "b",     "        ", "        ", "        ",
      "C",     "a",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "a",     "        ", "        ", "        ",
      "C",     "b",     "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "b",     "        ", "        ", "        "
    ))

})


test_that("no post space added if NULL",{

  df <- tibble(
    grp1 = c("A","B","C","D"),
    trtA = rep("xx (xx%)", 4),
    trtB = rep("xx (xx%)", 4),
    trtC = rep("xx (xx%)", 4),
  )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = NULL))
  )

  expect_equal(
    apply_row_grp_plan(df, sample_grp_plan, vars(grp1)),
    tribble(
      ~grp1, ~trtA,      ~trtB,      ~trtC,
      "A",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "D",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
    ))
})



test_that("post space is truncated to data width",{

  df <- tibble(
    grp1 = c("A","B","C","D"),
    trtA = rep("xx (xx%)", 4),
    trtB = rep("xx (xx%)", 4),
    trtC = rep("xx (xx%)", 4),
  )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space ="----------------------"))
  )

  expect_equal(
    apply_row_grp_plan(df, sample_grp_plan, vars(grp1)),
    tribble(
      ~grp1, ~trtA,      ~trtB,      ~trtC,
      "A",  "xx (xx%)", "xx (xx%)",  "xx (xx%)",
      "A",  "--------", "--------",  "--------",
      "B",  "xx (xx%)", "xx (xx%)",  "xx (xx%)",
      "B",  "--------", "--------",  "--------",
      "C",  "xx (xx%)", "xx (xx%)",  "xx (xx%)",
      "C",  "--------", "--------",  "--------",
      "D",  "xx (xx%)", "xx (xx%)",  "xx (xx%)",
      "D",  "--------", "--------",  "--------"))
})


test_that("Check combine_group_cols with a single group", {

  mock_single_grp <- tibble(
    crossing(grp1 = c("A","B","C"),
             lab = c("a","b")),
    trtA = rep("xx (xx%)", 6),
    trtB = rep("xx (xx%)", 6),
    trtC = rep("xx (xx%)", 6),
  )

  auto_test_no_span <- combine_group_cols(mock_single_grp,
                     group = vars(grp1), label = sym("lab"),
                     spanning_header = FALSE)
  man_test_no_span <- tribble(  ~lab,   ~trtA,     ~trtB,   ~trtC,
             "A"  , NA,         NA,       NA,
             "  a", "xx (xx%)", "xx (xx%)", "xx (xx%)",
             "  b", "xx (xx%)", "xx (xx%)", "xx (xx%)",
             "B"  , NA,         NA,       NA,
             "  a", "xx (xx%)", "xx (xx%)", "xx (xx%)",
             "  b", "xx (xx%)", "xx (xx%)", "xx (xx%)",
             "C"  , NA,         NA,       NA,
             "  a", "xx (xx%)", "xx (xx%)", "xx (xx%)",
             "  b", "xx (xx%)", "xx (xx%)", "xx (xx%)")

  expect_equal(auto_test_no_span, man_test_no_span)
  #With spanning (so no change to the data)
  expect_equal(combine_group_cols(mock_single_grp,
                                  group = vars(grp1), label = sym("lab"),
                                  spanning_header = TRUE),
               mock_single_grp
               )
})


test_that("Check combine_group_cols with a multi groups", {
  mock_multi_grp <- tribble(
    ~grp1,    ~grp2,     ~ my_label,
    "grp1_1", "grp2_1", "my_label_1",
    "grp1_1", "grp2_1", "my_label_2",
    "grp1_1", "grp2_2", "my_label_1",
    "grp1_1", "grp2_2", "my_label_2",
    "grp1_2", "grp2_1", "my_label_1",
    "grp1_2", "grp2_1", "my_label_2",
    "grp1_2", "grp2_2", "my_label_1",
    "grp1_2", "grp2_2", "my_label_2",
  ) %>%
    mutate(
      trtA = rep("xx (xx%)", 8),
      trtB = rep("xx (xx%)", 8),
      trtC = rep("xx (xx%)", 8),
    )

  auto_test_no_span <- combine_group_cols(mock_multi_grp,
                     group = vars(grp1, grp2), label = sym("my_label"),
                     spanning_header = FALSE)
  man_test_no_span <- tribble(
    ~my_label,         ~trtA,     ~trtB,     ~trtC,
    "grp1_1"        , NA      , NA      , NA      ,
    "  grp2_1"      , NA      , NA      , NA      ,
    "    my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "    my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "  grp2_2"      , NA      , NA      , NA      ,
    "    my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "    my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "grp1_2"        , NA      , NA      , NA      ,
    "  grp2_1"      , NA      , NA      , NA      ,
    "    my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "    my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "  grp2_2"      , NA      , NA      , NA      ,
    "    my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "    my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)"
  )

  expect_equal(auto_test_no_span, man_test_no_span)

  auto_test_with_span <- combine_group_cols(mock_multi_grp,
                                          group = vars(grp1, grp2), label = sym("my_label"),
                                          spanning_header = TRUE)

  #Should be the same as removing a group
  man_test_with_span <- combine_group_cols(mock_multi_grp,
                     group = vars(grp1), label = sym("my_label"),
                     spanning_header = FALSE)
  expect_equal(auto_test_with_span, man_test_with_span)

})

