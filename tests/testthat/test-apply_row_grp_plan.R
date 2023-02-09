test_that("insert post space - single grouping variable",{

  df <- tibble(
    grp1 = c("A","B","C","D"),
    label = as.character(1:4),
    trtA = rep("xx (xx%)", 4),
    trtB = rep("xx (xx%)", 4),
    trtC = rep("xx (xx%)", 4),
  )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = " ")),
    label_loc = element_row_grp_loc(location = "spanning")
  )

  expect_equal(
    apply_row_grp_struct(df, sample_grp_plan$struct_ls, vars(grp1), sym("label")),
    tibble::tribble(
      ~grp1,~label, ~trtA,      ~trtB,      ~trtC,
      "A",  "1",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",  " ",  " ", " ", " ",
      "B",  "2",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",  " ",  " ", " ", " ",
      "C",  "3",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",  " ",  " ", " ", " ",
      "D",  "4",  "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "D",  " ",  " ", " ", " "
    ))


  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = " "))
  )

  expect_equal(
    apply_row_grp_struct(df, sample_grp_plan$struct_ls, vars(grp1), sym("label")),
    tibble::tribble(
      ~grp1, ~label, ~trtA,      ~trtB,       ~trtC,
      "A", "1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A", " ", " ", " ", " ",
      "B", "2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B", " ", " ", " ", " ",
      "C", "3", "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C", " ", " ", " ", " ",
      "D", "4", "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "D", " ", " ", " ", " ",
    ))

})

test_that("insert post space - two grouping variables",{

  df <- tibble(
    crossing(grp1 = c("A","B","C"),
             grp2 = c("a","b"),
             label = as.character(1:2)),
    trtA = rep("xx (xx%)", 12),
    trtB = rep("xx (xx%)", 12),
    trtC = rep("xx (xx%)", 12),
  )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = list(grp1 = ".default", grp2 = "b"), element_block(post_space = " ")),
    label_loc = element_row_grp_loc(location = "spanning")
  )

  expect_equal(
    apply_row_grp_struct(df, sample_grp_plan$struct_ls, vars(grp1, grp2), label = sym("label")),
    tibble::tribble(
      ~grp1, ~grp2, ~label,   ~trtA,       ~trtB,     ~trtC,
      "A",  "a",   "1", "xx (xx%)" ,"xx (xx%)", "xx (xx%)",
      "A",  "a",   "2", "xx (xx%)" ,"xx (xx%)", "xx (xx%)",
      "A",  "b",   "1", "xx (xx%)" ,"xx (xx%)", "xx (xx%)",
      "A",  "b",   "2", "xx (xx%)" ,"xx (xx%)", "xx (xx%)",
      "A",  "b",   " ", " " ," ", " ",
      "B",  "a",   "1", "xx (xx%)" ,"xx (xx%)", "xx (xx%)",
      "B",  "a",   "2", "xx (xx%)" ,"xx (xx%)", "xx (xx%)",
      "B",  "b",   "1", "xx (xx%)" ,"xx (xx%)", "xx (xx%)",
      "B",  "b",   "2", "xx (xx%)" ,"xx (xx%)", "xx (xx%)",
      "B",  "b",   " ", " " ," ", " ",
      "C",  "a",   "1", "xx (xx%)" ,"xx (xx%)", "xx (xx%)",
      "C",  "a",   "2", "xx (xx%)" ,"xx (xx%)", "xx (xx%)",
      "C",  "b",   "1", "xx (xx%)" ,"xx (xx%)", "xx (xx%)",
      "C",  "b",   "2", "xx (xx%)" ,"xx (xx%)", "xx (xx%)",
      "C",  "b",   " ", " " ," ", " ",
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
    row_grp_structure(group_val = c("B"), element_block(post_space = " ")),
    label_loc = element_row_grp_loc(location = "spanning")
  )

  expect_equal(
    apply_row_grp_struct(df, sample_grp_plan$struct_ls, vars(grp1)),
    tibble::tribble(
      ~grp1, ~trtA,      ~trtB,      ~trtC,
      "A",  "xx (xx%)", "xx (xx%)",  "xx (xx%)",
      "A",  "--------", "--------",  "--------",
      "B",  "xx (xx%)", "xx (xx%)",  "xx (xx%)",
      "B",  " ", " ",  " ",
      "C",  "xx (xx%)", "xx (xx%)",  "xx (xx%)",
      "C",  "--------", "--------",  "--------",
      "D",  "xx (xx%)", "xx (xx%)",  "xx (xx%)"
    )
    )
})



test_that("insert post space after specific value",{

  df <- tibble(
    crossing(grp1 = c("A","B","C"),
             grp2 = c("a","b")),
    label = as.character(1),
    trtA = rep("xx (xx%)", 6),
    trtB = rep("xx (xx%)", 6),
    trtC = rep("xx (xx%)", 6),
  )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = list(grp1 = "A", grp2 = "b"), element_block(post_space = " ")),
    label_loc = element_row_grp_loc(location = "spanning")
  )

  expect_equal(
    apply_row_grp_struct(df, sample_grp_plan$struct_ls, vars(grp1, grp2), label = sym("label")),
    tibble::tribble(
      ~grp1,  ~grp2, ~label,   ~trtA,       ~trtB,     ~trtC,
      "A",     "a", "1",   "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "b", "1",   "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "b", " ",   " ", " ", " ",
      "B",     "a", "1",   "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",     "b", "1",   "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "a", "1",   "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "b", "1",   "xx (xx%)", "xx (xx%)", "xx (xx%)"
    )
    )

})




test_that("overlapping row_grp_structures - prefers latest",{

  df <- tibble(
    crossing(grp1 = c("A","B","C"),
             grp2 = c("a","b")),
    label = as.character(1),
    trtA = rep("xx (xx%)", 6),
    trtB = rep("xx (xx%)", 6),
    trtC = rep("xx (xx%)", 6),
  )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = " ")),
    row_grp_structure(group_val = list(grp1 = "A", grp2 = "b"), element_block(post_space = "***")),
    label_loc = element_row_grp_loc(location = "column")
  )

  expect_equal(
    apply_row_grp_struct(df, sample_grp_plan$struct_ls, vars(grp1, grp2), label = sym("label")),
    tibble::tribble(
      ~grp1,  ~grp2, ~label,   ~trtA,       ~trtB,     ~trtC,
      "A",     "a", "1",   "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "a", " ",   " ", " ", " ",
      "A",     "b", "1",   "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "A",     "b", "*",   "********", "********", "********",
      "B",     "a", "1",   "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",     "a", " ",   " ", " ", " ",
      "B",     "b", "1",   "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "B",     "b", " ",   " ", " ", " ",
      "C",     "a", "1",   "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "a", " ",   " ", " ", " ",
      "C",     "b", "1",   "xx (xx%)", "xx (xx%)", "xx (xx%)",
      "C",     "b", " ",   " ", " ", " ",
    ) )

})


test_that("no post space added if NULL",{

  df <- tibble(
    grp1 = c("A","B","C","D"),
    trtA = rep("xx (xx%)", 4),
    trtB = rep("xx (xx%)", 4),
    trtC = rep("xx (xx%)", 4),
  )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = NULL)),
    label_loc = element_row_grp_loc(location = "spanning")
  )

  expect_equal(
    apply_row_grp_struct(df, sample_grp_plan$struct_ls, vars(grp1)),
    tibble::tribble(
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
    row_grp_structure(group_val = ".default", element_block(post_space ="----------------------")),
    label_loc =element_row_grp_loc(location = "spanning")
  )

  expect_equal(
    apply_row_grp_struct(df, sample_grp_plan$struct_ls, vars(grp1)),
    tibble::tribble(
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
                                          group = vars(grp1), label = sym("lab"))
  man_test_no_span <- tibble::tribble(
    ~grp1,  ~lab,   ~trtA,     ~trtB,   ~trtC,       ~..tfrmt_row_grp_lbl,
    "A",  "A"  , NA,         NA,       NA,           TRUE,
    "A",  "  a", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
    "A",  "  b", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
    "B",  "B"  , NA,         NA,       NA,           TRUE,
    "B",  "  a", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
    "B",  "  b", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
    "C",  "C"  , NA,         NA,       NA,           TRUE,
    "C",  "  a", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
    "C",  "  b", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE)


  expect_equal(auto_test_no_span, man_test_no_span)
  #With spanning (so no change to the data)
  expect_equal(combine_group_cols(mock_single_grp,
                                  group = vars(grp1), label = sym("lab"),
                                  element_row_grp_loc(location = "spanning")) %>%
                 select(-..tfrmt_row_grp_lbl),
               mock_single_grp
  )
})


test_that("Check combine_group_cols with a multi groups", {
  mock_multi_grp <- tibble::tribble(
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
                                          element_row_grp_loc(location = "indented"))

  man_test_no_span <- tibble::tribble(
    ~grp1,     ~my_label,      ~grp2,   ~trtA,     ~trtB,     ~trtC,         ~..tfrmt_row_grp_lbl,
    "grp1_1", "grp1_1"        ,NA     , NA      , NA      , NA      ,        TRUE,
    "grp1_1", "  grp2_1"      ,"grp2_1", NA     , NA      , NA      ,        TRUE,
    "grp1_1", "    my_label_1","grp2_1", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
    "grp1_1", "    my_label_2","grp2_1", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
    "grp1_1", "  grp2_2"      ,"grp2_2", NA      , NA      , NA      ,        TRUE,
    "grp1_1", "    my_label_1","grp2_2", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
    "grp1_1", "    my_label_2","grp2_2", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
    "grp1_2", "grp1_2"        , NA     , NA      , NA      , NA      ,        TRUE,
    "grp1_2", "  grp2_1"      ,"grp2_1", NA      , NA      , NA      ,        TRUE,
    "grp1_2", "    my_label_1","grp2_1", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
    "grp1_2", "    my_label_2","grp2_1", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
    "grp1_2", "  grp2_2"      ,"grp2_2", NA      , NA      , NA      ,        TRUE,
    "grp1_2", "    my_label_1","grp2_2", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
    "grp1_2", "    my_label_2","grp2_2", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE
  )

  expect_equal(auto_test_no_span, man_test_no_span)

  auto_test_with_span <- combine_group_cols(mock_multi_grp,
                                            group = vars(grp1, grp2), label = sym("my_label"),
                                            element_row_grp_loc(location = "spanning"))

  #Should be the same as removing a group
  man_test_with_span <- mock_multi_grp %>%
    group_by(grp1) %>%
    group_split() %>%
    map_dfr(combine_group_cols,group = vars(grp2), label = sym("my_label")) %>%
    select(grp1, grp2, everything()) %>%
    mutate(grp1 = ifelse(grp1=="", NA, grp1)) %>%
    fill(grp1, .direction = "up")
  expect_equal(auto_test_with_span, man_test_with_span)

})


test_that("Check apply_row_grp_* w/ list-columns (in case of incomplete body_plan)", {
  mock_multi_grp <- tibble::tribble(
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
      trtA = rep("xx (xx%)", 8) %>% as.list(),
      trtB = rep("xx (xx%)", 8) %>% as.list(),
      trtC = rep("xx (xx%)", 8) %>% as.list(),
    )

  sample_grp_plan <- row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space =" ")),
    label_loc = element_row_grp_loc(location = "indented")
  )

  auto_test_listcols <- apply_row_grp_lbl(mock_multi_grp, sample_grp_plan$label_loc,group = vars(grp1, grp2), label = sym("my_label")) %>%
    remove_grp_cols(sample_grp_plan$label_loc,group = vars(grp1, grp2),label = sym("my_label"))

  man_test_listcols <- tibble::tribble(
    ~my_label,      ~trtA,     ~trtB,     ~trtC,         ~..tfrmt_row_grp_lbl,
   "grp1_1"        , NA     , NA       , NA      ,       TRUE,
   "  grp2_1"      , NA       , NA       , NA       ,      TRUE,
   "    my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
   "    my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
   "  grp2_2"      , NA       , NA       , NA       ,      TRUE,
   "    my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
   "    my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
   "grp1_2"        , NA       , NA       , NA       ,      TRUE,
   "  grp2_1"      , NA       , NA       , NA       ,      TRUE,
   "    my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
   "    my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
   "  grp2_2"      , NA      , NA       , NA       ,      TRUE,
   "    my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE,
   "    my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)", FALSE
  ) %>%
    mutate(across(trtA:trtC, ~as.list(.x)))

  expect_equal(auto_test_listcols, man_test_listcols)


  auto_test_listcols <- apply_row_grp_struct(mock_multi_grp, sample_grp_plan$struct_ls,group = vars(grp1, grp2), label = sym("my_label"))

  man_test_listcols <- tibble::tribble(
    ~grp1,    ~grp2,   ~my_label,      ~trtA,     ~trtB,     ~trtC,
    "grp1_1", "grp2_1", "my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "grp1_1", "grp2_1", "my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "grp1_1", "grp2_1", " ", " ", " ", " ",
    "grp1_1", "grp2_2", "my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "grp1_1", "grp2_2", "my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "grp1_1", "grp2_2", " ", " ", " ", " ",
    "grp1_2", "grp2_1", "my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "grp1_2", "grp2_1", "my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "grp1_2", "grp2_1", " ", " ", " ", " ",
    "grp1_2", "grp2_2", "my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "grp1_2", "grp2_2", "my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "grp1_2", "grp2_2", " ", " ", " ", " "
  ) %>%
    mutate(across(trtA:trtC, ~as.list(.x)))

  expect_equal(auto_test_listcols, man_test_listcols)



})



test_that("> 2 groups with and without spanner_label", {
  mock_multi_grp <- tibble::tribble(
    ~grp1,    ~grp2,     ~grp3, ~ my_label,
    "grp1_1", "grp2_1", "grp3_1", "my_label_1",
    "grp1_1", "grp2_1", "grp3_1", "my_label_2",
    "grp1_1", "grp2_1", "grp3_2", "my_label_1",
    "grp1_1", "grp2_1", "grp3_2", "my_label_2",
    "grp1_1", "grp2_2", "grp3_1", "my_label_1",
    "grp1_1", "grp2_2", "grp3_1", "my_label_2",
    "grp1_1", "grp2_2", "grp3_2", "my_label_1",
    "grp1_1", "grp2_2", "grp3_2", "my_label_2",
  ) %>%
    mutate(
      trtA = rep("xx (xx%)", 8),
      trtB = rep("xx (xx%)", 8),
      trtC = rep("xx (xx%)", 8),
    )

  plan_no_span <- row_grp_plan(
  )

  expect_equal(
    apply_row_grp_lbl(mock_multi_grp, plan_no_span$label_loc, vars(grp1, grp2, grp3), sym("my_label")) %>%
      remove_grp_cols(plan_no_span$label_loc,vars(grp1, grp2, grp3)),
    tibble::tribble(
      ~my_label        , ~trtA     , ~trtB     , ~trtC   ,     ~..tfrmt_row_grp_lbl,
      "grp1_1"           ,NA          ,NA         ,NA        , TRUE,
      "  grp2_1"         ,NA         ,NA         ,NA        , TRUE,
      "    grp3_1"       ,NA         ,NA         ,NA        , TRUE,
      "      my_label_1" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "      my_label_2" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "    grp3_2"       ,NA         ,NA         ,NA,         TRUE,
      "      my_label_1" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "      my_label_2" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "  grp2_2"         ,NA         ,NA         ,NA        , TRUE,
      "    grp3_1"       ,NA         ,NA         ,NA        , TRUE,
      "      my_label_1" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "      my_label_2" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "    grp3_2"       ,NA         ,NA        ,NA        ,  TRUE,
      "      my_label_1" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "      my_label_2" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)" ,FALSE
    )
  )

  plan_with_span <- row_grp_plan(label_loc= element_row_grp_loc(location = "spanning"))

  expect_equal(
    apply_row_grp_lbl(mock_multi_grp, plan_with_span$label_loc, vars(grp1, grp2, grp3), sym("my_label")) %>%
      remove_grp_cols(plan_with_span$label_loc, vars(grp1, grp2, grp3)),
    tibble::tribble(
     ~grp1,   ~my_label        , ~trtA     , ~trtB     , ~trtC   ,    ~..tfrmt_row_grp_lbl,
     "grp1_1", "grp2_1"         ,NA         ,NA         ,NA,          TRUE,
     "grp1_1", "  grp3_1"       ,NA         ,NA         ,NA,         TRUE,
     "grp1_1", "    my_label_1" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
     "grp1_1", "    my_label_2" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
     "grp1_1", "  grp3_2"       ,NA         ,NA         ,NA,         TRUE,
     "grp1_1", "    my_label_1" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
     "grp1_1", "    my_label_2" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
     "grp1_1", "grp2_2"         ,NA         ,NA         ,NA,         TRUE,
     "grp1_1", "  grp3_1"       ,NA         ,NA         ,NA,         TRUE,
     "grp1_1", "    my_label_1" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
     "grp1_1", "    my_label_2" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
     "grp1_1", "  grp3_2"       ,NA         ,NA         ,NA,         TRUE,
     "grp1_1", "    my_label_1" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
     "grp1_1", "    my_label_2" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE
    ) %>% group_by(grp1)
  )

})


test_that("Summary rows are not indented", {
  mock_multi_grp <- tibble::tribble(
    ~grp1,    ~grp2, ~my_label,
    "cat_1", "cat_1", "cat_1",
    "cat_2", "cat_2", "cat_2",
    "cat_2", "sub_cat_2", "sub_cat_2",
    "cat_2", "sub_cat_2", "sub_cat_3",
    "cat_3", "cat_3", "cat_3",
    "cat_3", "sub_cat_3a", "sub_cat_3a",
    "cat_3", "sub_cat_3b", "sub_cat_3b_1",
    "cat_3", "sub_cat_3b", "sub_cat_3b_3",
  ) %>%    mutate(
      trtA = rep("xx (xx%)", 8),
      trtB = rep("xx (xx%)", 8),
      trtC = rep("xx (xx%)", 8),
    )

  plan_no_span <- row_grp_plan(
  )

  expect_equal(
    apply_row_grp_lbl(mock_multi_grp, plan_no_span$label_loc, vars(grp1, grp2), sym("my_label")) %>%
      remove_grp_cols( plan_no_span$label_loc, vars(grp1, grp2)),
    tibble::tribble(
      ~my_label ,        ~trtA       , ~trtB       , ~trtC,   ~..tfrmt_row_grp_lbl,
      "cat_1"            ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "cat_2"            ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "  sub_cat_2"      ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "    sub_cat_3"    ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "cat_3"            ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "  sub_cat_3a"     ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "  sub_cat_3b"     ,NA         ,NA         ,NA        ,TRUE,
      "    sub_cat_3b_1" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE,
      "    sub_cat_3b_3" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)", FALSE
    )
  )

  plan_with_span <- row_grp_plan(label_loc= element_row_grp_loc(location = "spanning"))

  expect_equal(
    apply_row_grp_lbl(mock_multi_grp, plan_with_span$label_loc, vars(grp1, grp2), sym("my_label")) %>%
      remove_grp_cols(plan_with_span$label_loc, vars(grp1, grp2)),
    tibble::tribble(
      ~grp1,   ~my_label ,        ~trtA       , ~trtB       , ~trtC,  ~..tfrmt_row_grp_lbl,
       "cat_1", "cat_1"          ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)",FALSE,
       "cat_2", "cat_2"          ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)",FALSE,
       "cat_2", "sub_cat_2"      ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)",FALSE,
       "cat_2", "  sub_cat_3"    ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)",FALSE,
       "cat_3", "cat_3"          ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)",FALSE,
       "cat_3", "sub_cat_3a"     ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)",FALSE,
       "cat_3", "sub_cat_3b"     ,NA         ,NA         ,NA        ,TRUE,
       "cat_3", "  sub_cat_3b_1" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)",FALSE,
       "cat_3", "  sub_cat_3b_3" ,"xx (xx%)" ,"xx (xx%)" ,"xx (xx%)",FALSE
    ) %>% group_by(grp1)
  )

})

test_that("row order is retained for all selections",{

  dat <- tibble::tribble(
    ~grp1, ~grp2, ~lbl, ~prm, ~column, ~val, ~ord,
    "d",   "c",   "n", "n",   1,   1,  1,
    "a",   "b",   "m", "n",   1,   2,  2,
    "q",   "v",   "s", "n",   1,   3,  3,
    "b",   "p",   "e", "n",   1,   4,  4
  )

  tfrmt_temp <- tfrmt(
    group = c(grp1, grp2),
    label = lbl,
    column = column,
    value = val,
    param = prm,
    sorting_cols = ord,
    col_plan = col_plan(-ord),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("x"))
    )
  )

  gt_indented <-  tfrmt_temp %>%
    tfrmt(
      row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented"))
    ) %>%
    print_to_gt(dat)


  gt_indented_dat <- gt_indented$`_data`
  gt_indented_man <- tibble::tribble(
    ~lbl,       ~`1`, ~..tfrmt_row_grp_lbl,
    "d"       ,NA ,  TRUE,
    "  c"    ,NA,  TRUE,
    "    n"  ,"1", FALSE,
    "a"      ,NA , TRUE,
    "  b"    ,NA , TRUE,
    "    m"  ,"2", FALSE,
    "q"      ,NA , TRUE,
    "  v"    ,NA , TRUE,
    "    s"  ,"3", FALSE,
    "b"      ,NA , TRUE,
    "  p"    ,NA , TRUE,
    "    e"  ,"4", FALSE )

  expect_equal(gt_indented_dat, gt_indented_man, ignore_attr = c(".col_plan_vars",".footnote_locs"))


  gt_spanning <-  tfrmt_temp %>%
    tfrmt(
      row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "spanning"))
    ) %>%
    print_to_gt(dat)

  gt_spanning_dat <- gt_spanning$`_data`
  gt_spanning_man <- tibble::tribble(
    ~ grp1,  ~lbl,       ~`1`, ~..tfrmt_row_grp_lbl,
      "d",     "c"   , NA , TRUE,
      "d",     "  n" , "1", FALSE,
      "a",     "b"   , NA , TRUE,
      "a",     "  m" , "2", FALSE,
      "q",     "v"   , NA , TRUE,
      "q",     "  s" , "3", FALSE,
      "b",     "p"   , NA , TRUE,
      "b",     "  e" , "4", FALSE)
  expect_equal(gt_spanning_dat, gt_spanning_man)

  gt_column <-  tfrmt_temp %>%
    tfrmt(
      row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "column"))
    ) %>%
    print_to_gt(dat)

  gt_column_dat <- gt_column$`_data`
  expect_equal(gt_column_dat, gt_spanning_man)

  # original order also respected if no order variable supplied
  tfrmt(
    group = c(grp1, grp2),
    label = lbl,
    column = column,
    value = val,
    param = prm,
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("x"))
    ),
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented"))
    ) %>%
    print_to_gt(dat %>% select(-ord))


  gt_indented_dat <- gt_indented$`_data`
  gt_indented_man <- tibble::tribble(
    ~lbl,       ~`1`, ~..tfrmt_row_grp_lbl,
    "d"       ,NA ,  TRUE,
    "  c"    , NA, TRUE,
    "    n"  ,"1", FALSE,
    "a"      ,NA , TRUE,
    "  b"    ,NA , TRUE,
    "    m"  ,"2", FALSE,
    "q"      ,NA , TRUE,
    "  v"    ,NA , TRUE,
    "    s"  ,"3", FALSE,
    "b"      ,NA , TRUE,
    "  p"    ,NA , TRUE,
    "    e"  ,"4",  FALSE)
  expect_equal(gt_indented_dat, gt_indented_man, ignore_attr = c(".col_plan_vars",".footnote_locs"))
})


test_that("Row group plans with col style plan",{


  raw_dat <- tibble::tribble(
      ~g1,  ~g2,       ~one,   ~param, ~column, ~ value,
     "G1", "g3",    "n (%)",      "n",  "trt1",      12,
     "G1", "g3",    "n (%)",    "pct",  "trt1",      34,
     "G2_", "g3",     "mean",   "mean",  "trt1",    12.3,
     "G2_", "g3",       "sd",     "sd",  "trt1",    4.34,
     "G2_", "g3",   "median", "median",  "trt1",      14,
     "G3", "g3", "(q1, q3)",     "q1",  "trt1",      10,
     "G3", "g3", "(q1, q3)",     "q3",  "trt1",      20,
     "G1", "g3",    "n (%)",      "n",  "trt2",      24,
     "G1", "g3",    "n (%)",    "pct",  "trt2",      58,
     "G2_", "g3",     "mean",   "mean",  "trt2",    15.4,
     "G2_", "g3",       "sd",     "sd",  "trt2",    8.25,
     "G2_", "g3",   "median", "median",  "trt2",      16,
     "G3", "g3", "(q1, q3)",     "q1",  "trt2",      22,
     "G3", "g3", "(q1, q3)",     "q3",  "trt2",      22,
     "G1", "g3",     "mean",   "pval",  "four",   0.0001
  )

  plan <- tfrmt(
    label = one,
    group = c(g1,g2),
    column = vars(column),
    value = value,
    param = param,
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",label_val = ".default",
        frmt("xx.xx")
      ),
      frmt_structure(
        group_val = ".default",label_val = "n (%)",
        frmt_combine("{n} ({pct}%)",
                     n = frmt("x"),
                     pct = frmt("xx.x"))
      ),
      frmt_structure(
        group_val = ".default",label_val = "(q1, q3)",
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
      )
    ),
    row_grp_plan = row_grp_plan(
      row_grp_structure(
        group_val = list(g1 = c("G1","G2_"), g2 = ".default"),
        element_block = element_block(post_space = "----")
      ),
      label_loc = element_row_grp_loc(location = "column")
    ),
    col_style_plan =  col_style_plan(
      col_style_structure(align = "right", col = g1), # col must be the top lebel group
      col_style_structure(align = "right", col = one), # always bueno
      col_style_structure(align = "right", width = 4, col = vars(starts_with("trt"))),
      col_style_structure(align = "left", col = trt1),
      col_style_structure(width = 10, col = four)
    )
  )

  ## suppressing warning from alignment using multiple values. Not pertinent to this test
  suppressWarnings({
    tfrmt_gt <- print_to_gt(plan, raw_dat)
  })

  expect_equal(
    tfrmt_gt$`_data` %>%
      select(-`..tfrmt_row_grp_lbl`) %>%
      as.list(),
    list(
      g1 = c(" G1", " G1", " G1", " G1", "G2_", "G2_", "G2_", "G2_", "G2_", " G3", " G3"),
      one = c( "        g3", "     n (%)", "      mean", "  --------","        g3", "      mean", "        sd", "    median", "  --------", "        g3", "  (q1, q3)"),
      trt1 = c(NA, "12 (34.0%)", "          ", "----------", NA, "12.30     ", " 4.34     ", "14.00     ", "----------",  NA, "(10, 20)  "),
      trt2 = c(NA, "24\n(58.0%)", "          ", "----------", NA, "     15.40", "      8.25", "     16.00", "----------", NA, "  (22,\n  22)"),
      four = c( NA, "", "<.001", "-----",NA, "", "", "", "-----", NA, "")
      )
  )


})

test_that("Row group plans with col style plan - error checks against group",{

  expect_error({
      tfrmt(
        group = c(g1,g2),
        row_grp_plan = row_grp_plan(
          row_grp_structure(
            group_val = list(g1 = c("G1","G2_"), g2 = ".default"),
            element_block = element_block(post_space = "----")
          ),
          label_loc = element_row_grp_loc(location = "column")
        ),
        col_style_plan =  col_style_plan(
          col_style_structure(align = "right", col = g2), # col must be the top lebel group
          col_style_structure(align = "right", col = one), # always bueno
          col_style_structure(align = "right", width = 200, col = vars(starts_with("trt"))),
          col_style_structure(align = c(" "), col = trt1),
          col_style_structure(width = 100, col = four)
        )
      )
    },
    paste(
      "Invalid col_style_structure in row_grp_plan at position `1`:",
      "  `col` value: g2",
      "  When row_grp_plan label location is `column`, only the only valid group col to style is `g1`",
      sep = "\n"
    ),
    fixed = TRUE
  )

})

test_that("Suppress printing of groups", {

  mock_multi_grp <- tibble::tribble(
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


  my_plan <- row_grp_plan(label_loc = element_row_grp_loc(location = "noprint"))

  df_no_grp <- tibble::tribble(
    ~my_label,   ~trtA,     ~trtB,     ~trtC  ,
    "my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_1", "xx (xx%)", "xx (xx%)", "xx (xx%)",
    "my_label_2", "xx (xx%)", "xx (xx%)", "xx (xx%)",
  )

  expect_equal(apply_row_grp_lbl(mock_multi_grp, my_plan$label_loc, vars(grp1, grp2), sym("my_label")) %>%
                 remove_grp_cols(my_plan$label_loc, vars(grp1, grp2)),
               df_no_grp)
})


test_that("Row group plan indenting handles factor variables", {

  dat <- tibble::tribble(
    ~grp_span, ~grp,      ~rowlbl,    ~column, ~param, ~value,
    "span", "topgrp1", "topgrp1"  ,  "A",  "mean",   1,
    "span", "topgrp1", "topgrp1"  ,  "B",  "mean",   1,
    "span", "topgrp1", "lowergrp1",  "A",  "mean",   1,
    "span", "topgrp1", "lowergrp1",  "B",  "mean",   2,
    "span", "topgrp2", "topgrp2"  ,  "A",  "mean",   3,
    "span", "topgrp2", "topgrp2"  ,  "B",  "mean",   4,
    "span", "topgrp2", "lowergrp1",  "A",  "mean",   2,
    "span", "topgrp2", "lowergrp1",  "B",  "mean",   1,
  )

  grp_plan <- row_grp_plan(
  )

  expected <-  tibble::tribble(
    ~ rowlbl,  ~ column,  ~param,  ~value, ~..tfrmt_row_grp_lbl,
    "span"          , NA ,    NA    ,   NA  , TRUE   ,
    "  topgrp1"     ,"A",   "mean",      1 , FALSE,
    "  topgrp1"     ,"B",   "mean",      1 , FALSE,
    "    lowergrp1" ,"A",   "mean",      1 , FALSE,
    "    lowergrp1" ,"B",   "mean",      2 , FALSE,
    "  topgrp2"     ,"A",   "mean",      3 , FALSE,
    "  topgrp2"     ,"B",   "mean",      4 , FALSE,
    "    lowergrp1" ,"A",   "mean",      2 , FALSE,
    "    lowergrp1" ,"B",   "mean",      1 , FALSE)

  expect_equal(
    apply_row_grp_lbl(dat %>% mutate(across(grp_span:rowlbl, as.factor)),
                      grp_plan$label_loc, vars(grp_span, grp), sym("rowlbl")) %>%
      remove_grp_cols(grp_plan$label_loc, vars(grp_span, grp)),
    expected)

  expect_equal(
    apply_row_grp_lbl(dat %>% mutate(across(rowlbl, as.factor)),
                      grp_plan$label_loc, vars(grp_span, grp), sym("rowlbl")) %>%
      remove_grp_cols(grp_plan$label_loc, vars(grp_span, grp)),
    expected)

  expect_equal(
    apply_row_grp_lbl(dat %>% mutate(across(grp, as.factor)),
                      grp_plan$label_loc, vars(grp_span, grp), sym("rowlbl")) %>%
      remove_grp_cols(grp_plan$label_loc, vars(grp_span, grp)),
    expected)

  expect_equal(
    apply_row_grp_lbl(dat %>% mutate(across(grp_span, as.factor)),
                      grp_plan$label_loc, vars(grp_span, grp), sym("rowlbl")) %>%
      remove_grp_cols(grp_plan$label_loc, vars(grp_span, grp)),
    expected)
})
