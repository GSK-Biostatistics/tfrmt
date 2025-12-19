test_that("apply_col_pla() works", {
  df1 <- tibble::tribble(
    ~grp2 , ~lbl , ~prm  , ~column , ~val , ~ord ,
    "c"   , "n"  , "n"   ,       1 , 1    ,    1 ,
    "c"   , "n"  , "n_2" ,       1 , 1.1  ,    1 ,
    "b"   , "m"  , "n"   ,       1 , 2    ,    2 ,
    "b"   , "m"  , "n_2" ,       1 , 2.1  ,    2 ,
    "v"   , "s"  , "n"   ,       1 , 3    ,    3 ,
    "v"   , "s"  , "n_3" ,       1 , 3.3  ,    3 ,
    "p"   , "e"  , "n"   ,       1 , 4    ,    4
  )

  expect_snapshot(
    apply_col_plan(
      df1,
      col_selection = "-ord",
      grp_lbl = rlang::new_quosures(
        list(
          quo(grp2),
          quo(lbl)
        )
      )
    )
  )

  processed_df1 <- apply_col_plan(
    df1,
    col_selection = "-ord",
    grp_lbl = rlang::new_quosures(
      list(
        quo(grp2),
        quo(lbl)
      )
    )
  )

  expect_identical(
    processed_df1,
    dplyr::select(df1, -ord)
  )
})

test_that("apply_col_plan() group and label vars are excluded from renaming", {
  df1 <- tibble::tribble(
    ~grp2 , ~lbl , ~prm  , ~column , ~val , ~ord ,
    "c"   , "n"  , "n"   ,       1 , 1    ,    1 ,
    "c"   , "n"  , "n_2" ,       1 , 1.1  ,    1 ,
    "b"   , "m"  , "n"   ,       1 , 2    ,    2 ,
    "b"   , "m"  , "n_2" ,       1 , 2.1  ,    2 ,
    "v"   , "s"  , "n"   ,       1 , 3    ,    3 ,
    "v"   , "s"  , "n_3" ,       1 , 3.3  ,    3 ,
    "p"   , "e"  , "n"   ,       1 , 4    ,    4
  )

  expect_snapshot(
    apply_col_plan(
      df1,
      col_selection = c("-ord", "lbl"),
      grp_lbl = rlang::new_quosures(
        list(
          quo(grp2),
          quo(lbl)
        )
      )
    )
  )

  processed_df1 <- apply_col_plan(
    df1,
    col_selection = c("-ord", "lbl"),
    grp_lbl = rlang::new_quosures(
      list(
        quo(grp2),
        quo(lbl)
      )
    )
  )

  expect_identical(
    processed_df1,
    dplyr::select(df1, -ord)
  )
})
