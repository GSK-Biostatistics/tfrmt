test_that("apply_col_plan() works", {
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
      grp_lbl = rlang::quos(grp2, lbl)
    )
  )

  processed_df1 <- apply_col_plan(
    df1,
    col_selection = "-ord",
    grp_lbl = rlang::quos(grp2, lbl)
  )

  expect_identical(
    processed_df1,
    dplyr::select(df1, -ord)
  )

  # col_selection can be either character or a list of quosures
  expect_identical(
    apply_col_plan(
      df1,
      col_selection = "-ord",
      grp_lbl = rlang::quos(grp2, lbl)
    ),
    apply_col_plan(
      df1,
      col_selection = rlang::quos(-ord),
      grp_lbl = rlang::quos(grp2, lbl)
    )
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
      grp_lbl = rlang::quos(grp2, lbl)
    )
  )

  processed_df1 <- apply_col_plan(
    df1,
    col_selection = c("-ord", "lbl"),
    grp_lbl = rlang::quos(grp2, lbl)
  )

  expect_identical(
    processed_df1,
    dplyr::select(df1, -ord)
  )
})

test_that("create_stub_head() works", {
  col_plan_vars <- rlang::quos(-ord, grp2, lbl, `1`)

  group <- rlang::quos(grp2)

  label <- rlang::quos(lbl)

  row_grp_plan_label_loc <- "indented"

  expect_snapshot(
    create_stub_head(
      col_plan_vars = rlang::quos(-ord, grp2, lbl, `1`),
      group = rlang::quos(grp2),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "indented"
    )
  )

  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(-ord, grp2, lbl, `1`),
      group = rlang::quos(grp2),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "indented"
    ),
    ""
  )

  expect_snapshot(
    create_stub_head(
      col_plan_vars = rlang::quos(-ord, grp2, lbl, `1`),
      group = rlang::quos(grp2),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "column"
    )
  )

  expect_identical(
    create_stub_head(
      col_plan_vars = rlang::quos(-ord, grp2, lbl, `1`),
      group = rlang::quos(grp2),
      label = rlang::quos(lbl),
      row_grp_plan_label_loc = "column"
    ),
    c("", "")
  )
})

test_that("create_col_order() works", {
  expect_s3_class(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(column),
      cp = col_plan(-ord)
    ),
    c("quosures", "list")
  )

  # we don't need to snapshot the environment names, we're only interested in
  # the expressions
  expect_snapshot(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(column),
      cp = col_plan(-ord)
    ) |>
      purrr::map(rlang::quo_get_expr)
  )

  expect_identical(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(column),
      cp = col_plan(-ord)
    ) |>
      purrr::map(rlang::quo_get_expr),
    rlang::exprs(
      -ord,
      grp2,
      lbl,
      `1`
    )
  )
})

test_that("create_col_order() with NULL col plan", {
  # the output is a list of the data_names as quosures
  expect_s3_class(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(column),
      cp = NULL
    ),
    c("quosures", "list")
  )

  # we don't need to snapshot the environment names, we're only interested in
  # the expressions
  expect_snapshot(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(column),
      cp = NULL
    ) |>
      purrr::map(rlang::quo_get_expr)
  )

  expect_identical(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(column),
      cp = NULL
    ) |>
      purrr::map(rlang::quo_get_expr),
    rlang::exprs(
      grp2,
      lbl,
      ord,
      `1`
    )
  )
})
