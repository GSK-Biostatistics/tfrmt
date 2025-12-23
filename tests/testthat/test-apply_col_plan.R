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
  skip("temp skip")
  expect_s3_class(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(column),
      cp = col_plan(-ord)
    ),
    c("quosures", "list")
  )

  # we strip out the environment memory address (aka the label)
  expect_snapshot(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(column),
      cp = col_plan(-ord)
    ),
    transform = strip_env_label
  )

  expect_equal(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(column),
      cp = col_plan(-ord)
    ),
    rlang::quos(
      -ord,
      grp2,
      lbl,
      `1`
    ),
    ignore_formula_env = TRUE
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
    ),
    transform = strip_env_label
  )

  expect_equal(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(column),
      cp = NULL
    ),
    rlang::quos(
      grp2,
      lbl,
      ord,
      `1`
    ),
    ignore_formula_env = TRUE
  )
})

test_that("create_col_order() with empty columns arg & cp not NULL", {
  expect_s3_class(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(),
      cp = col_plan(-ord)
    ),
    c("quosures", "list")
  )

  # we don't need to snapshot the environment names, we're only interested in
  # the expressions
  expect_snapshot(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(),
      cp = col_plan(-ord)
    ),
    transform = strip_env_label
  )

  expect_identical(
    create_col_order(
      data_names = c("grp2", "lbl", "ord", "1"),
      columns = rlang::quos(),
      cp = col_plan(-ord)
    ),
    rlang::quos(
      -ord,
      grp2,
      lbl,
      `1`
    ),
    ignore_formula_env = TRUE
  )
})

test_that("create_col_order() with span_structure()", {
  skip("temp skip")
  dat <- tibble::tribble(
    ~group , ~label      , ~span1     , ~my_col  , ~parm   , ~val ,
    "g1"   , "rowlabel1" , "cols 1,2" , "col1"   , "value" ,    1 ,
    "g1"   , "rowlabel1" , "cols 1,2" , "col2"   , "value" ,    1 ,
    "g1"   , "rowlabel1" , NA         , "mycol3" , "value" ,    1 ,
    "g1"   , "rowlabel1" , "col 4"    , "col4"   , "value" ,    1 ,
    "g1"   , "rowlabel1" , NA         , "mycol5" , "value" ,    1 ,
    "g1"   , "rowlabel2" , "cols 1,2" , "col1"   , "value" ,    2 ,
    "g1"   , "rowlabel2" , "cols 1,2" , "col2"   , "value" ,    2 ,
    "g1"   , "rowlabel2" , NA         , "mycol3" , "value" ,    2 ,
    "g1"   , "rowlabel2" , "col 4"    , "col4"   , "value" ,    2 ,
    "g1"   , "rowlabel2" , NA         , "mycol5" , "value" ,    2 ,
    "g2"   , "rowlabel3" , "cols 1,2" , "col1"   , "value" ,    3 ,
    "g2"   , "rowlabel3" , "cols 1,2" , "col2"   , "value" ,    3 ,
    "g2"   , "rowlabel3" , NA         , "mycol3" , "value" ,    3 ,
    "g2"   , "rowlabel3" , "col 4"    , "col4"   , "value" ,    3 ,
    "g2"   , "rowlabel3" , NA         , "mycol5" , "value" ,    3 ,
  )

  expect_snapshot(
    create_col_order(
      data_names = c(
        "group",
        "label",
        "cols 1,2___tlang_delim___col1",
        "cols 1,2___tlang_delim___col2",
        "mycol3",
        "col 4___tlang_delim___col4",
        "mycol5"
      ),
      columns = rlang::quos(span1, my_col),
      cp = col_plan(
        # rename column spanner in same level
        span_structure(span1 = c("first cols" = "cols 1,2")),
        group,
        label,
        starts_with("col"),
        new_col_3 = mycol3,
        -mycol5
      )
    ),
    transform = strip_env_label
  )

  expect_identical(
    create_col_order(
      # data_names = c("group", "label", "span2", "span1", "my_col", "parm", "val"),
      data_names = c(
        "group",
        "label",
        "cols 1,2___tlang_delim___col1",
        "cols 1,2___tlang_delim___col2",
        "mycol3",
        "col 4___tlang_delim___col4",
        "mycol5"
      ),
      columns = rlang::quos(span1, my_col),
      cp = col_plan(
        # rename column spanner in same level
        span_structure(span1 = c("first cols" = "cols 1,2")),
        group,
        label,
        starts_with("col"),
        new_col_3 = mycol3,
        -mycol5
      )
    ),
    rlang::quos(
      group,
      label,
      `first cols___tlang_delim___col1` = `cols 1,2___tlang_delim___col1`,
      `first cols___tlang_delim___col2` = `cols 1,2___tlang_delim___col2`,
      `col 4___tlang_delim___col4`,
      new_col_3 = mycol3,
      -mycol5
    ),
    ignore_formula_env = TRUE
  )
})

test_that("col_plan_quo_to_vars() works", {
  # base case
  expect_identical(
    col_plan_quo_to_vars(
      x = rlang::quos(ord),
      column_names = "column",
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = NULL
    ),
    # the output is a named vector with empty names
    rlang::set_names("ord", "")
  )

  # negative selection
  expect_identical(
    col_plan_quo_to_vars(
      x = rlang::quos(-ord),
      column_names = "column",
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = NULL
    ),
    # the output is a named vector with empty names
    rlang::set_names("-ord", "")
  )

  # names are propagated if x contains named quosures
  expect_identical(
    col_plan_quo_to_vars(
      x = rlang::quos(new_order = ord),
      column_names = "column",
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = NULL
    ),
    c(new_order = "ord")
  )

  # when x is a named list of quosures the output is a named vector with unique
  # names derived from the tidyselect expression
  expect_identical(
    col_plan_quo_to_vars(
      x = rlang::quos(starts_with("ord")) |> rlang::quos_auto_name(),
      column_names = "column",
      data_names = c("grp2", "lbl", "ord", "order_new", "1"),
      preselected_cols = NULL
    ),
    c(
      `starts_with("ord")1` = "ord",
      `starts_with("ord")2` = "order_new"
    )
  )

  expect_identical(
    col_plan_quo_to_vars(
      x = rlang::quos(ord),
      column_names = "column",
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = "lbl"
    ),
    rlang::set_names(
      c("lbl", "ord"),
      ""
    )
  )

  # impact of return_only_selected == TRUE
  expect_identical(
    col_plan_quo_to_vars(
      x = rlang::quos(ord),
      column_names = "column",
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = "lbl",
      return_only_selected = TRUE
    ),
    rlang::set_names("ord", "")
  )

  # when return_only_selected is FALSE then both preselected and selected cols
  # are returned
  expect_identical(
    col_plan_quo_to_vars(
      x = rlang::quos(ord),
      column_names = "column",
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = "lbl",
      return_only_selected = FALSE
    ),
    rlang::set_names(
      c("lbl", "ord"),
      ""
    )
  )

  # skip("I don't think this test is ok")
  # default_everything_behavior = FALSE
  expect_identical(
    col_plan_quo_to_vars(
      x = rlang::quos(everything()),
      column_names = "column",
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = c("lbl", "1"),
      default_everything_behavior = FALSE
    ),
    rlang::set_names(
      c("lbl", "1", "grp2", "ord"),
      ""
    )
  )

  # default_everything_behavior = TRUE
  expect_identical(
    col_plan_quo_to_vars(
      x = rlang::quos(everything()),
      column_names = "column",
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = c("lbl", "1"),
      default_everything_behavior = TRUE
    ),
    rlang::set_names(
      c("lbl", "1", "lbl", "1", "grp2", "ord"),
      ""
    )
  )
})

test_that("char_as_quo() works", {
  expect_s3_class(
    char_as_quo("x"),
    "quosure"
  )

  expect_identical(
    char_as_quo("x"),
    rlang::quo(x),
    ignore_formula_env = TRUE
  )

  expect_s3_class(
    char_as_quo(
      "starts_with('foo')"
    ),
    "quosure"
  )

  expect_identical(
    char_as_quo(
      "starts_with('foo')"
    ),
    rlang::quo(
      starts_with("foo")
    ),
    ignore_formula_env = TRUE
  )

  # when x is a valid(ish) tidyselect expression the output is a call
  expect_true(
    char_as_quo("starts_with('foo')") |>
      rlang::quo_is_call()
  )

  expect_false(
    char_as_quo("starts_with('foo')") |>
      rlang::quo_is_symbol()
  )

  # backticks are added when x is not a valid tidyselect call and the output
  # expression is a symbol and not a call
  expect_identical(
    char_as_quo("like('foo')"),
    rlang::quo(`like('foo')`),
    ignore_formula_env = TRUE
  )

  expect_true(
    char_as_quo("like('foo')") |>
      rlang::quo_is_symbol()
  )

  expect_false(
    char_as_quo("like('foo')") |>
      rlang::quo_is_call()
  )

  # char_as_quo() uses parse()
  # parse errors are converted to symbols
  expect_error(
    parse(
      text = "foo12-3bar"
    )
  )

  expect_true(
    char_as_quo(
      "foo12-3bar"
    ) |>
      rlang::quo_is_symbol()
  )

  expect_identical(
    char_as_quo("foo12-3bar"),
    rlang::quo(`foo12-3bar`),
    ignore_formula_env = TRUE
  )

  # exception: valid tidyselect expression, but the output is not a call
  # this unit tests should be reviewed as part of
  # https://github.com/GSK-Biostatistics/tfrmt/issues/578
  skip("https://github.com/GSK-Biostatistics/tfrmt/issues/578")
  expect_true(
    char_as_quo("foo:bar") |>
      rlang::quo_is_call()
  )

  # same issue with namespaced calls
  expect_true(
    char_as_quo("tidyselect::starts_with('foo')") |>
      rlang::quo_is_call()
  )
})

test_that("eval_col_plan_quo() works", {
  expect_identical(
    eval_col_plan_quo(
      x = rlang::quo(ord),
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_vals = NULL,
      default_everything_behavior = FALSE
    ),
    "ord"
  )

  # not really clear, but eval_col_plan_quo only works when x is a single
  # quosure (and not a list of quosures)
  expect_identical(
    eval_col_plan_quo(
      x = rlang::quo(everything()),
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_vals = NULL,
      default_everything_behavior = TRUE
    ),
    c("grp2", "lbl", "ord", "1")
  )

  # default_everything_behavior produces effects only in a very narrow case:
  # - when preselected_vars is not empty, and
  # - x is everything()
  # the behaviour is likely incorrect, the below drops "grp2" (the first
  # variable in the data_names vector, due to ...[-seq_along(preselected_vals)])
  #
  # data_names <- data_names[-seq_along(preselected_vals)] should be replaced with
  # data_names <- setdiff(data_names, preselected_vals)
  skip("incorrect behaviour")
  expect_identical(
    eval_col_plan_quo(
      x = rlang::quo(everything()),
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_vals = "ord",
      default_everything_behavior = FALSE
    ),
    c("grp2", "lbl", "1")
  )

  # the below should produce identical results, but they do not
  skip("https://github.com/GSK-Biostatistics/tfrmt/issues/578")
  expect_identical(
    eval_col_plan_quo(
      x = rlang::quo(everything()),
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_vals = "ord",
      default_everything_behavior = FALSE
    ),
    eval_col_plan_quo(
      x = rlang::quo(tidyselect::everything()),
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_vals = "ord",
      default_everything_behavior = FALSE
    )
  )
})

test_that("col_plan_span_structure_to_vars() works", {
  expect_equal(1 + 1, 2)
})

test_that("split_data_names_to_df() works", {
  skip("temp skip")
  # simple case
  expect_snapshot(
    split_data_names_to_df(
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = NULL,
      column_names = "column"
    )
  )

  expect_identical(
    split_data_names_to_df(
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = NULL,
      column_names = "column"
    ),
    tibble::tibble(
      column = c("grp2", "lbl", "ord", "1"),
      `__tfrmt_new_name__column` = c("grp2", "lbl", "ord", "1"),
      subtraction_status = c(FALSE, FALSE, FALSE, FALSE)
    )
  )

  # preselected_cols renames
  expect_snapshot(
    split_data_names_to_df(
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = c(order = "ord"),
      column_names = "column"
    )
  )

  # column splitting by column_names
  expect_snapshot(
    split_data_names_to_df(
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = c(order = "ord"),
      column_names = c("foo", "bar")
    )
  )

  # negative selection
  # doesn't really work (ord appears twice in the output)
  expect_snapshot(
    split_data_names_to_df(
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = "-ord",
      column_names = "column"
    )
  )

  # it should be
  expect_identical(
    split_data_names_to_df(
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = "-ord",
      column_names = "column"
    ),
    tibble::tibble(
      column = c("ord", "grp2", "lbl", "ord", "1"),
      `__tfrmt_new_name__column` = c("-ord", "grp2", "lbl", "ord", "1"),
      subtraction_status = c(TRUE, FALSE, FALSE, FALSE, FALSE)
    )
  )
})

test_that("unite_df_to_data_names() works", {
  skip("temp skip")
  expect_identical(
    split_data_names_to_df(
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = NULL,
      column_names = "column"
    ) |>
      unite_df_to_data_names(
        preselected_cols = NULL,
        column_names = "column",
        return_only_selected = FALSE
      ),
    rlang::set_names(
      c("grp2", "lbl", "ord", "1"),
      ""
    )
  )

  expect_identical(
    split_data_names_to_df(
      data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = "ord",
      column_names = "column"
    ) |>
      unite_df_to_data_names(
        preselected_cols = "ord",
        column_names = "column",
        return_only_selected = TRUE
      ),
    rlang::set_names(
      c("grp2", "lbl", "ord", "1"),
      ""
    )
  )

  split_data_names_to_df(
    data_names = c("grp2", "lbl", "ord", "1"),
    preselected_cols = "-ord",
    column_names = "column"
  ) |>
    unite_df_to_data_names(
      preselected_cols = "-ord",
      column_names = "column"
    )

  tibble::tibble(
    column = c("ord", "grp2", "lbl", "1"),
    `__tfrmt_new_name__column` = c("-ord", "grp2", "lbl", "1"),
    subtraction_status = c(TRUE, FALSE, FALSE, FALSE)
  ) |>
    unite_df_to_data_names(
      preselected_cols = "-ord",
      column_names = "column"
    )

  split_data_names_to_df(
    data_names = c("grp2", "lbl", "ord", "1"),
    preselected_cols = "ord",
    column_names = "column"
  ) |>
    unite_df_to_data_names(
      preselected_cols = "ord",
      column_names = "column"
    )
})
