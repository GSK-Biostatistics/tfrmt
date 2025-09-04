test_that("prep_...() pipe with demographic data", {

  # data prep -------------------------------------------------------------
  adsl <- pharmaverseadam::adsl |>
    dplyr::filter(SAFFL == "Y") |>
    dplyr::select(USUBJID, ARM, SEX, AGE, AGEGR1, ETHNIC, RACE) |>
    dplyr::mutate(
      SEX = factor(
        SEX,
        levels = c("F", "M")
      ),
      AGEGR1 = factor(
        AGEGR1,
        levels = c("<18", "18-64", ">64"),
        labels = c("<=18 years", "Between 18 and 65 years", ">=65 years")
      ),
      ETHNIC = factor(
        ETHNIC,
        levels = c(
          "HISPANIC OR LATINO",
          "NOT HISPANIC OR LATINO",
          "NOT REPORTED"
        )
      ),
      RACE = factor(
        RACE,
        levels = c(
          "AMERICAN INDIAN OR ALASKA NATIVE",
          "ASIAN",
          "BLACK OR AFRICAN AMERICAN",
          "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
          "WHITE"
        )
      )
    )

  # create card -----------------------------------------------------------
  ard <- cards::ard_stack(
    data = adsl,
    .by = ARM,
    cards::ard_continuous(
      variables = AGE,
      statistic = ~ cards::continuous_summary_fns(
        c("N", "mean", "sd", "min", "max")
      )
    ),
    cards::ard_categorical(
      variables = c(AGEGR1, SEX, ETHNIC, RACE)
    ),
    .overall = TRUE,
    .total_n = TRUE
  )


  # manual tidy -------------------------------------------------------------
  ard_tbl <- ard |>
    # rename cols, coalesce variable levels/stat_labels, unnest
    cards::rename_ard_columns(columns = c("group1")) |>
    cards::unlist_ard_columns() |>
    dplyr::mutate(
      label = purrr::map_chr(
        variable_level,
        ~ {
          # Assign NA if the element is empty
          if (length(.x) == 0) {
            return(NA_character_)
          }

          as.character(.x[[1]]) # Convert to character
        }
      )
    ) |>
    # set missing labels to stat_lavel
    dplyr::mutate(
      label = dplyr::if_else(is.na(label), stat_label, label),
      # Add big N labels
      ARM = dplyr::if_else(
        variable == "ARM",
        label,
        ARM
      ),
      # ID total trt
      ARM = dplyr::if_else(
        is.na(ARM) | variable == "..ard_total_n..",
        "Overall ARM",
        ARM
      ),
      # unique stat names for big N's
      stat_name = dplyr::if_else(
        (variable == "ARM" & stat_name == "n") | variable == "..ard_total_n..",
        "bigN",
        stat_name
      ),
      # relabel the label for n's
      label = dplyr::if_else(
        stat_name == "N",
        "n",
        label
      )
    ) |>
    # remove unneeded stats
    dplyr::filter(
      !(variable == "ARM" & stat_name != "bigN")
    ) |>
    # sorting (mostly handled by factors, above)
    dplyr::mutate(
      ord1 = forcats::fct_inorder(variable) |>
        forcats::fct_relevel("SEX", after = 0) |>
        as.numeric(),
      ord2 = dplyr::if_else(label == "n", 1, 2)
    ) |>
    # drop variables not needed
    dplyr::select(ARM, variable, label, stat_name, stat, ord1, ord2) |>
    # remove dups (extra denoms per variable level)
    unique()

  # the tfrmt table -------------------------------------------------------
  dm_t01 <- tfrmt(
    title = "Summary of Demographic Characteristics",
    group = variable,
    label = label,
    param = stat_name,
    value = stat,
    column = ARM,
    sorting_cols = c(ord1, ord2),
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt("xxx")
      ),
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{n} ({p}%)",
          n = frmt("xxx"),
          p = frmt("xx", transform = ~ . * 100)
        )
      )
    ),
    big_n = big_n_structure(
      param_val = "bigN",
      n_frmt = frmt("\n(N=xx)")
    ),
    col_plan = col_plan(
      -starts_with("ord")
    ),
    col_style_plan = col_style_plan(
      col_style_structure(
        col = everything(),
        align = "left"
      )
    ),
    row_grp_plan = row_grp_plan(
      row_grp_structure(
        group_val = ".default",
        element_block(
          post_space = " "
        )
      )
    )
  )

  a <- print_to_gt(dm_t01, ard_tbl)
  a_html <- a |>
    gt::as_raw_html() |>
    # the id is randomly generated
    strip_id()


  # tidy with `prep_...()` functions ----------------------------------------
  prepped_ard <- ard |>
    shuffle_card(
      by = "ARM"
    ) |>
    prep_combine_vars(
      c("AGE", "AGEGR1", "SEX", "ETHNIC", "RACE")
    ) |>
    prep_big_n(
      vars = "ARM"
    ) |>
    prep_label() |>
    dplyr::mutate(
      label = dplyr::if_else(
        .data$stat_name == "N",
        "n",
        .data$label
      ),
      variable_level = dplyr::if_else(
        .data$stat_name == "N",
        NA,
        .data$variable_level
      )
    ) |>
    unique() |>
    dplyr::mutate(
      # we need these additional steps for compatibility with the manual
      # processing
      label = dplyr::if_else(
        .data$stat_name == "bigN" & .data$context == "categorical",
        ARM,
        .data$label
      ),
      ord1 = forcats::fct_inorder(stat_variable) |>
        forcats::fct_relevel("SEX", after = 0) |>
        as.numeric(),
      ord2 = dplyr::if_else(label == "n", 1, 2)
    ) |>
    dplyr::select(
      ARM,
      variable = stat_variable,
      label,
      stat_name,
      stat,
      ord1,
      ord2
    )

  # expect_identical(
  #   dplyr::arrange(ard_tbl, ord1, ord2),
  #   dplyr::arrange(prepped_ard, ord1, ord2)
  # )

  expect_no_error(
    b <- print_to_gt(dm_t01, prepped_ard)
  )

  b_html <- b |>
    gt::as_raw_html() |>
    # the id is randomly generated so we strip it
    strip_id()

  expect_identical(
    a_html,
    b_html
  )

  expect_identical(
    gt::extract_body(a),
    gt::extract_body(b)
  )

  expect_snapshot(
    gt::as_raw_html(b),
    transform = strip_id
  )
})

test_that("prep_...() pipe with adverse effects data", {

  # data prep -------------------------------------------------------------
  # Filter to include only subjects marked as part of the safety population
  adsl <- pharmaverseadam::adsl |>
    dplyr::filter(SAFFL == "Y")

  #subset data to limit printed rows for demo purposes
  adae <- pharmaverseadam::adae |>
    dplyr::filter(AESOC %in% unique(AESOC)[1:3]) |>
    dplyr::group_by(AESOC) |>
    dplyr::filter(AEDECOD %in% unique(AEDECOD)[1:3]) |>
    dplyr::ungroup()

  # create card -----------------------------------------------------------
  suppressMessages(
    ae_ard <- cards::ard_stack_hierarchical(
      data = adae,
      by = c(TRT01A, AESEV),
      variables = c(AEBODSYS, AETERM),
      statistic = ~ c("n", "p"), # Calculate count and percentage
      denominator = adsl,
      id = USUBJID,
      over_variables = TRUE,
      total_n = TRUE
    )
  )


  # manual tidy -------------------------------------------------------------
  ae2_ard_tbl <- ae_ard |>
    cards::rename_ard_columns() |>
    cards::unlist_ard_columns() |>
    dplyr::mutate(
      # label any event rows for top level any event and each aesoc any event
      # rows
      AETERM = dplyr::if_else(
        !is.na(..ard_hierarchical_overall..) &
          ..ard_hierarchical_overall.. == TRUE |
          is.na(AETERM) & !is.na(AEBODSYS),
        "ANY EVENT",
        AETERM
      ),
      # update na rows in aebodsys to be ANY EVENT so labels appear correctly
      # in tfrmt
      AEBODSYS = dplyr::if_else(
        AETERM == "ANY EVENT" &
          is.na(AEBODSYS),
        "ANY EVENT",
        AEBODSYS
      ),
      #create bigN values
      stat_name = dplyr::if_else(
        is.na(AETERM) & stat_name == "n" | context == "total_n",
        "bigN",
        stat_name
      )
    ) |>
    #filter to just the needed stats
    dplyr::filter(
      !(is.na(AETERM) & stat_name %in% c("N", "p"))
    ) |>
    dplyr::select(
      TRT01A,
      AESEV,
      AEBODSYS,
      AETERM,
      stat,
      stat_name
    )

  # the tfrmt table -------------------------------------------------------
  ae_t02 <- tfrmt_n_pct(
    n = "n",
    pct = "p",
    pct_frmt_when = frmt_when(
      "==1" ~ frmt("100"),
      ">.99" ~ frmt("(>99%)"),
      "==0" ~ "",
      "<.01" ~ frmt("(<1%)"),
      "TRUE" ~ frmt("(xx%)", transform = ~ . * 100)
    )
  ) |>
    tfrmt(
      group = AEBODSYS,
      label = AETERM,
      param = stat_name,
      value = stat,
      column = c(TRT01A, AESEV),
      row_grp_plan = row_grp_plan(
        row_grp_structure(
          group_val = ".default",
          element_block(
            post_space = " "
          )
        )
      ),
      big_n = big_n_structure(
        param_val = "bigN",
        n_frmt = frmt(" (N=xx)")
      )
    )

  a <- print_to_gt(ae_t02, ae2_ard_tbl)
  a

  # tidy with `prep_...()` functions ----------------------------------------
  prepped_ard <- ae_ard |>
    shuffle_card(
      by = c("TRT01A", "AESEV"),
      fill_overall = NA,
      fill_hierarchical_overall = "ANY EVENT"
    ) |>
    # prep_combine_vars(
    #   vars = c("AEBODSYS", "AETERM")
    # ) |>
    prep_big_n(
      vars = c("TRT01A", "AESEV")
    ) |>
    # prep_label() |>
    prep_hierarchical_fill(
      vars = c("AEBODSYS", "AETERM"),
      fill = "ANY EVENT"
    ) |>
    dplyr::select(-context, -stat_variable, -stat_label) |>
    dplyr::relocate(stat_name, .after = stat)

  expect_equal(
    arrange(ae2_ard_tbl, TRT01A, AESEV, AEBODSYS, AETERM),
    arrange(prepped_ard, TRT01A, AESEV, AEBODSYS, AETERM),
    ignore_attr = TRUE
  )

  expect_no_error(
    b <- print_to_gt(ae_t02, prepped_ard)
  )

  expect_identical(
    gt::extract_body(a),
    gt::extract_body(b),
    ignore_attr = ".col_plan_vars"
  )

  # check messaging around combining vars in a hierarchical ard stack df
  expect_message(
    ae_ard |>
      shuffle_card(
        by = c("TRT01A", "AESEV"),
        fill_overall = NA,
        fill_hierarchical_overall = "ANY EVENT"
      ) |>
      prep_combine_vars(
        vars = c("AEBODSYS", "AETERM")
      ),
    regexp = "The `context` column indicates data comes from a hierarchical `ard` stack.",
    fixed = TRUE
  )

  # check messaging around prep_label() needing `variable_level`
  expect_message(
    ae_ard |>
      shuffle_card(
        by = c("TRT01A", "AESEV"),
        fill_overall = NA,
        fill_hierarchical_overall = "ANY EVENT"
      ) |>
      prep_big_n(
        vars = c("TRT01A", "AESEV")
      ) |>
      prep_label(),
    regexp = "Required column (`variable_level`) not present in the input data.",
    fixed = TRUE
  )
})


#
# test_that("prep_card() fails when column is not character", {
#
#   # data prep -------------------------------------------------------------
#   adsl <- pharmaverseadam::adsl |>
#     dplyr::filter(SAFFL == "Y") |>
#     dplyr::select(USUBJID, ARM, SEX, AGE, AGEGR1, ETHNIC, RACE)
#
#   # create card -----------------------------------------------------------
#   ard <- cards::ard_stack(
#     data = adsl,
#     .by = ARM,
#     cards::ard_continuous(
#       variables = AGE,
#       statistic = ~ cards::continuous_summary_fns(
#         c("N", "mean", "sd", "min", "max")
#       )
#     ),
#     cards::ard_categorical(
#       variables = c(AGEGR1, SEX, ETHNIC, RACE)
#     ),
#     .overall = TRUE,
#     .total_n = TRUE
#   )
#
#
#   expect_snapshot(
#     error = TRUE,
#     prep_card(ard, column = 2),
#   )
# })
#
# test_that("prep_card() removes context == 'attributes'", {
#
#   # data prep -------------------------------------------------------------
#   adsl <- cards::ADSL |>
#     dplyr::filter(SAFFL == "Y") |>
#     dplyr::select(USUBJID, ARM, SEX, AGE, AGEGR1, ETHNIC, RACE)
#
#   # create card -----------------------------------------------------------
#   ard <- cards::ard_stack(
#     data = adsl,
#     .by = ARM,
#     cards::ard_continuous(
#       variables = AGE,
#       statistic = ~ cards::continuous_summary_fns(
#         c("N", "mean", "sd", "min", "max")
#       )
#     ),
#     cards::ard_categorical(
#       variables = c(AGEGR1, SEX, ETHNIC, RACE)
#     ),
#     .overall = TRUE,
#     .total_n = TRUE,
#     .attributes = TRUE
#   )
#
#   # expect_equal since there are some small differences in the `stat` column
#   # which `shuffle_card()` returns as character. the conversion to double
#   # seems to be lossy
#   expect_equal(
#     prep_card(
#       ard,
#       column = "ARM"
#     ) |>
#       dplyr::mutate(
#         stat = as.double(stat)
#       ),
#     prep_card(
#       ard |> dplyr::filter(context != "attributes"),
#       column = "ARM"
#     )
#   )
# })

test_that("prep_combine_vars() works", {
  df <- tibble::tibble(
    a = 1:6,
    context = rep("categorical", 6),
    b = c("a", rep(NA, 5)),
    c = c(NA, "b", rep(NA, 4)),
    d = c(NA, NA, "c", rep(NA, 3)),
    e = c(NA, NA, NA, "d", rep(NA, 2)),
    f = c(NA, NA, NA, NA, "e", NA),
    g = c(rep(NA, 5), "f")
  )

  expect_identical(
    prep_combine_vars(
      df,
      vars = c("b", "c", "d", "e", "f", "g")
    ),
    tibble::tibble(
      a = 1:6,
      context = rep("categorical", 6),
      variable_level = c("a", "b", "c", "d", "e", "f")
    )
  )

  expect_snapshot(
    prep_combine_vars(
      df,
      vars = c("b", "c", "d", "e", "f", "g")
    )
  )

  expect_snapshot(
    prep_combine_vars(
      df,
      vars = c("b", "c", "d", "e", "f"),
      remove = FALSE
    )
  )
})

test_that("prep_combine_vars() returns the input when context hierarchical", {

  df <- tibble::tibble(
    a = 1:6,
    context = rep("hierarchical", 6),
    b = c("a", rep(NA, 5)),
    c = c(NA, "b", rep(NA, 4)),
    d = c(NA, NA, "c", rep(NA, 3)),
    e = c(NA, NA, NA, "d", rep(NA, 2)),
    f = c(NA, NA, NA, NA, "e", NA),
    g = c(rep(NA, 5), "f")
  )

  expect_identical(
    suppressMessages(
      prep_combine_vars(
        df,
        vars = c("b", "c", "d", "e", "f", "g")
      )
    ),
    df
  )

  expect_snapshot(
    prep_combine_vars(
      df,
      vars = c("b", "c", "d", "e", "f", "g")
    )
  )
})

test_that("prep_combine_vars() return the input unchanged when length(vars)=1", {

  df <- tibble::tibble(
    a = 1:6,
    context = rep("categorical", 6),
    b = c("a", rep(NA, 5)),
    c = c(NA, "b", rep(NA, 4)),
    d = c(NA, NA, "c", rep(NA, 3)),
    e = c(NA, NA, NA, "d", rep(NA, 2)),
    f = c(NA, NA, NA, NA, "e", NA),
    g = c(rep(NA, 5), "f")
  )

  expect_identical(
    suppressMessages(
      prep_combine_vars(
        df,
        vars = "b"
      )
    ),
    df
  )

  expect_snapshot(
    prep_combine_vars(
      df,
      vars = "b"
    )
  )
})

test_that("prep_combine_vars() does not over unite", {

  # c, d and e are identical, the are not pasted together in the output
  # the input is returned unchanged
  df <- tibble::tibble(
    a = 1:6,
    context = rep("categorical", 6),
    b = c("a", rep(NA, 5)),
    c = c(NA, "b", rep(NA, 4)),
    d = c(NA, "b", rep(NA, 4)),
    e = c(NA, "b", rep(NA, 4)),
    f = c(NA, NA, NA, NA, "e", NA),
    g = c(rep(NA, 5), "f")
  )

  expect_identical(
    suppressMessages(
      prep_combine_vars(
        df,
        vars = c("b", "c", "d", "e", "f", "g")
      )
    ),
    df
  )

  expect_snapshot(
    prep_combine_vars(
      df,
      vars = c("b", "c", "d", "e", "f", "g")
    )
  )
})

test_that("prep_combine_vars() informs when the context col is missing", {
  df <- tibble::tibble(
    a = 1:6,
    b = c("a", rep(NA, 5)),
    c = c(NA, "b", rep(NA, 4)),
    d = c(NA, "b", rep(NA, 4)),
    e = c(NA, "b", rep(NA, 4)),
    f = c(NA, NA, NA, NA, "e", NA),
    g = c(rep(NA, 5), "f")
  )

  expect_snapshot(
    prep_combine_vars(
      df,
      vars = c("b", "c", "d", "e", "f", "g")
    )
  )
})

test_that("prep_big_n() works", {
  df <- tibble::tibble(
    stat_name = c("n", "max", "min", rep(c("n", "N", "p"), times = 2)),
    context = rep(c("continuous", "hierarchical", "categorical"), each = 3),
    stat_variable = rep(c("a", "b", "c"), each = 3)
  ) |>
    bind_rows(
      tibble::tibble(
        stat_name = "n",
        context = "total_n",
        stat_variable = "d"
      )
    )

  expect_identical(
    prep_big_n(
      df,
      vars = c("b", "c")
    ),
    tibble::tibble(
      stat_name = c("n", "max", "min", rep("bigN", 3)),
      context = c(
        rep("continuous", 3),
        "hierarchical",
        "categorical",
        "total_n"
      ),
      stat_variable = c(rep("a", 3), "b", "c", "d")
    )
  )

  expect_snapshot(
    prep_big_n(
      df,
      vars = c("b", "c")
    )
  )

  expect_snapshot(
    prep_big_n(
      df,
      vars = "b"
    )
  )
})

test_that("prep_big_n() informs when required columns are missing", {
  df <- tibble::tibble(
    a = c("n", "max", "min", rep(c("n", "N", "p"), times = 2)),
    context = rep(c("continuous", "hierarchical", "categorical"), each = 3),
    stat_variable = rep(c("a", "b", "c"), each = 3)
  )

  expect_identical(
    suppressMessages(
      prep_big_n(df)
    ),
    df
  )

  expect_snapshot(
    prep_big_n(df)
  )
})

test_that("prep_label() works", {
  df <- tibble::tibble(
    variable_level = rep(c("a", "b", "c"), each = 3),
    stat_label = c("n", "N", "%", "N", "Mean", "SD", "n", "N", "%"),
    stat_name = c("n", "N", "p", "N", "mean", "sd", "n", "N", "p"),
    context = rep(c("categorical", "continuous", "hierarchical"), each = 3)
  )

  expect_identical(
    prep_label(df),
    tibble::tibble(
      variable_level = rep(c("a", "b", "c"), each = 3),
      stat_label = c("n", "N", "%", "N", "Mean", "SD", "n", "N", "%"),
      stat_name = c("n", "N", "p", "N", "mean", "sd", "n", "N", "p"),
      context = rep(c("categorical", "continuous", "hierarchical"), each = 3),
      label = c("a", "a", "a", "N", "Mean", "SD", "c", "c", "c")
    )
  )

  expect_snapshot(
    prep_label(df)
  )
})

test_that("prep_label() returns the input when the required cols are missing", {
  # `variable_level` col is not present
  df <- tibble::tibble(
    x = c("d", "e", "f"),
    stat_label = c("a", "b", "c"),
    context = c("categorical", "continuous", "hierarchical")
  )

  expect_identical(
    suppressMessages(prep_label(df)),
    df
  )

  expect_snapshot(
    prep_label(df)
  )

  # `stat_label` col is not present
  df2 <- tibble::tibble(
    variable_level = c("d", "e", "f"),
    y = c("a", "b", "c"),
    context = c("categorical", "continuous", "hierarchical")
  )

  expect_snapshot(
    prep_label(df2)
  )

  expect_identical(
    suppressMessages(prep_label(df2)),
    df2
  )
})

test_that("prep_hierarchical_fill() returns the input when `length(vars) < 2`", {
  df <- tibble::tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, 3)
  )

  expect_identical(
    suppressMessages(
      prep_hierarchical_fill(
        df,
        vars = "y"
      )
    ),
    df
  )

  expect_snapshot(
    prep_hierarchical_fill(
      df,
      vars = "y"
    ),
  )
})

test_that("prep_hierarchical_fill() fills pairwise conditionally", {

  df <- tibble::tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, 3)
  )

  # z is not filled - still 3 NAs
  expect_identical(
    prep_hierarchical_fill(df, vars = c("x", "y")),
    tibble::tibble(
      x = c(1, 2, NA),
      y = c("a", "Any y", "b"),
      z = rep(NA, 3)
    )
  )

  expect_identical(
    prep_hierarchical_fill(df, vars = c("x", "y", "z")),
    tibble::tibble(
      x = c(1, 2, NA),
      y = c("a", "Any y", "b"),
      z = rep("Any z", 3)
    )
  )
})

test_that("prep_hierarchical_fill() with `fill`", {

  df <- tibble::tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, "3")
  )

  # the second value of y is replaced with "2
  expect_identical(
    prep_hierarchical_fill(
      df,
      vars = c("x", "y"),
      fill = "foo"
    ),
    tibble::tibble(
      x = c(1, 2, NA),
      y = c("a", "foo", "b"),
      z = rep(NA, 3)
    )
  )

  expect_identical(
    prep_hierarchical_fill(
      df,
      vars = c("x", "y", "z"),
      fill = "bar"
    ),
    tibble::tibble(
      x = c(1, 2, NA),
      y = c("a", "bar", "b"),
      z = c("bar", "bar", "bar")
    )
  )
})

test_that("prep_hierarchical_fill() with `fill` 'Any {colname}'", {

  df <- tibble::tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, "3")
  )

  expect_identical(
    prep_hierarchical_fill(
      df,
      vars = c("x", "y"),
      fill = "Any {colname}"
    ),
    tibble::tibble(
      x = c(1, 2, NA),
      y = c("a", "Any y", "b"),
      z = rep(NA, 3)
    )
  )

  expect_identical(
    prep_hierarchical_fill(
      df,
      vars = c("x", "y", "z"),
      fill = "Any {colname}"
    ),
    tibble::tibble(
      x = c(1, 2, NA),
      y = c("a", "Any y", "b"),
      z = c("Any z", "Any z", "Any z")
    )
  )
})

test_that("prep_hierarchical_fill() with `fill_from='left'` works", {

  df <- tibble::tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, "3")
  )

  expect_identical(
    prep_hierarchical_fill(df, vars = c("x", "y"), fill_from = "left"),
    tibble::tibble(
      x = c(1, 2, NA),
      y = c("a", "2", "b"),
      z = rep(NA, 3)
    )
  )

  expect_identical(
    prep_hierarchical_fill(df, vars = c("x", "y", "z"), fill_from = "left"),
    tibble::tibble(
      x = c(1, 2, NA),
      y = c("a", "2", "b"),
      z = c("a", "2", "b")
    )
  )
})

test_that("prep_hierarchical_fill() complains with `fill_from` other than 'left'", {

  df <- tibble::tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, "3")
  )

  expect_snapshot(
    error = TRUE,
    prep_hierarchical_fill(
      df,
      vars = c("x", "y", "z"),
      fill_from = "foo"
    )
  )
})

test_that("prep_hierarchical_fill() errors when `fill` is not character", {

  df <- tibble::tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, "3")
  )

  expect_snapshot(
    error = TRUE,
    prep_hierarchical_fill(
      df,
      vars = c("x", "y", "z"),
      fill = 2
    )
  )
})

test_that("generate_pairs() works", {

  expect_identical(
    generate_pairs(
      c("foo", "bar", "baz")
    ),
    list(
      c("foo", "bar"),
      c("bar", "baz")
    )
  )
})

test_that("generate_pairs() complains", {

  # with non-character input
  expect_snapshot(
    error = TRUE,
    generate_pairs(1:3)
  )

  # with an input having less than 2 elements
  expect_snapshot(
    error = TRUE,
    generate_pairs("foo")
  )

})

test_that("replace_na_pairwise() works", {

  expect_identical(
    replace_na_pairwise(
      tibble::tibble(
        x = c(1, 2, NA),
        y = c("a", NA, "b"),
        z = rep(NA, 3)
      ),
      pair = c("y", "z")
    ),
    # all NAs in z (when y is not NA) are replaced with `"Any z"`
    tibble::tibble(
      x = c(1, 2, NA),
      y = c("a", NA, "b"),
      z = c("Any z", NA, "Any z")
    )
  )

  expect_identical(
    replace_na_pairwise(
      tibble::tibble(
        x = c(1, 2, NA),
        y = c("a", NA, "b"),
        z = rep(NA, 3)
      ),
      pair = c("y", "z"),
      fill = "foo"
    ),
    # all NAs in z (when y is not NA) are replaced with `"Any z"`
    tibble::tibble(
      x = c(1, 2, NA),
      y = c("a", NA, "b"),
      z = c("foo", NA, "foo")
    )
  )

  expect_identical(
    replace_na_pairwise(
      tibble::tibble(
        x = c(1, 2, NA),
        y = c("a", NA, "b"),
        z = rep(NA, 3)
      ),
      pair = c("y", "z"),
      fill_from = "left"
    ),
    # all NAs in z (when y is not NA) are replaced with `"Any z"`
    tibble::tibble(
      x = c(1, 2, NA),
      y = c("a", NA, "b"),
      z = c("a", NA, "b")
    )
  )
})

test_that("replace_na_pairwise() complains", {

  expect_snapshot(
    error = TRUE,
    replace_na_pairwise(
      tibble::tibble(
        x = c(1, 2, NA),
        y = c("a", NA, "b"),
        z = rep(NA, 3)
      ),
      pair = 1:2
    )
  )

  expect_snapshot(
    error = TRUE,
    replace_na_pairwise(
      tibble::tibble(
        x = c(1, 2, NA),
        y = c("a", NA, "b"),
        z = rep(NA, 3)
      ),
      pair = c("x", "y", "z")
    )
  )

  expect_snapshot(
    error = TRUE,
    replace_na_pairwise(
      tibble::tibble(
        x = c(1, 2, NA),
        y = c("a", NA, "b"),
        z = rep(NA, "3")
      ),
      pair = c("y", "z"),
      fill_from = "right"
    )
  )

  expect_snapshot(
    error = TRUE,
    replace_na_pairwise(
      tibble::tibble(
        x = c(1, 2, NA),
        y = c("a", NA, "b"),
        z = rep(NA, "3")
      ),
      pair = c("y", "z"),
      fill = 2
    )
  )

})

test_that("is_card_with_attributes() works", {
  df <- tibble::tibble(
    context = c("categorical", "attributes"),
    a = 1:2
  )

  expect_true(is_card_with_attributes(df))

  df2 <- tibble::tibble(
    context = c("categorical", "continuous"),
    a = 1:2
  )

  expect_false(is_card_with_attributes(df2))
})
