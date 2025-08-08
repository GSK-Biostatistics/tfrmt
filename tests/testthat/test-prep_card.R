test_that("prep_card() works with demographic data", {
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

  # tidy for table --------------------------------------------------------
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

  # with prep_card --------------------------------------------------------
  ard_tbl_with_prep <- ard |>
    prep_card(by = "ARM") |>
    dplyr::mutate(
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

  expect_identical(
    dplyr::arrange(ard_tbl, ord1, ord2),
    dplyr::arrange(ard_tbl_with_prep, ord1, ord2)
  )

  expect_no_error(
    b <- print_to_gt(dm_t01, ard_tbl_with_prep)
  )

  b_html <- b |>
    gt::as_raw_html() |>
    # the id is randomly generated
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

test_that("prep_card() works with adverse effects data", {
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

  # tidy for table --------------------------------------------------------
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

  # with prep_card -------------------------------------------------------
  ae2_ard_tbl_with_prep <- ae_ard |>
    prep_card(
      by = c("TRT01A", "AESEV"),
      fill_overall = NA,
      fill_hierarchical_overall = "ANY EVENT"
    ) |>
    dplyr::select(-context, -stat_variable, -stat_label) |>
    dplyr::relocate(stat_name, .after = stat)

  expect_equal(
    arrange(ae2_ard_tbl, TRT01A, AESEV, AEBODSYS, AETERM),
    arrange(ae2_ard_tbl_with_prep, TRT01A, AESEV, AEBODSYS, AETERM),
    ignore_attr = TRUE
  )

  expect_no_error(
    b <- print_to_gt(ae_t02, ae2_ard_tbl_with_prep)
  )

  expect_identical(
    gt::extract_body(a),
    gt::extract_body(b),
    # for some reasom the .col_plan_vars attributes are different
    ignore_attr = ".col_plan_vars"
  )
})

test_that("prep_card() unite_data_vars does not over-unite", {
  # we only want to unite when it effectively has the same impact as coalesce
  a <- cards::ard_strata(
    cards::ADSL,
    .by = ARM,
    .f = ~ cards::ard_categorical(
      .x,
      by = SEX,
      variables = AGEGR1
    )
  )

  b <- a |>
    prep_card(by = "ARM")

  # there is no variable_level column
  expect_true(
    !"variable_level" %in% names(b)
  )

  # b is actually identical to the shuffled_card
  # # TODO think how we can figure this out and maybe return early
  expect_identical(
    shuffle_card(a),
    b
  )
})

test_that("fill_pairwise() fills pairwise conditionally", {
  df <- tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, 3)
  )

  # z is not filled - still 3 NAs
  expect_identical(
    fill_pairwise(df, variables = c("x", "y")),
    tibble(
      x = c(1, 2, NA),
      y = c("a", "Any y", "b"),
      z = rep(NA, 3)
    )
  )

  # when passing a single variable, the input is returned unchanged
  expect_identical(
    fill_pairwise(df, variables = c("x")),
    df
  )

  expect_identical(
    fill_pairwise(df, variables = c("x", "y", "z")),
    tibble(
      x = c(1, 2, NA),
      y = c("a", "Any y", "b"),
      z = rep("Any z", 3)
    )
  )
})

test_that("fill_pairwise() with `fill_hierarchical_overall`", {
  df <- tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, "3")
  )

  # the second value of y is replaced with "2
  expect_identical(
    fill_pairwise(
      df,
      variables = c("x", "y"),
      fill_hierarchical_overall = "foo"
    ),
    tibble(
      x = c(1, 2, NA),
      y = c("a", "foo", "b"),
      z = rep(NA, 3)
    )
  )

  expect_identical(
    fill_pairwise(
      df,
      variables = c("x", "y", "z"),
      fill_hierarchical_overall = "bar"
    ),
    tibble(
      x = c(1, 2, NA),
      y = c("a", "bar", "b"),
      z = c("bar", "bar", "bar")
    )
  )
})

test_that("fill_pairwise() with `fill_hierarchical_overall` 'Any {colname}'", {
  df <- tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, "3")
  )

  expect_identical(
    fill_pairwise(
      df,
      variables = c("x", "y"),
      fill_hierarchical_overall = "Any {colname}"
    ),
    tibble(
      x = c(1, 2, NA),
      y = c("a", "Any y", "b"),
      z = rep(NA, 3)
    )
  )

  expect_identical(
    fill_pairwise(
      df,
      variables = c("x", "y", "z"),
      fill_hierarchical_overall = "Any {colname}"
    ),
    tibble(
      x = c(1, 2, NA),
      y = c("a", "Any y", "b"),
      z = c("Any z", "Any z", "Any z")
    )
  )
})

test_that("fill_pairwise() with `fill_from='left'` works", {
  df <- tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, "3")
  )

  expect_identical(
    fill_pairwise(df, variables = c("x", "y"), fill_from = "left"),
    tibble(
      x = c(1, 2, NA),
      y = c("a", "2", "b"),
      z = rep(NA, 3)
    )
  )

  expect_identical(
    fill_pairwise(df, variables = c("x", "y", "z"), fill_from = "left"),
    tibble(
      x = c(1, 2, NA),
      y = c("a", "2", "b"),
      z = c("a", "2", "b")
    )
  )
})

test_that("fill_pairwise() complains with `fill_from` other than 'left'", {
  df <- tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, "3")
  )

  expect_snapshot(
    error = TRUE,
    fill_pairwise(
      df,
      variables = c("x", "y", "z"),
      fill_from = "foo"
    )
  )
})

test_that("fill_pairwise() errors with `fill_hierarchical_overall` non-char", {
  df <- tibble(
    x = c(1, 2, NA),
    y = c("a", NA, "b"),
    z = rep(NA, "3")
  )

  expect_snapshot(
    error = TRUE,
    fill_pairwise(
      df,
      variables = c("x", "y", "z"),
      fill_hierarchical_overall = 2
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

test_that("replace_na_pair() works", {
  expect_identical(
    replace_na_pair(
      tibble(
        x = c(1, 2, NA),
        y = c("a", NA, "b"),
        z = rep(NA, 3)
      ),
      pair = c("y", "z")
    ),
    # all NAs in z (when y is not NA) are replaced with `"Any z"`
    tibble(
      x = c(1, 2, NA),
      y = c("a", NA, "b"),
      z = c("Any z", NA, "Any z")
    )
  )
})
