test_that("prep_tfrmt() works with DM-T01", {

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
          if (length(.x) == 0) return(NA_character_)

          as.character(.x[[1]])   # Convert to character
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
    title = "Table Title",
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
          p = frmt("xx", transform = ~. * 100)
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

  # with prep_tfrmt -------------------------------------------------------
  ard_tbl_with_prep <- ard |>
    shuffle_card() |>
    prep_tfrmt("ARM") |>
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

test_that("prep_tfrmt() works with AE-T02", {
skip()
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
  ae_ard <- cards::ard_stack_hierarchical(
    data = adae,
    by =  c(TRT01A, AESEV),
    variables = c(AEBODSYS, AETERM),
    statistic = ~ c("n", "p"),  # Calculate count and percentage
    denominator = adsl,
    id = USUBJID,
    over_variables = TRUE,
    total_n = TRUE
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
      !(is.na(AETERM)  & stat_name %in% c("N", "p"))
    ) |>
    dplyr::select(
      TRT01A, AESEV, AEBODSYS, AETERM, stat, stat_name
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
      "TRUE" ~ frmt("(xx%)", transform = ~.*100)
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

  # with prep_tfrmt -------------------------------------------------------
  ae2_ard_tbl_with_prep <- ae_ard |>
    shuffle_card() |>
    prep_tfrmt(column = c(TRT01A, AESEV)) |>
    mutate(
      AESEV = if_else(
        AESEV == "Overall AESEV",
        NA,
        AESEV
      ),
      # TRT01A = if_else(
      #   TRT01A == "Overall TRT01A",
      #   NA,
      #   TRT01A
      # ),
      AEBODSYS = if_else(
        AEBODSYS == "Any AEBODSYS",
        "ANY EVENT",
        AEBODSYS
      ),
      AETERM = if_else(
        AETERM == "Any AETERM",
        "ANY EVENT",
        AETERM
      )
    )

  b <- print_to_gt(ae_t02, ae2_ard_tbl_with_prep)

  waldo::compare(
    arrange(ae2_ard_tbl, TRT01A, AESEV, AEBODSYS, AETERM),
    arrange(ae2_ard_tbl_with_prep, TRT01A, AESEV, AEBODSYS, AETERM)
  )

  expect_identical(
    gt::extract_body(a),
    gt::extract_body(b)
  )
})
