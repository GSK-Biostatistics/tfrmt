test_that("prep_tfrmt() works", {
  adsl <- pharmaverseadam::adsl |>
    dplyr::filter(SAFFL == "Y") |>
    dplyr::select(USUBJID, ARM, SEX, AGE, AGEGR1, ETHNIC, RACE)

  ard_no_attributes <- cards::ard_stack(
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
    .total_n = TRUE,
    .attributes = FALSE
  )

  ard_tbl <- ard |>
    cards::shuffle_ard() |>
    dplyr::mutate(
      label = purrr::map_chr(
        variable_level,
        ~ {
          if (length(.x) == 0) {
            return(NA_character_)
          }
          as.character(.x[[1]])
        }
      )
    ) |>
    dplyr::mutate(
      label = dplyr::if_else(is.na(label), stat_label, label),
      # Add big N labels
      ARM = dplyr::if_else(variable == "ARM", label, ARM),
      # ID total trt
      ARM = dplyr::if_else(
        is.na(ARM) | variable == "..ard_total_n..",
        "Overall ARM",
        ARM
      ),
      # unique stat names for big N's
      stat_name = dplyr::if_else(
        (variable=="ARM" & stat_name=="n") | variable=="..ard_total_n..",
        "bigN",
        stat_name
      ),
      # relabel the label for n's
      label = dplyr::if_else(stat_name=="N", "n", label)
    ) |>
    # remove unneeded stats
    dplyr::filter(!(variable=="ARM" & stat_name !="bigN")) |>
    # drop variables not needed
    dplyr::select(ARM, variable, label, stat_name, stat)|>
    # remove dups (extra denoms per variable level)
    unique() |>
    # arranging for waldo::compare
    dplyr::arrange(ARM, variable, label)

  ard_tbl_with_prep_and_no_attributes <- ard_no_attributes |>
    cards::shuffle_ard() |>
    prep_tfrmt("ARM")

  expect_identical(
    ard_tbl,
    ard_tbl_no_attributes
  )

  dm_t01_format <-  tfrmt(
    title = "Table DM_T01: Summary of Demographic and Baseline Characteristics",
    group = variable,
    label = label,
    param = stat_name,
    value = stat,
    column = ARM,
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
          p = frmt("xx", transform = ~.*100)
        )
      )
    ),
    big_n = big_n_structure(
      param_val = "bigN",
      n_frmt = frmt("\n(N=xx)")
    ),
    col_style_plan = col_style_plan(
      col_style_structure(
        col= everything(),
        align = "left"
      )
    ),
    row_grp_plan = row_grp_plan(
      row_grp_structure(
        group_val = ".default",
        element_block(post_space = " ")
      )
    )
  )

  expect_no_error(
    print_to_gt(dm_t01_format, ard_tbl_no_attributes),
  )

  expect_snapshot(
    print_to_gt(dm_t01_format, ard_tbl_no_attributes) |> gt::as_raw_html(),
    transform = strip_id
  )
})

test_that("prep_tfrmt() works with attributes of shuffled ard", {
  ard_attributes <- ard_stack(
    data = adsl,
    .by = ARM,
    ard_continuous(
      variables = AGE,
      statistic = ~ continuous_summary_fns(
        c("N","mean","sd","min","max"))
    ),
    ard_categorical(
      variables = c(AGEGR1, SEX, ETHNIC, RACE)
    ),
    .overall = TRUE,
    .total_n = TRUE,
    .attributes = TRUE
  )

  shuffled_ard_attributes <- ard_attributes |>
    shuffle_ard()

  ard_tbl_attributes <- shuffled_ard_attributes |>
    prep_tfrmt("ARM")

  expect_snapshot(
    prep_tfrmt(shuffled_ard_attributes, "ARM")
  )

})
