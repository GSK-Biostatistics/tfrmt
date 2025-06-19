test_that("extract_grouping_variables() works with a single grouping variable", {
  adsl <- pharmaverseadam::adsl |>
    dplyr::filter(
      SAFFL == "Y"
    ) |>
    dplyr::select(
      USUBJID,
      ARM,
      SEX,
      AGE,
      AGEGR1,
      ETHNIC,
      RACE
    ) |>
    dplyr::mutate(
      SEX = factor(SEX),
      AGEGR1 = factor(AGEGR1),
      ETHNIC = factor(ETHNIC),
      RACE = factor(RACE)
    )

  # `ard_categorical.data.frame()` throws a warning due to a partial argument
  # match of `variable` to `variables`, hence the need to suppress warnings
  suppressWarnings(
    ard <- cards::ard_stack(
      data = adsl,
      .by = ARM,
      cards::ard_continuous(
        variables = AGE,
        statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd", "min", "max"))
      ),
      cards::ard_categorical(
        variables = c(
          AGEGR1,
          SEX,
          ETHNIC,
          RACE
        )
      ),
      .overall = TRUE,
      .total_n = TRUE
    )
  )

  expect_equal(
    extract_grouping_variables(ard),
    "ARM"
  )
})

test_that("extract_grouping_variables() works with multiple grouping variables", {
  adsl <- pharmaverseadam::adsl |>
    dplyr::filter(
      SAFFL == "Y"
    ) |>
    # create dummy site for demonstration purposes
    dplyr::mutate(
      COUNTRY = dplyr::if_else(
        SITEID == 718,
        "UK",
        COUNTRY
      )
    )

  # create siteid counts by country and arm
  # `ard_categorical.data.frame()` throws a warning due to a partial argument
  # match of `variable` to `variables`, hence the need to suppress warnings
  suppressWarnings(
    ard1 <- cards::ard_strata(
      .data = adsl,
      .by = COUNTRY,
      .f = ~ cards::ard_categorical(
        .x,
        by = ARM,
        variable = SITEID,
        statistic = list(
          SITEID = c("n", "p")
        ),
        denominator = adsl
      )
    )
  )

  # create ard for siteid counts by country (Total column)
  # `ard_categorical.data.frame()` throws a warning due to a partial argument
  # match of `variable` to `variables`, hence the need to suppress warnings
  suppressWarnings(
    ard2 <- cards::ard_strata(
      .data = adsl,
      .by = COUNTRY,
      .f = ~ cards::ard_categorical(
        .x,
        variable = SITEID,
        statistic = list(
          SITEID = c("n", "p")
        ),
        denominator = adsl
      )
    ) |>
      rename(
        "group2" = group1,
        "group2_level" = group1_level
      ) |>
      mutate(
        group1 = "ARM",
        group1_level = list("Total")
      )
  )

  # counts by country
  ard3 <- cards::ard_stack(
    data = adsl,
    .by = ARM,
    .overall = TRUE,
    cards::ard_categorical(
      variables = COUNTRY,
      statistic = everything() ~ c("n", "p")
    )
  ) |>
    rename(
      "group2" = variable,
      "group2_level" = variable_level
    ) |>
    mutate(
      variable = "SITEID",
      variable_level = list("Subtotal")
    )

  final_ard <- bind_rows(
    ard1,
    ard2,
    ard3,
    cards::ard_total_n(adsl)
  ) |>
    cards::unlist_ard_columns()

  expect_equal(
    extract_grouping_variables(final_ard),
    c("ARM", "COUNTRY")
  )
})
