test_that("shuffle/trim works", {
  # shuffle without group/var levels
  ard_simple <- cards::ard_continuous(cards::ADSL, by = "ARM",variables = "AGE")

  ard_simple_shuffled <- ard_simple |>
    shuffle_card(by = NULL, trim = FALSE) |>
    as.data.frame()

  expect_snapshot(ard_simple_shuffled)

  # shuffle with 1 by var
  ard_grp <- cards::bind_ard(
    cards::ard_categorical(cards::ADSL, variables = "ARM"),
    cards::ard_categorical(cards::ADSL, by = "ARM", variables = "AGEGR1")
  )
  ard_grp_shuffled <- ard_grp |>
    shuffle_card(by = "ARM", trim = FALSE) |>
    dplyr::filter(!stat_name == "N")
  expect_true(all(!is.na(ard_grp_shuffled$ARM)))

  ard_hier <- cards::ard_hierarchical_count(
    data = cards::ADAE,
    variables = c(AESOC, AEDECOD),
    by = TRTA
  )
  ard_hier_shuff <- ard_hier |>
    shuffle_card(trim = FALSE) |>
    as.data.frame()
  expect_true(all(!is.na(ard_hier_shuff$AESOC)))


  # shuffle many different formats
  ard_test <- cards::bind_ard(
    cards::ard_categorical(cards::ADSL, variables = "ARM"),
    cards::ard_continuous(cards::ADSL, by = "ARM", variables = "AGE", stat_label = ~ list(c("mean", "sd") ~ "Mean(SD)")),
    cards::ard_categorical(cards::ADSL, by = "ARM", variables = "AGEGR1"),
    cards::ard_missing(cards::ADSL, by = "ARM", variables = c("AGEGR1", "AGE"))
  )
  ard_shuffled <- ard_test |>
    shuffle_card(by = "ARM") |>
    as.data.frame()

  expect_snapshot(ard_shuffled[1:5, ])

  # shuffle & trim
  ard_shuff_trim <- ard_test |>
    shuffle_card(by = "ARM") |>
    as.data.frame()
  expect_snapshot(ard_shuff_trim[1:5, ])
  # only numeric stats
  expect_type(ard_shuff_trim$stat, "double")
  # no list columns
  expect_true(!any(map_lgl(ard_shuff_trim, is.list)))
})

# test_that("shuffle_ard handles protected names", {
#   ard_test <- cards::ard_categorical(
#     cards::ADSL |> dplyr::rename(stat = ARM),
#     by = "stat",
#     variables = "AGEGR1"
#   ) |>
#     shuffle_card()
#
#   expect_equal(names(ard_test)[1], "stat.1")
# })

test_that("shuffle_card notifies user about warnings/errors before dropping", {
  expect_snapshot(
    cards::ard_continuous(
      cards::ADSL,
      variables = AGEGR1
    ) |>
      shuffle_card()
  )
})

test_that("shuffle_card fills missing group levels if the group is meaningful", {
  # mix of missing/nonmissing group levels present before shuffle
  expect_snapshot(
    cards::bind_ard(
      cards::ard_continuous(cards::ADSL, by = "ARM", variables = "AGE", statistic = ~ cards::continuous_summary_fns("mean")),
      dplyr::tibble(group1 = "ARM", variable = "AGE", stat_name = "p", stat_label = "p", stat = list(0.05))
    ) |>
      dplyr::filter(dplyr::row_number() <= 5L) |>
      shuffle_card()
  )

  # no group levels present before shuffle
  expect_snapshot(
    cards::bind_ard(
      cards::ard_continuous(cards::ADSL, variables = "AGE", statistic = ~ cards::continuous_summary_fns("mean")),
      dplyr::tibble(group1 = "ARM", variable = "AGE", stat_name = "p", stat_label = "p", stat = list(0.05))
    ) |>
      dplyr::filter(dplyr::row_number() <= 5L) |>
      shuffle_card()
  )

  # mix of group variables - fills overall only if variable has been calculated by group elsewhere
  expect_snapshot(
    cards::bind_ard(
      cards::ard_categorical(cards::ADSL, by = ARM, variables = AGEGR1) |> dplyr::slice(1),
      cards::ard_categorical(cards::ADSL, variables = AGEGR1) |> dplyr::slice(1),
      cards::ard_continuous(cards::ADSL, by = SEX, variables = AGE) |> dplyr::slice(1),
      cards::ard_continuous(cards::ADSL, variables = AGE) |> dplyr::slice(1)
    ) |>
      shuffle_card(by = c("ARM","SEX")) |>
      as.data.frame()
  )
  # custom fill
  expect_snapshot(
    cards::bind_ard(
      cards::ard_categorical(cards::ADSL, by = ARM, variables = AGEGR1) |> dplyr::slice(1),
      cards::ard_categorical(cards::ADSL, variables = AGEGR1) |> dplyr::slice(1),
      cards::ard_continuous(cards::ADSL, by = SEX, variables = AGE) |> dplyr::slice(1),
      cards::ard_continuous(cards::ADSL, variables = AGE) |> dplyr::slice(1)
    ) |>
      shuffle_card(by = c("ARM","SEX"), fill_overall = "{colname}") |>
      as.data.frame()
  )

  # mix of hierarchical group variables - fills overall only if variable has been calculated by group elsewhere
  expect_snapshot(
    cards::bind_ard(
      cards::ard_categorical(cards::ADSL, by = c(ARM, SEX), variables = AGEGR1) |> dplyr::slice(1),
      cards::ard_categorical(cards::ADSL, by = SEX, variables = AGEGR1) |> dplyr::slice(1),
      cards::ard_categorical(cards::ADSL, variables = AGEGR1) |> dplyr::slice(1)
    ) |>
      shuffle_card(by = c("ARM","SEX"))
  )
  # custom fill
  expect_snapshot(
    cards::bind_ard(
      cards::ard_categorical(cards::ADSL, by = c(ARM, SEX), variables = AGEGR1) |> dplyr::slice(1),
      cards::ard_categorical(cards::ADSL, by = SEX, variables = AGEGR1) |> dplyr::slice(1),
      cards::ard_categorical(cards::ADSL, variables = AGEGR1) |> dplyr::slice(1)
    ) |>
      shuffle_card(by = c("ARM","SEX"), fill_overall = "total")
  )
  expect_snapshot(
    cards::bind_ard(
      cards::ard_categorical(cards::ADSL, by = c(ARM, SEX), variables = AGEGR1) |> dplyr::slice(1),
      cards::ard_categorical(cards::ADSL, by = SEX, variables = AGEGR1) |> dplyr::slice(1),
      cards::ard_categorical(cards::ADSL, variables = AGEGR1) |> dplyr::slice(1)
    ) |>
      shuffle_card(by = c("ARM","SEX"), fill_overall = NA)
  )

  # fills with a unique group value if one already exists in the df
  adsl_new <- cards::ADSL |>
    dplyr::mutate(ARM = ifelse(ARM == "Placebo", "Overall ARM", ARM))
  expect_snapshot(
    cards::bind_ard(
      cards::ard_continuous(adsl_new, variables = "AGE", statistic = ~ cards::continuous_summary_fns("mean")),
      cards::ard_continuous(adsl_new, by = "ARM", variables = "AGE", statistic = ~ cards::continuous_summary_fns("mean"))
    ) |>
      shuffle_card(by = "ARM")
  )
})

test_that("shuffle_card doesn't trim off NULL/NA values", {
  # mix of char NA, NULL values
  res <- suppressMessages(
    data.frame(x = rep_len(NA, 10)) |>
      cards::ard_continuous(
        variables = x,
        statistic = ~ cards::continuous_summary_fns(c("median", "p25", "p75"))
      ) |>
      shuffle_card() |>
      dplyr::pull(stat)
  )

  # check that all rows present
  expect_length(res, 3)
})

test_that("shuffle_card coerces all factor groups/variables to character", {
  adsl_ <- cards::ADSL |>
    dplyr::mutate(RACE = factor(RACE))

  res <- cards::ard_categorical(
    data = adsl_,
    by = TRT01A,
    variables = c(RACE, ETHNIC)
  ) |>
    shuffle_card()

  res_classes <- res |>
    dplyr::select(-stat) |>
    sapply(class)

  # all are character
  expect_true(all(res_classes == "character"))

  # correct coersion
  expect_equal(
    sort(unique(res$RACE)),
    sort(unique(as.character(adsl_$RACE)))
  )
  expect_equal(
    sort(unique(res$ETHNIC)),
    sort(unique(as.character(adsl_$ETHNIC)))
  )
})

test_that("shuffle_card fills missing group levels if the group is meaningful for cardx output", {
  # cardx ARD: this is a dput() of a cardx result (see commented out code below) SAVED 2024-08-30
  ard_cardx <- structure(
    list(
      group1 = c("ARM", "ARM", "SEX", "SEX"),
      variable = c(
        "AGEGR1",
        "AGEGR1", "AGEGR1", "AGEGR1"
      ),
      context = c(
        "stats_chisq_test",
        "stats_chisq_test", "stats_chisq_test", "stats_chisq_test"
      ),
      stat_name = c("statistic", "p.value", "statistic", "p.value"),
      stat_label = c(
        "X-squared Statistic", "p-value", "X-squared Statistic",
        "p-value"
      ),
      stat = list(
        statistic = c(`X-squared` = 5.07944166638125),
        p.value = 0.0788884197453486,
        statistic = c(`X-squared` = 1.03944199945198),
        p.value = 0.594686442507218
      ),
      fmt_fun = list(
        statistic = 1L,
        p.value = 1L, statistic = 1L, p.value = 1L
      ),
      warning = list(
        warning = NULL, warning = NULL, warning = NULL, warning = NULL
      ),
      error = list(error = NULL, error = NULL, error = NULL, error = NULL)
    ),
    row.names = c(
      NA,
      -4L
    ),
    class = c("card", "tbl_df", "tbl", "data.frame")
  )

  expect_snapshot(
    ard_cardx |>
      shuffle_card() |>
      as.data.frame()
  )
})

test_that("shuffle_card() fills grouping columns with `Overall <var>` or `Any <var>`", {
  adae <- cards::ADAE |>
    dplyr::filter(
      SAFFL == "Y",
      TRTA %in% c("Placebo", "Xanomeline High Dose"),
      AESOC %in% unique(AESOC)[1:2]
    ) |>
    dplyr::group_by(AESOC) |>
    dplyr::filter(
      AETERM %in% unique(AETERM)[1:2]
    ) |>
    dplyr::ungroup()

  adsl <- cards::ADSL |>
    dplyr::filter(
      SAFFL == "Y",
      TRTA %in% c("Placebo", "Xanomeline High Dose")
    )

  ard_hier <- cards::ard_stack_hierarchical(
    data = adae,
    by = TRTA,
    variables = c(AESOC, AETERM),
    denominator = adsl,
    id = USUBJID,
    overall = TRUE,
    over_variables = TRUE,
    total_n = TRUE
  )

  shuffled_ard_hier <- ard_hier |>
    shuffle_card()

  expect_identical(
    shuffled_ard_hier |>
      dplyr::filter(
        context == "total_n"
      ) |>
      dplyr::pull(TRTA),
    "Overall TRTA"
  )

  expect_identical(
    shuffled_ard_hier |>
      dplyr::filter(stat_variable == "..ard_hierarchical_overall..") |>
      dplyr::pull(AESOC) |>
      unique(),
    "Any AESOC"
  )
  expect_identical(
    shuffled_ard_hier |>
      dplyr::filter(stat_variable == "..ard_hierarchical_overall..") |>
      dplyr::pull(AETERM) |>
      unique(),
    "Any AETERM"
  )

  # custom fill
  shuffled_ard_hier <- ard_hier |>
    shuffle_card(fill_overall = "overall {colname} observed",
                 fill_hierarchical_overall = "any {colname} observed")

  expect_identical(
    shuffled_ard_hier |>
      dplyr::filter(
        context == "total_n"
      ) |>
      dplyr::pull(TRTA),
    "overall TRTA observed"
  )
  expect_identical(
    shuffled_ard_hier |>
      dplyr::filter(stat_variable == "..ard_hierarchical_overall..") |>
      dplyr::pull(AESOC) |>
      unique(),
    "any AESOC observed"
  )
  expect_identical(
    shuffled_ard_hier |>
      dplyr::filter(stat_variable == "..ard_hierarchical_overall..") |>
      dplyr::pull(AETERM) |>
      unique(),
    "any AETERM observed"
  )
})

test_that("shuffle_card() fills with multiple `by` columns", {
  adae <- cards::ADAE |>
    dplyr::filter(
      SAFFL == "Y",
      TRTA %in% c("Placebo", "Xanomeline High Dose"),
      AESOC %in% unique(AESOC)[1:2]
    ) |>
    dplyr::group_by(AESOC) |>
    dplyr::filter(
      AETERM %in% unique(AETERM)[1:2]
    ) |>
    dplyr::ungroup()

  adsl <- cards::ADSL |>
    dplyr::filter(
      SAFFL == "Y",
      TRTA %in% c("Placebo", "Xanomeline High Dose")
    )

  ard <- cards::ard_stack_hierarchical(
    data = adae,
    by = c(TRTA, SEX),
    variables = c(AESOC, AETERM),
    denominator = adsl,
    id = USUBJID,
    overall = TRUE,
    over_variables = TRUE,
    total_n = TRUE
  )

  shuffled_ard <- ard |>
    shuffle_card()

  expect_identical(
    shuffled_ard |>
      dplyr::filter(
        context == "total_n"
      ) |>
      dplyr::select(
        TRTA,
        AESOC,
        SEX
      ),
    data.frame(
      TRTA = "Overall TRTA",
      AESOC = NA_character_,
      SEX = "Overall SEX"
    ),
    # the shuffled_ard preserves the card attributes and returns a tibble. We
    # need to ignore the attributes for the purpose of this comparison
    ignore_attr = TRUE
  )

  # expect_identical(
  #   shuffled_ard |>
  #     dplyr::filter(
  #       variable == "..ard_hierarchical_overall.."
  #     ) |>
  #     dplyr::pull(AESOC) |>
  #     unique(),
  #   "Any AESOC"
  # )


})

test_that("shuffle_card() messages about 'Overall <var>' or 'Any <var>'", {
  test_data <- dplyr::tibble(
    ARM = c("..cards_overall..", "Overall ARM", NA, "BB", NA),
    TRTA = c(NA, NA, "..hierarchical_overall..", "C", "C")
  )

  # messaging actually comes from .derive_overall_labels
  expect_snapshot(
    test_data |>
      dplyr::mutate(
        dplyr::across(
          ARM:TRTA,
          ~ .derive_overall_labels(
            .x,
            fill_overall = "Overall {colname}",
            fill_hierarchical_overall = "Any {colname}"
          )
        )
      )
  )

  adae <- cards::ADAE |>
    dplyr::filter(
      SAFFL == "Y",
      TRTA %in% c("Placebo", "Xanomeline High Dose"),
      AESOC %in% unique(AESOC)[1:2]
    ) |>
    dplyr::group_by(AESOC) |>
    dplyr::filter(
      AETERM %in% unique(AETERM)[1:2]
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      TRTA = dplyr::if_else(
        TRTA == "Xanomeline High Dose",
        "Overall TRTA",
        TRTA
      ),
      AESOC = dplyr::if_else(
        AESOC == "GASTROINTESTINAL DISORDERS",
        "Any AESOC",
        AESOC
      )
    )

  adsl <- cards::ADSL |>
    dplyr::filter(
      SAFFL == "Y",
      TRTA %in% c("Placebo", "Xanomeline High Dose")
    ) |>
    dplyr::mutate(
      TRTA = dplyr::if_else(
        TRTA == "Xanomeline High Dose",
        "Overall TRTA",
        TRTA
      )
    )

  ard <- cards::ard_stack_hierarchical(
    data = adae,
    by = c(TRTA, SEX),
    variables = c(AESOC, AETERM),
    denominator = adsl,
    id = USUBJID,
    overall = TRUE,
    over_variables = TRUE,
    total_n = TRUE
  )

  expect_snapshot(
    shuffled_ard <- ard |>
      shuffle_card()
  )

  expect_identical(
    shuffled_ard |>
      dplyr::filter(
        context == "total_n"
      ) |>
      dplyr::select(
        TRTA,
        AESOC,
        SEX
      ),
    data.frame(
      TRTA = "Overall TRTA.1",
      AESOC = NA_character_,
      SEX = "Overall SEX"
    ),
    # the shuffled_ard preserves the card attributes and returns a tibble. We
    # need to ignore the attributes for the purpose of this comparison
    ignore_attr = TRUE
  )

  # expect_identical(
  #   shuffled_ard |>
  #     dplyr::filter(
  #       variable == "..ard_hierarchical_overall.."
  #     ) |>
  #     dplyr::pull(AESOC) |>
  #     unique(),
  #   "Any AESOC.1"
  # )

})


test_that("shuffle_card() preserves the attributes of a `card` object", {
  adae <- cards::ADAE |>
    dplyr::filter(
      SAFFL == "Y",
      TRTA %in% c("Placebo", "Xanomeline High Dose"),
      AESOC %in% unique(AESOC)[1:2]
    ) |>
    dplyr::group_by(AESOC) |>
    dplyr::filter(
      AETERM %in% unique(AETERM)[1:2]
    ) |>
    dplyr::ungroup()

  adsl <- cards::ADSL |>
    dplyr::filter(
      SAFFL == "Y",
      TRTA %in% c("Placebo", "Xanomeline High Dose")
    )

  ard <- cards::ard_stack_hierarchical(
    data = adae,
    by = TRTA,
    variables = c(AESOC, AETERM),
    denominator = adsl,
    id = USUBJID,
    overall = TRUE,
    over_variables = TRUE,
    total_n = TRUE
  )

  shuffled_ard <- shuffle_card(ard)

  expect_identical(
    attributes(ard)[["args"]],
    attributes(shuffled_ard)[["args"]]
  )
})


test_that("shuffle_card() sorting option", {
  withr::local_seed(0829)

  ard <- cards::ard_continuous(cards::ADSL, by = "ARM", variables = "AGE")
  # randomly sort data
  ard_mixed <- dplyr::slice(ard, sample.int(nrow(ard)))

  ard_unnest <- ard_mixed |>
    cards::rename_ard_columns(fill = "Overall {colname}") |>
    cards::unlist_ard_columns() |>
    dplyr::select(-any_of(c("fmt_fn", "fmt_fun", "warning", "error")))

  expect_equal(
    ard_unnest,
    shuffle_card(ard_mixed, order_rows = FALSE) |> dplyr::select(-stat_variable),
    ignore_attr = TRUE
  )
})
