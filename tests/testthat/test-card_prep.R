test_that("multiplication works", {
  adsl <- pharmaverseadam::adsl |>
    filter(SAFFL == "Y") |>
    select(USUBJID, ARM, SEX, AGE, AGEGR1, ETHNIC, RACE) |>
    mutate(
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

  # ard <- ard_stack(
  #   data = adsl,
  #   .by = ARM,
  #   ard_continuous(variables = AGE,
  #                  statistic = ~ continuous_summary_fns(c("N","mean","sd","min","max"))),
  #   ard_categorical(variables = c(AGEGR1, SEX, ETHNIC, RACE)),
  #   .overall = TRUE,
  #   .total_n = TRUE
  # )


  ard_no_attributes <- ard_stack(
    data = adsl,
    .by = ARM,
    ard_continuous(
      variables = AGE,
      statistic = ~ continuous_summary_fns(
        c("N", "mean", "sd", "min", "max")
      )
    ),
    ard_categorical(
      variables = c(AGEGR1, SEX, ETHNIC, RACE)
    ),
    .overall = TRUE,
    .total_n = TRUE,
    .attributes = FALSE
  )
})
