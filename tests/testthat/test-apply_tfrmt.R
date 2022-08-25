


test_that("pivot_wider_tfrmt gives message when frmt_combine may be missing",{

  dat1 <- tibble::tribble(
    ~grp2, ~lbl, ~prm, ~column, ~val, ~ord,
    "c",   "n", "n",   1,   1,  1,
    "c",   "n", "n_2", 1,   1.1,  1,
    "b",   "m", "n",   1,   2,  2,
    "b",   "m", "n_2", 1,   2.1,  2,
    "v",   "s", "n",   1,   3,  3,
    "v",   "s", "n_3",   1,   3.3,  3,
    "p",   "e", "n",   1,   4,  4
  )

  dat2 <- tibble::tribble(
    ~grp1, ~grp2, ~lbl, ~prm, ~column, ~val, ~ord,
    "d",   "c",   "n", "n",   1,   1,  1,
    "d",   "c",   "n", "n_2", 1,   1.1,  1,
    "a",   "b",   "m", "n",   1,   2,  2,
    "a",   "b",   "m", "n_2",   1,   2.1,  2,
    "q",   "v",   "s", "n",   1,   3,  3,
    "b",   "p",   "e", "n",   1,   4,  4
  )


  dat3 <- tibble::tribble(
    ~grp1, ~grp2, ~lbl, ~prm, ~column, ~val, ~ord,
    "d",   "c",   "n", "n",   1,   1,  1,
    "d",   "c",   "n", "n_2", 1,   1.1,  1,
    "a",   "b",   "m", "n",   1,   2,  2,
    "a",   "b",   "m", "n_2", 1,   2.1,  2,
    "q",   "v",   "s", "n",   1,   3,  3,
    "q",   "v",   "s", "n_3",   1,   3.3,  3,
    "b",   "p",   "e", "n",   1,   4,  4
  )



  tfrmt_temp <- tfrmt(
    group = grp2,
    label = lbl,
    column = column,
    value = val,
    param = prm,
    sorting_cols = ord,
    col_plan = col_plan(-ord),
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt("x.x")
        ),
      frmt_structure(
        group_val = ".default",
        label_val = "m",
        frmt_combine("{n}/{n_2}",
                     n = frmt("x"),
                     n_2 = frmt("x.x")
                     )
        )
      )
    )

  expect_message(
    processed_dat <- apply_tfrmt(dat1, tfrmt_temp, mock = FALSE),
    paste0(
      c(
        "Multiple param listed for the same group/label values.",
        "The following frmt_structures may be missing from the body_plan",
        "or the order may need to be changed:",
        "- `frmt_structure(group_val = \"c\", label_val = \"n\", frmt_combine(\"{n}, {n_2}\",n = frmt(\"xx\"), n_2 = frmt(\"xx\")))`",
        "- `frmt_structure(group_val = \"v\", label_val = \"s\", frmt_combine(\"{n}, {n_3}\",n = frmt(\"xx\"), n_3 = frmt(\"xx\")))`"
      ),
      collapse = "\n"
    ),
    fixed = TRUE
  )

  expect_message(
    processed_dat <- apply_tfrmt(dat2, tfrmt_temp %>% tfrmt(group = c(grp1,grp2)), mock = FALSE),
    paste0(
      c(
        "Multiple param listed for the same group/label values.",
        "The following frmt_structures may be missing from the body_plan",
        "or the order may need to be changed:",
        "- `frmt_structure(group_val = list(grp1 = \"d\", grp2 = \"c\"), label_val = \"n\", frmt_combine(\"{n}, {n_2}\",n = frmt(\"xx\"), n_2 = frmt(\"xx\")))`"
      ),
      collapse = "\n"
    ),
    fixed = TRUE
  )

  expect_message(
    processed_dat <- apply_tfrmt(dat3, tfrmt_temp %>% tfrmt(group = c(grp1,grp2)), mock = FALSE),
    paste0(
      c(
        "Multiple param listed for the same group/label values.",
        "The following frmt_structures may be missing from the body_plan",
        "or the order may need to be changed:",
        "- `frmt_structure(group_val = list(grp1 = \"d\", grp2 = \"c\"), label_val = \"n\", frmt_combine(\"{n}, {n_2}\",n = frmt(\"xx\"), n_2 = frmt(\"xx\")))`",
        "- `frmt_structure(group_val = list(grp1 = \"q\", grp2 = \"v\"), label_val = \"s\", frmt_combine(\"{n}, {n_3}\",n = frmt(\"xx\"), n_3 = frmt(\"xx\")))`"
      ),
      collapse = "\n"
    ),
    fixed = TRUE
  )



})
