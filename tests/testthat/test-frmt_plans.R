test_that("frmt_structure is not sensitive to new argument order", {
    # new argument order = ellipsis (`...`) comes first (before `group_val` and
    # `label_val`)

    # set up data
    es_data <- tibble::tibble(
        rowlbl1 = c(
            rep("Completion Status", 12),
            rep("Primary reason for withdrawal", 28)
        ),
        rowlbl2 = c(
            rep("Completed", 4),
            rep("Adverse Event", 4),
            rep("Unknown", 4),
            rep("Adverse Event", 4),
            rep("Lost to follow-up", 4),
            rep("Protocol violation", 4),
            rep("Subject decided to withdraw", 4),
            rep("Protocol Violation", 4),
            rep("Pre-Operative Dose[1]", 4),
            rep("Other", 4)
        ),
        param = c(rep(c("n", "n", "pct", "pct"), 10)),
        trt = c(rep(c("Placebo", "Treatment"), 20)),
        value = c(
            24, 19, 2400 / 48, 1900 / 38, 5, 1, 500 / 48, 100 / 38, 19, 18,
            1900 / 48, 1800 / 38, 1, 1, 100 / 48, 100 / 38, 0, 0, 0, 0, 0, 0, 0,
            0, 1, 1, 100 / 48, 100 / 38, 1, 4, 100 / 48, 400 / 38, 1, 0,
            100 / 48, 0, 2, 3, 200 / 48, 300 / 38
        )
    )

    # tfrmt with old order of the frmt_structure arguments
    tfrmt_old <- tfrmt(
        # specify columns in the data
        group = c(rowlbl1),
        label = rowlbl2,
        column = trt,
        param = param,
        value = value,
        # set formatting for value
        body_plan = body_plan(
            frmt_structure(
                group_val = ".default",
                label_val = ".default",
                frmt_combine(
                    "{n} {pct}",
                    n = frmt("xxx"),
                    pct = frmt_when(
                        "==100" ~ "",
                        "==0" ~ "",
                        TRUE ~ frmt("(xx.x %)")
                    )
                )
            )
        ),
        footnote_plan = footnote_plan(
            footnote_structure(
                "Test footnote 1",
                column_val = "Placebo"
            ),
            marks = "letters"
        )
    )

    tfrmt_new1 <- tfrmt(
        # specify columns in the data
        group = c(rowlbl1),
        label = rowlbl2,
        column = trt,
        param = param,
        value = value,
        # set formatting for value
        body_plan = body_plan(
            frmt_structure(
                frmt_combine(
                    "{n} {pct}",
                    n = frmt("xxx"),
                    pct = frmt_when(
                        "==100" ~ "",
                        "==0" ~ "",
                        TRUE ~ frmt("(xx.x %)")
                    )
                )
            )
        ),
        footnote_plan = footnote_plan(
            footnote_structure(
                "Test footnote 1",
                column_val = "Placebo"
            ),
            marks = "letters"
        )
    )

    tfrmt_new2 <- tfrmt(
        # specify columns in the data
        group = c(rowlbl1),
        label = rowlbl2,
        column = trt,
        param = param,
        value = value,
        # set formatting for value
        body_plan = body_plan(
            frmt_structure(
                frmt_combine(
                    "{n} {pct}",
                    n = frmt("xxx"),
                    pct = frmt_when(
                        "==100" ~ "",
                        "==0" ~ "",
                        TRUE ~ frmt("(xx.x %)")
                    )
                ),
                group_val = ".default",
                label_val = ".default"
            )
        ),
        footnote_plan = footnote_plan(
            footnote_structure(
                "Test footnote 1",
                column_val = "Placebo"
            ),
            marks = "letters"
        )
    )

    expect_no_error({
        output_old <- apply_tfrmt(es_data, tfrmt_old)
    })

    expect_no_error({
        output_new1 <- apply_tfrmt(es_data, tfrmt_new1)
    })

    expect_no_error({
        output_new2 <- apply_tfrmt(es_data, tfrmt_new2)
    })

    expect_equal(
        output_old,
        output_new1,
        ignore_attr = TRUE
    )

    expect_equal(
        output_old,
        output_new2,
        ignore_attr = TRUE
    )
})
