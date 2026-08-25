test_that("row_grp_plan", {
    test_grp_plan <- row_grp_plan(
        row_grp_structure(
            group_val = c("A", "C"),
            element_block = element_block(
                post_space = "---"
            )
        ),
        row_grp_structure(
            group_val = c("B"),
            element_block = element_block(
                post_space = " "
            )
        ),
        label_loc = element_row_grp_loc(
            location = "column"
        )
    )

    expect_s3_class(
        test_grp_plan,
        c("row_grp_plan", "frmt_table")
    )
    expect_snapshot(test_grp_plan)
})

test_that("row_grp_structure", {
    test_grp_structure <- row_grp_structure(
        group_val = list(
            grp1 = "A",
            grp2 = "b"
        ),
        element_block = element_block(
            post_space = " "
        )
    )

    expect_s3_class(
        test_grp_structure,
        c("row_grp_structure", "frmt_table")
    )
    expect_snapshot(test_grp_structure)
})

test_that("row_grp_structure with unnamed list", {
    expect_snapshot(error = TRUE, {
        row_grp_structure(
            group_val = list(
                grp1 = "A",
                "b"
            ),
            element_block = element_block(
                post_space = " "
            )
        )
    })
})
