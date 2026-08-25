test_that("multiplication works", {
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

    expect_s3_class(test_grp_plan, "row_grp_plan")
    expect_snapshot(test_grp_plan)
})
