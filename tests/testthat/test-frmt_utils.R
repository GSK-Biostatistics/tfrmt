test_that("is_frmt", {
    expect_true(
        is_frmt(
            structure(
                "foo",
                class = "frmt"
            )
        )
    )

    expect_false(
        is_frmt(
            "foo"
        )
    )
})
