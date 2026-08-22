test_that("is_tfrmt", {
    expect_true(
        is_tfrmt(
            structure(
                "foo",
                class = "tfrmt"
            )
        )
    )

    expect_false(is_tfrmt("foo"))
})

test_that("check_tfrmt", {
    expect_no_error(
        check_tfrmt(
            structure(
                "foo",
                class = "tfrmt"
            )
        )
    )

    expect_no_error(
        check_tfrmt(
            NULL,
            allow_null = TRUE
        )
    )

    expect_error(
        check_tfrmt(
            "foo"
        ),
        '`"foo"` must be a tfrmt object, not the string "foo"'
    )

    expect_error(
        check_tfrmt(
            TRUE
        ),
        "`TRUE` must be a tfrmt object, not `TRUE`"
    )
})
