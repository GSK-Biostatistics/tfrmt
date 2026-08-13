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

test_that("check_frmt", {
    expect_no_error(
        check_frmt(
            structure(
                "foo",
                class = "frmt"
            )
        )
    )

    expect_no_error(check_frmt(NULL, allow_null = TRUE))

    expect_error(
        check_frmt("foo", ),
        '`"foo"` must be a frmt object, not the string "foo".'
    )
})

test_that("is_frmt_strict", {
    expect_true(
        is_frmt_strict(
            structure(
                rlang::expr(foo(bar)),
                class = "frmt"
            )
        )
    )

    # the input must not have additional classes
    expect_false(
        is_frmt_strict(
            structure(
                rlang::expr(foo(bar)),
                class = c("frmt", "frmt_when")
            )
        )
    )
})

test_that("check_frmt_strict", {
    expect_no_error(
        check_frmt_strict(
            structure(
                rlang::expr(foo(bar)),
                class = "frmt"
            )
        )
    )

    expect_no_error(
        check_frmt_strict(
            NULL,
            allow_null = TRUE
        )
    )

    # the input must not have additional classes
    expect_error(
        check_frmt_strict(
            structure(
                rlang::expr(foo(bar)),
                class = c("frmt_when", "frmt")
            )
        ),
        "must be a <frmt> object, not a <frmt_when> object."
    )
})

test_that("is_frmt_combine", {
    expect_true(
        is_frmt_combine(
            structure(
                rlang::expr(foo(bar)),
                class = c("frmt", "frmt_combine")
            )
        )
    )

    expect_true(
        is_frmt_combine(
            structure(
                rlang::expr(foo(bar)),
                class = c("frmt_combine")
            )
        )
    )

    expect_false(
        is_frmt_combine(
            "foo"
        )
    )
})

test_that("check_frmt_combine", {
    expect_no_error(
        check_frmt_combine(
            structure(
                rlang::expr(foo(bar)),
                class = c("frmt", "frmt_combine")
            )
        )
    )

    expect_no_error(
        check_frmt_combine(
            structure(
                rlang::expr(foo(bar)),
                class = c("frmt_combine")
            )
        )
    )

    expect_no_error(check_frmt_combine(NULL, allow_null = TRUE))

    expect_error(
        check_frmt_combine("foo"),
        '`"foo"` must be a frmt combine object, not the string "foo".'
    )
})
