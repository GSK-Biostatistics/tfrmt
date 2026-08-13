test_that("is_frmt", {
    expect_true(is_frmt(frmt("XXX.XX")))
    expect_false(is_frmt("foo"))
})

test_that("check_frmt", {
    expect_no_error(check_frmt(frmt("XXX.XX")))
    expect_no_error(check_frmt(NULL, allow_null = TRUE))

    expect_error(
        check_frmt("foo"),
        '`"foo"` must be a frmt object, not the string "foo".'
    )
})

test_that("is_frmt_strict", {
    expect_true(is_frmt_strict(frmt("XXX.XX")))

    test_frmt <- frmt_when(
        ">3" ~ frmt("(X.X%)"),
        "<=3" ~ frmt("Undetectable")
    )

    # the input must not have additional classes
    expect_s3_class(
        test_frmt,
        c("frmt_when", "frmt")
    )
    expect_false(is_frmt_strict(test_frmt))
})

test_that("check_frmt_strict", {
    expect_no_error(check_frmt_strict(frmt("XXX.XX")))
    expect_no_error(check_frmt_strict(NULL, allow_null = TRUE))

    test_frmt <- frmt_when(
        ">3" ~ frmt("(X.X%)"),
        "<=3" ~ frmt("Undetectable")
    )

    # the input must not have additional classes
    expect_error(
        check_frmt_strict(test_frmt),
        "must be a <frmt> object, not a <frmt_when> object."
    )
})

test_that("is_frmt_combine", {
    expect_true(is_frmt_combine(frmt_combine("XXX %", "XX,XXX")))

    expect_false(is_frmt_combine("foo"))
})

test_that("check_frmt_combine", {
    expect_no_error(check_frmt_combine(frmt_combine("XXX %", "XX,XXX")))
    expect_no_error(check_frmt_combine(NULL, allow_null = TRUE))

    expect_error(
        check_frmt_combine("foo"),
        '`"foo"` must be a frmt combine object, not the string "foo".'
    )
})

test_that("is_frmt_when", {
    expect_true(
        is_frmt_when(
            frmt_when(
                ">3" ~ frmt("(X.X%)"),
                "<=3" ~ frmt("Undetectable")
            )
        )
    )

    expect_false(is_frmt_when("foo"))
})

test_that("check_frmt_when", {
    expect_no_error(
        check_frmt_when(
            frmt_when(
                ">3" ~ frmt("(X.X%)"),
                "<=3" ~ frmt("Undetectable")
            )
        )
    )

    expect_no_error(check_frmt_when(NULL, allow_null = TRUE))

    expect_error(
        check_frmt_when("foo"),
        '`"foo"` must be a frmt when object, not the string "foo".'
    )
})

test_that("is_frmt_structure", {
    expect_true(
        is_frmt_structure(
            frmt_structure(
                group_val = c("group1"),
                label_val = ".default",
                frmt("XXX")
            )
        )
    )

    expect_false(is_frmt_structure("foo"))
})

test_that("check_frmt_structure", {
    expect_no_error(
        check_frmt_structure(
            frmt_structure(
                group_val = c("group1"),
                label_val = ".default",
                frmt("XXX")
            )
        )
    )

    expect_no_error(check_frmt_structure(NULL, allow_null = TRUE))

    expect_error(
        check_frmt_structure("foo"),
        '`"foo"` must be a frmt structure object, not the string "foo".'
    )
})

test_that("is_row_grp_structure", {
    expect_true(
        is_row_grp_structure(
            row_grp_structure(
                group_val = c("A", "C"),
                element_block(post_space = "---")
            )
        )
    )

    expect_false(is_row_grp_structure("foo"))
})


test_that("check_row_grp_structure", {
    expect_no_error(
        check_row_grp_structure(
            row_grp_structure(
                group_val = c("A", "C"),
                element_block(post_space = "---")
            )
        )
    )

    expect_no_error(check_row_grp_structure(NULL, allow_null = TRUE))

    expect_error(
        check_row_grp_structure("foo"),
        '`"foo"` must be a row group structure object, not the string "foo".'
    )
})
