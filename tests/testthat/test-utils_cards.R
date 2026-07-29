test_that("is_card works correctly", {
    # Setup mock objects
    good_card <- structure(list(), class = "card")
    bad_card <- list()

    expect_true(is_card(good_card))
    expect_false(is_card(bad_card))
})

test_that("is_bind_ard_card works correctly", {
    good_bind <- structure(list(), class = "bind_ard")
    bad_bind <- structure(list(), class = "card")

    expect_true(is_bind_ard_card(good_bind))
    expect_false(is_bind_ard_card(bad_bind))
})

test_that("check_card passes valid inputs and handles allow_null", {
    valid_card <- structure(list(), class = "card")

    # Valid card should pass invisibly
    expect_invisible(check_card(valid_card))
    expect_null(check_card(valid_card))

    # NULL should fail by default
    expect_error(
        check_card(NULL),
        regexp = "`NULL` must be a card object, not NULL"
    )

    # NULL should pass when allow_null = TRUE
    expect_invisible(check_card(NULL, allow_null = TRUE))
    expect_null(check_card(NULL, allow_null = TRUE))
})

test_that("check_card throws correct cli errors for invalid inputs", {
    invalid_input <- "not a card"

    # Test standard error throwing and message formatting
    expect_error(
        check_card(invalid_input, arg = "my_arg"),
        regexp = "`my_arg` must be a card object"
    )

    # Missing argument should also fall through to stop_input_type
    expect_error(
        check_card(),
        regexp = 'argument "expr" is missing'
    )
})

test_that("get_card_attr_arg extracts attributes properly", {
    mock_card <- structure(
        data.frame(),
        class = "card",
        args = list(
            by = "group_var",
            strata = "age_strata"
        )
    )

    # Default "by" argument
    expect_equal(
        get_card_attr_arg(mock_card),
        "group_var"
    )

    # Custom argument extraction
    expect_equal(
        get_card_attr_arg(mock_card, arg = "strata"),
        "age_strata"
    )

    # Non-existent argument returns NULL
    expect_null(
        get_card_attr_arg(mock_card, arg = "missing_arg")
    )
})

test_that("set_card_args modifies parameters properly", {
    mock_card <- structure(
        data.frame(),
        class = "card",
        args = list(by = "old")
    )

    # Modify an existing attribute element
    modified_card <- set_card_args(mock_card, "by", "new")
    expect_equal(
        attr(modified_card, "args")$by,
        "new"
    )

    # Add a new attribute element
    modified_card <- set_card_args(modified_card, "variable", "var_name")
    expect_equal(
        attr(modified_card, "args")$variable,
        "var_name"
    )
})

test_that("drop_bind_ard_args clears stale args on bind_ard objects", {
    # Case 1: Object is a bind_ard and has args attribute -> should drop targeted keys
    mock_bind_ard <- structure(
        data.frame(),
        class = "bind_ard",
        args = list(
            by = "sex",
            variable = "bmi",
            strata = "site",
            leave_me = "keep"
        )
    )

    cleaned <- drop_bind_ard_args(mock_bind_ard)
    expect_null(attr(cleaned, "args")$by)
    expect_null(attr(cleaned, "args")$variable)
    expect_null(attr(cleaned, "args")$strata)
    expect_equal(attr(cleaned, "args")$leave_me, "keep")

    # Case 2: Object is a bind_ard but has NO args attribute -> should do nothing without crashing
    mock_no_args <- structure(data.frame(), class = "bind_ard")
    expect_equal(drop_bind_ard_args(mock_no_args), mock_no_args)

    # Case 3: Object is NOT a bind_ard -> should leave args untouched
    mock_regular_card <- structure(
        data.frame(),
        class = "card",
        args = list(
            by = "sex",
            variable = "bmi"
        )
    )
    expect_equal(
        drop_bind_ard_args(mock_regular_card),
        mock_regular_card
    )
})
