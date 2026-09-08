test_that("is_element_block", {
    expect_true(is_element_block(element_block()))
    expect_false(is_element_block("foo"))
})
test_that("check_element_block", {
    expect_no_error(
        check_element_block(element_block())
    )
    expect_no_error(
        check_element_block(NULL, allow_null = TRUE)
    )

    expect_error(
        check_element_block("foo"),
        '`"foo"` must be an element_block object, not the string "foo".'
    )
})
