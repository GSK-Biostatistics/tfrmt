# TODO

This text is not a task.

## Bugs

- [ ] This is a task ~3d #feat @john 2020-03-20
- [ ] This is a task with a subtask ~1d #bug @jane
  - [ ] And it has a subtask!

## Improvements

- [ ] revisit some of the `check_` functions. I think the default approach should be the aim of the `check_()` functions is only to verify the input is as expected. Currently these functions do a lot more:
  - [ ] `check_col_plan_dots()`
  - [ ] `check_span_structure_dots()`
- [ ] revisit `is_valid_()` functionality
  - [ ] the constructor should always produce a valid object
- [ ] some `is_()` functions should probably not be user facing (i.e. exported), such as: `is_frmt_structure()`, `is_frmt()`, `is_frmt_combine()`, `is_frmt_when()`, `is_row_grp_structure()`.
- [ ] improve messaging (to discuss):
  - [ ] messages should be aware of the context (errors and warnings should surface from the functions directly called by the users)
    - no `call = NULL`, but rather `call = rlang::caller_env()`
  - [ ] improve multi-row cli messages (by adding bullet points)
  - [ ] use inline formatting
  - [ ] wrap lines to 80 characters
  - [ ] revisit / add `is_()` and `check_()` functions for the tfrmt classes
  - [ ] implement existing rlang checks, for example:
    - `check_logical()` in `display_row_frmts()` instead of having a conditional statement and the very bottom of the function body
    - `check_whole_number()` for width in `col_style_structure()`. Do we want to continue supporting `width` as character in `col_style_structure()`?
  - [ ] decide on a consistent approach to:
    -  classing the message
    - including or not including context

## Performance


## Quality of life (aka spring cleaning)

- [ ] prefer `expect_equal()` to `expect_identical()` (contrary to {lintr}). Follow Hadley's advice: "Numerical precision can also vary across platforms, so use `expect_equal()` unless you have a specific reason for using `expect_identical()`." @dragosmg


## Documentation


# Backlog (postponed tasks)


# Done ✓

- [x] use namespaced calls @dragosmg
- [x] ~~This task has been declined~~ (declined)
