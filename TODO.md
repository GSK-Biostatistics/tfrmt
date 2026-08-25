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
- [ ] functions should check their inputs. If the function is not user facing, then it should report the error / condition from the calling function. For example, `apply_tfrmt()` should have a `call = rlang::caller_env()` argument.
- [ ] all user-facing functions should check all their inputs before doing any work.
  - for example, `big_n_structure()` should check `param_val`
- [ ] all S3 classes introduced by {tfrmt} should have matching `is_<class>()` and `check_<class>()` functions.

## Performance


## Quality of life (aka spring cleaning)

- [ ] prefer `expect_equal()` to `expect_identical()` (contrary to {lintr}). Follow Hadley's advice: "Numerical precision can also vary across platforms, so use `expect_equal()` unless you have a specific reason for using `expect_identical()`." @dragosmg


## Documentation


# Backlog (postponed tasks)


# Done ✓

- [x] use namespaced calls @dragosmg
- [x] ~~This task has been declined~~ (declined)
