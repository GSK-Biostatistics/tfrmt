# TODO

This text is not a task.

## Bugs

This is a task ~3d \#feat @john 2020-03-20

This is a task with a subtask ~1d \#bug @jane

And it has a subtask!

## Improvements

revisit some of the `check_` functions. I think the default approach
should be the aim of the `check_()` functions is only to verify the
input is as expected. Currently these functions do a lot more:

`check_col_plan_dots()`

`check_span_structure_dots()`

revisit `is_valid_()` functionality

the constructor should always produce a valid object

some `is_()` functions should probably not be user facing
(i.e. exported), such as:
[`is_frmt_structure()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/frmt_utils.md),
[`is_frmt()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/frmt_utils.md),
[`is_frmt_combine()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/frmt_utils.md),
[`is_frmt_when()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/frmt_utils.md),
[`is_row_grp_structure()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/frmt_utils.md).

improve messaging (to discuss):

messages should be aware of the context (errors and warnings should
surface from the functions directly called by the users)

- no `call = NULL`, but rather `call = rlang::caller_env()`

improve multi-row cli messages (by adding bullet points)

use inline formatting

wrap lines to 80 characters

revisit / add `is_()` and `check_()` functions for the tfrmt classes

implement existing rlang checks, for example:

- `check_logical()` in
  [`display_row_frmts()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/display_row_frmts.md)
  instead of having a conditional statement and the very bottom of the
  function body
- `check_whole_number()` for width in
  [`col_style_structure()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/theme_element.md).
  Do we want to continue supporting `width` as character in
  [`col_style_structure()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/theme_element.md)?

decide on a consistent approach to:

- classing the message
- including or not including context

functions should check their inputs. If the function is not user facing,
then it should report the error / condition from the calling function.
For example, `apply_tfrmt()` should have a `call = rlang::caller_env()`
argument.

all user-facing functions should check all their inputs before doing any
work.

- for example,
  [`big_n_structure()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/big_n_structure.md)
  should check `param_val`

all S3 classes introduced by {tfrmt} should have matching `is_<class>()`
and `check_<class>()` functions.

## Performance

## Quality of life (aka spring cleaning)

prefer `expect_equal()` to `expect_identical()` (contrary to {lintr}).
Follow Hadley’s advice: “Numerical precision can also vary across
platforms, so use `expect_equal()` unless you have a specific reason for
using `expect_identical()`.” @dragosmg

## Documentation

# Backlog (postponed tasks)

# Done ✓

use namespaced calls @dragosmg

~~This is an example of a declined task~~ (declined)
