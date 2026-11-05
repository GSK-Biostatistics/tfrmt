# invalid calls error

    Code
      parse_col_plan_entry(quote(foo(x)))
    Condition
      Error:
      ! Invalid entry: `foo(x)`
      x Only the following can be entered as contents:
      * span structures (`span_structure()`),
      * selection helpers (see <https://tidyselect.r-lib.org/reference>),
      * unquoted expressions representing variable names.
      i Renaming variables with `new_name = old_name` is allowed.

---

    Code
      col_plan(foo(x))
    Condition
      Error in `col_plan()`:
      ! Invalid entry: `foo(x)`
      x Only the following can be entered as contents:
      * span structures (`span_structure()`),
      * selection helpers (see <https://tidyselect.r-lib.org/reference>),
      * unquoted expressions representing variable names.
      i Renaming variables with `new_name = old_name` is allowed.

# invalid calls inside span_structure() omit the span structure bullet

    Code
      span_structure(a = foo(x))
    Condition
      Error in `span_structure()`:
      ! Invalid entry: `foo(x)`
      x Only the following can be entered as contents:
      * selection helpers (see <https://tidyselect.r-lib.org/reference>),
      * unquoted expressions representing variable names.
      i Renaming variables with `new_name = old_name` is allowed.

