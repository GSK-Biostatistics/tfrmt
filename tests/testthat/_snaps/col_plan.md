# span_structure misc, including errors

    Code
      span_structure()
    Condition
      Error in `span_structure()`:
      ! `span_structure()` requires at least one named argument.

---

    Code
      span_structure(`blah blah blah`)
    Condition
      Error in `span_structure()`:
      ! Entries of a `span_structure()` must be named.
      i Got: span_structure(`blah blah blah`)

---

    Code
      span_structure(name_col = 0+1i)
    Condition
      Error in `span_structure()`:
      ! Invalid entry: `0 + (0+1i)`
      x Only the following can be entered as contents:
      * selection helpers (see <https://tidyselect.r-lib.org/reference>),
      * unquoted expressions representing variable names.
      i Renaming variables with `new_name = old_name` is allowed.

---

    Code
      span_structure(name_col = vars(matrix()))
    Condition
      Error in `span_structure()`:
      ! Invalid entry: `matrix()`
      x Only the following can be entered as contents:
      * selection helpers (see <https://tidyselect.r-lib.org/reference>),
      * unquoted expressions representing variable names.
      i Renaming variables with `new_name = old_name` is allowed.

---

    Code
      span_structure(col1 = "", col2 = vars(A, B))
    Condition
      Error in `span_structure()`:
      ! The following `span_structure()` arguments have no column values:
      x `col1`

---

    Code
      span_structure(col1 = "Test Label", col2 = c())
    Condition
      Error in `span_structure()`:
      ! The following `span_structure()` arguments have no column values:
      x `col2`

