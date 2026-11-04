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
      ! Entries of a `span_structure` must be named:
       span_structure(`blah blah blah`)

---

    Code
      span_structure(name_col = 0+1i)
    Condition
      Error in `purrr::map()`:
      i In index: 1.
      i With name: name_col.
      Caused by error in `lapply()`:
      ! Invalid entry: `0 + (0+1i)`
      Only selection helpers (See <https://tidyselect.r-lib.org/reference>),  or unquoted expressions representing variable names  can be entered as contents. Changing the names of individual variables using new_name = old_name syntax is allowable

---

    Code
      span_structure(name_col = vars(matrix()))
    Condition
      Error in `purrr::map()`:
      i In index: 1.
      i With name: name_col.
      Caused by error in `lapply()`:
      ! Invalid entry: `matrix()`
      Only selection helpers (See <https://tidyselect.r-lib.org/reference>),  or unquoted expressions representing variable names  can be entered as contents. Changing the names of individual variables using new_name = old_name syntax is allowable

---

    Code
      span_structure(col1 = "", col2 = vars(A, B))
    Condition
      Error in `span_structure()`:
      ! The following `span_structure()` arguments have no column values: `col1`

---

    Code
      span_structure(col1 = "Test Label", col2 = c())
    Condition
      Error in `span_structure()`:
      ! The following `span_structure()` arguments have no column values: `col2`

