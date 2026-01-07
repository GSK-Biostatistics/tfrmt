# print_to_gt() complains with incorrect inputs

    Code
      print_to_gt(mtcars)
    Condition
      Error in `print_to_gt()`:
      ! Requires a tfrmt object

---

    Code
      print_to_gt(tfrmt_spec, "foo")
    Condition
      Error in `print_to_gt()`:
      ! Requires data, if not available please use `print_mock_gt()`

