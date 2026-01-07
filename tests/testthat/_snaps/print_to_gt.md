# print_to_gt() works

    Code
      print_to_gt(tfrmt_spec, .data = dat)[["_data"]]
    Output
      # A tibble: 2 x 8
        label     span01___tlang_delim~1 span01___tlang_delim~2 span01___tlang_delim~3
        <chr>     <chr>                  <chr>                  <chr>                 
      1 "mygrp"   <NA>                   <NA>                   <NA>                  
      2 "  mylbl" 1.00                   1.00                   1.00                  
      # i abbreviated names:
      #   1: span01___tlang_delim___span1___tlang_delim___lower1_a,
      #   2: span01___tlang_delim___span1___tlang_delim___lower1_b,
      #   3: span01___tlang_delim___span2___tlang_delim___lower2_a
      # i 4 more variables:
      #   span01___tlang_delim___span2___tlang_delim___lower2_b <chr>,
      #   span02___tlang_delim___span3___tlang_delim___lower2_a <chr>, ...

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

# print_mock_gt() messages when tfrmt$param is missing

    Code
      print_mock_gt(tfrmt_spec_no_param)[["_data"]]
    Message
      `tfrmt` will need a `param` value to `print_to_gt` when data is available
    Output
      # A tibble: 3 x 5
        label   column1     column2     column3     ..tfrmt_row_grp_lbl
        <chr>   <chr>       <chr>       <chr>       <lgl>              
      1 label_1 xxx (xx.x%) xxx (xx.x%) xxx (xx.x%) FALSE              
      2 label_2 xxx (xx.x%) xxx (xx.x%) xxx (xx.x%) FALSE              
      3 label_3 xxx (xx.x%) xxx (xx.x%) xxx (xx.x%) FALSE              

# print_mock_gt() messages when tfrmt$column is missing

    Code
      print_mock_gt(tfrmt_spec_no_column)[["_data"]]
    Message
      `tfrmt` will need `column` value(s) to `print_to_gt` when data is available
    Output
      # A tibble: 3 x 5
        label   `__tfrmt__column1` `__tfrmt__column2` `__tfrmt__column3`
        <chr>   <chr>              <chr>              <chr>             
      1 label_1 xxx (xx.x%)        xxx (xx.x%)        xxx (xx.x%)       
      2 label_2 xxx (xx.x%)        xxx (xx.x%)        xxx (xx.x%)       
      3 label_3 xxx (xx.x%)        xxx (xx.x%)        xxx (xx.x%)       
      # i 1 more variable: ..tfrmt_row_grp_lbl <lgl>

# print_mock_gt() messages when tfrmt$value is missing

    Code
      print_mock_gt(tfrmt_spec_no_value)[["_data"]]
    Message
      Message: `tfrmt` will need `value` value to `print_to_gt` when data is available
    Output
      # A tibble: 3 x 5
        label   column1     column2     column3     ..tfrmt_row_grp_lbl
        <chr>   <chr>       <chr>       <chr>       <lgl>              
      1 label_1 xxx (xx.x%) xxx (xx.x%) xxx (xx.x%) FALSE              
      2 label_2 xxx (xx.x%) xxx (xx.x%) xxx (xx.x%) FALSE              
      3 label_3 xxx (xx.x%) xxx (xx.x%) xxx (xx.x%) FALSE              

# print_mock_gt() with missing body_plan

    Code
      print_mock_gt(tfrmt_spec_no_body_plan)[["_data"]]
    Output
      # A tibble: 3 x 5
        label   column1 column2 column3 ..tfrmt_row_grp_lbl
        <chr>   <chr>   <chr>   <chr>   <lgl>              
      1 label_1 X.X     X.X     X.X     FALSE              
      2 label_2 X.X     X.X     X.X     FALSE              
      3 label_3 X.X     X.X     X.X     FALSE              

# print_mock_data() removes `value` when it exists in the input data

    Code
      print_mock_gt(plan, data)
    Message
       Removing `value_to_remove` from input data for mocking.

