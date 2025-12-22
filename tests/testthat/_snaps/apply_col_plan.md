# apply_col_plan() works

    Code
      apply_col_plan(df1, col_selection = "-ord", grp_lbl = rlang::quos(grp2, lbl))
    Output
      # A tibble: 7 x 5
        grp2  lbl   prm   column   val
        <chr> <chr> <chr>  <dbl> <dbl>
      1 c     n     n          1   1  
      2 c     n     n_2        1   1.1
      3 b     m     n          1   2  
      4 b     m     n_2        1   2.1
      5 v     s     n          1   3  
      6 v     s     n_3        1   3.3
      7 p     e     n          1   4  

# apply_col_plan() group and label vars are excluded from renaming

    Code
      apply_col_plan(df1, col_selection = c("-ord", "lbl"), grp_lbl = rlang::quos(
        grp2, lbl))
    Output
      # A tibble: 7 x 5
        grp2  lbl   prm   column   val
        <chr> <chr> <chr>  <dbl> <dbl>
      1 c     n     n          1   1  
      2 c     n     n_2        1   1.1
      3 b     m     n          1   2  
      4 b     m     n_2        1   2.1
      5 v     s     n          1   3  
      6 v     s     n_3        1   3.3
      7 p     e     n          1   4  

# create_stub_head() works

    Code
      create_stub_head(col_plan_vars = rlang::quos(-ord, grp2, lbl, `1`), group = rlang::quos(
        grp2), label = rlang::quos(lbl), row_grp_plan_label_loc = "indented")
    Output
      [1] ""

---

    Code
      create_stub_head(col_plan_vars = rlang::quos(-ord, grp2, lbl, `1`), group = rlang::quos(
        grp2), label = rlang::quos(lbl), row_grp_plan_label_loc = "column")
    Output
      [1] "" ""

# create_col_order() works

    Code
      create_col_order(data_names = c("grp2", "lbl", "ord", "1"), columns = rlang::quos(
        column), cp = col_plan(-ord))
    Output
      <list_of<quosure>>
      
      [[1]]
      <quosure>
      expr: ^-ord
      env:  <env-address>
      
      [[2]]
      <quosure>
      expr: ^grp2
      env:  <env-address>
      
      [[3]]
      <quosure>
      expr: ^lbl
      env:  <env-address>
      
      [[4]]
      <quosure>
      expr: ^`1`
      env:  <env-address>
      

# create_col_order() with NULL col plan

    Code
      create_col_order(data_names = c("grp2", "lbl", "ord", "1"), columns = rlang::quos(
        column), cp = NULL)
    Output
      <list_of<quosure>>
      
      [[1]]
      <quosure>
      expr: ^grp2
      env:  <env-address>
      
      [[2]]
      <quosure>
      expr: ^lbl
      env:  <env-address>
      
      [[3]]
      <quosure>
      expr: ^ord
      env:  <env-address>
      
      [[4]]
      <quosure>
      expr: ^`1`
      env:  <env-address>
      

# create_col_order() with empty columns arg & cp not NULL

    Code
      create_col_order(data_names = c("grp2", "lbl", "ord", "1"), columns = rlang::quos(),
      cp = col_plan(-ord))
    Output
      <list_of<quosure>>
      
      [[1]]
      <quosure>
      expr: ^-ord
      env:  <env-address>
      
      [[2]]
      <quosure>
      expr: ^grp2
      env:  <env-address>
      
      [[3]]
      <quosure>
      expr: ^lbl
      env:  <env-address>
      
      [[4]]
      <quosure>
      expr: ^`1`
      env:  <env-address>
      

# create_col_order() with span_structure()

    Code
      create_col_order(data_names = c("group", "label",
        "cols 1,2___tlang_delim___col1", "cols 1,2___tlang_delim___col2", "mycol3",
        "col 4___tlang_delim___col4", "mycol5"), columns = rlang::quos(span1, my_col),
      cp = col_plan(span_structure(span1 = c(`first cols` = "cols 1,2")), group,
      label, starts_with("col"), new_col_3 = mycol3, -mycol5))
    Output
      <list_of<quosure>>
      
      [[1]]
      <quosure>
      expr: ^group
      env:  <env-address>
      
      [[2]]
      <quosure>
      expr: ^label
      env:  <env-address>
      
      $`first cols___tlang_delim___col1`
      <quosure>
      expr: ^`cols 1,2___tlang_delim___col1`
      env:  <env-address>
      
      $`first cols___tlang_delim___col2`
      <quosure>
      expr: ^`cols 1,2___tlang_delim___col2`
      env:  <env-address>
      
      [[5]]
      <quosure>
      expr: ^`col 4___tlang_delim___col4`
      env:  <env-address>
      
      $new_col_3
      <quosure>
      expr: ^mycol3
      env:  <env-address>
      
      [[7]]
      <quosure>
      expr: ^-mycol5
      env:  <env-address>
      

# split_data_names_to_df() works

    Code
      split_data_names_to_df(data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = NULL, column_names = "column")
    Output
      # A tibble: 4 x 3
        column `__tfrmt_new_name__column` subtraction_status
        <chr>  <chr>                      <lgl>             
      1 grp2   grp2                       FALSE             
      2 lbl    lbl                        FALSE             
      3 ord    ord                        FALSE             
      4 1      1                          FALSE             

---

    Code
      split_data_names_to_df(data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = c(order = "ord"), column_names = "column")
    Output
      # A tibble: 4 x 3
        column `__tfrmt_new_name__column` subtraction_status
        <chr>  <chr>                      <lgl>             
      1 ord    order                      FALSE             
      2 grp2   grp2                       FALSE             
      3 lbl    lbl                        FALSE             
      4 1      1                          FALSE             

---

    Code
      split_data_names_to_df(data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = c(order = "ord"), column_names = c("foo", "bar"))
    Output
      # A tibble: 4 x 5
        foo   bar   `__tfrmt_new_name__foo` `__tfrmt_new_name__bar` subtraction_status
        <chr> <chr> <chr>                   <chr>                   <lgl>             
      1 <NA>  ord   <NA>                    order                   FALSE             
      2 <NA>  grp2  <NA>                    grp2                    FALSE             
      3 <NA>  lbl   <NA>                    lbl                     FALSE             
      4 <NA>  1     <NA>                    1                       FALSE             

---

    Code
      split_data_names_to_df(data_names = c("grp2", "lbl", "ord", "1"),
      preselected_cols = "-ord", column_names = "column")
    Output
      # A tibble: 5 x 3
        column `__tfrmt_new_name__column` subtraction_status
        <chr>  <chr>                      <lgl>             
      1 ord    -ord                       TRUE              
      2 grp2   grp2                       FALSE             
      3 lbl    lbl                        FALSE             
      4 ord    ord                        FALSE             
      5 1      1                          FALSE             

