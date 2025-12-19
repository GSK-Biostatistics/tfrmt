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
      purrr::map(create_col_order(data_names = c("grp2", "lbl", "ord", "1"), columns = rlang::quos(
        column), cp = col_plan(-ord)), rlang::quo_get_expr)
    Output
      [[1]]
      -ord
      
      [[2]]
      grp2
      
      [[3]]
      lbl
      
      [[4]]
      `1`
      

# create_col_order() with NULL col plan

    Code
      purrr::map(create_col_order(data_names = c("grp2", "lbl", "ord", "1"), columns = rlang::quos(
        column), cp = NULL), rlang::quo_get_expr)
    Output
      [[1]]
      grp2
      
      [[2]]
      lbl
      
      [[3]]
      ord
      
      [[4]]
      `1`
      

# create_col_order() with empty columns arg & cp not NULL

    Code
      purrr::map(create_col_order(data_names = c("grp2", "lbl", "ord", "1"), columns = rlang::quos(),
      cp = col_plan(-ord)), rlang::quo_get_expr)
    Output
      [[1]]
      -ord
      
      [[2]]
      grp2
      
      [[3]]
      lbl
      
      [[4]]
      `1`
      

# create_col_order() with span_structure()

    Code
      purrr::map(create_col_order(data_names = c("group", "label",
        "cols 1,2___tlang_delim___col1", "cols 1,2___tlang_delim___col2", "mycol3",
        "col 4___tlang_delim___col4", "mycol5"), columns = rlang::quos(span1, my_col),
      cp = col_plan(span_structure(span1 = c(`first cols` = "cols 1,2")), group,
      label, starts_with("col"), new_col_3 = mycol3, -mycol5)), rlang::quo_get_expr)
    Output
      [[1]]
      group
      
      [[2]]
      label
      
      $`first cols___tlang_delim___col1`
      `cols 1,2___tlang_delim___col1`
      
      $`first cols___tlang_delim___col2`
      `cols 1,2___tlang_delim___col2`
      
      [[5]]
      `col 4___tlang_delim___col4`
      
      $new_col_3
      mycol3
      
      [[7]]
      -mycol5
      

