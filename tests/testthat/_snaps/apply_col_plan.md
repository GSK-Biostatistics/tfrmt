# apply_col_pla() works

    Code
      apply_col_plan(dat1, col_selection = "-ord", grp_lbl = rlang::new_quosures(list(
        quo(grp2), quo(lbl))))
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
      apply_col_plan(dat1, col_selection = c("-ord", "lbl"), grp_lbl = rlang::new_quosures(
        list(quo(grp2), quo(lbl))))
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

