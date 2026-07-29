# page plan with multiple structures

    Code
      auto_split <- apply_page_plan(df, my_page_plan, vars(grp1, grp2), quo(lbl))
    Message
      `page_plan` contains multiple `page_structures` with values set to ".default".
      Only the last one specified will be used.

# Page plan with max_rows edge cases: spanning and too-small max_rows

    Code
      result <- apply_tfrmt(df, mytfrmt)
      result
    Output
      [[1]]
      # A tibble: 3 x 4
      # Groups:   grp1 [1]
        grp1  lbl   trt   ..tfrmt_row_grp_lbl
      * <chr> <chr> <chr> <lgl>              
      1 AA    "A"   <NA>  TRUE               
      2 AA    "  a" 22    FALSE              
      3 AA    "  b" 11    FALSE              
      
      [[2]]
      # A tibble: 4 x 4
      # Groups:   grp1 [2]
        grp1  lbl   trt   ..tfrmt_row_grp_lbl
      * <chr> <chr> <chr> <lgl>              
      1 AA    "B"   <NA>  TRUE               
      2 AA    "  a" 24    FALSE              
      3 BB    "B"   <NA>  TRUE               
      4 BB    "  b" 55    FALSE              
      
      [[3]]
      # A tibble: 3 x 4
      # Groups:   grp1 [1]
        grp1  lbl   trt   ..tfrmt_row_grp_lbl
      * <chr> <chr> <chr> <lgl>              
      1 BB    "C"   <NA>  TRUE               
      2 BB    "  a" 12    FALSE              
      3 BB    "  b" 19    FALSE              
      

---

    Code
      result <- apply_tfrmt(df, mytfrmt)
    Message
      Unable to complete pagination because `max_rows` specified in `page_plan` is smaller than the number of rows dedicated to group labels. Suggest increasing `max_rows` and trying again.
    Code
      result
    Output
      # A tibble: 12 x 3
         lbl     trt   ..tfrmt_row_grp_lbl
       * <chr>   <chr> <lgl>              
       1 "AA"    <NA>  TRUE               
       2 "  A"   <NA>  TRUE               
       3 "    a" 22    FALSE              
       4 "    b" 11    FALSE              
       5 "  B"   <NA>  TRUE               
       6 "    a" 24    FALSE              
       7 "BB"    <NA>  TRUE               
       8 "  B"   <NA>  TRUE               
       9 "    b" 55    FALSE              
      10 "  C"   <NA>  TRUE               
      11 "    a" 12    FALSE              
      12 "    b" 19    FALSE              

