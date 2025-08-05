# not enough big Ns by page

    Code
      apply_tfrmt(.data = data, tfrmt = mytfrmt, mock = FALSE)
    Message
      Mismatch between big Ns and page_plan. For varying big N's by page (`by_page` = TRUE in `big_n_structure`), data must contain 1 big N value per unique grouping variable/value set to ".default" in `page_plan`
    Output
      [[1]]
      # A tibble: 3 x 5
        Label     `Placebo\nN = 12` `Treatment\nN = 14` `Total\nN = 31`
      * <chr>     <chr>             <chr>               <chr>          
      1 "Age (y)" <NA>              <NA>                <NA>           
      2 "  n"     "12"              "14"                "31"           
      3 "   "     " "               " "                 " "            
      # i 1 more variable: ..tfrmt_row_grp_lbl <lgl>
      
      [[2]]
      # A tibble: 3 x 5
        Label `Placebo\nN = 12` `Treatment\nN = 14` `Total\nN = 31`
      * <chr> <chr>             <chr>               <chr>          
      1 "Sex" <NA>              <NA>                <NA>           
      2 "  n" "20"              "32"                "18"           
      3 "   " " "               " "                 " "            
      # i 1 more variable: ..tfrmt_row_grp_lbl <lgl>
      

