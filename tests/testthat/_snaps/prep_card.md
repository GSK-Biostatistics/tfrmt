# prep_combine_vars() works

    Code
      prep_combine_vars(df, vars = c("b", "c", "d", "e", "f", "g"))
    Output
      # A tibble: 6 x 3
            a context     variable_level
        <int> <chr>       <chr>         
      1     1 categorical a             
      2     2 categorical b             
      3     3 categorical c             
      4     4 categorical d             
      5     5 categorical e             
      6     6 categorical f             

---

    Code
      prep_combine_vars(df, vars = c("b", "c", "d", "e", "f"), remove = FALSE)
    Output
      # A tibble: 6 x 9
            a context     variable_level b     c     d     e     f     g    
        <int> <chr>       <chr>          <chr> <chr> <chr> <chr> <chr> <chr>
      1     1 categorical a              a     <NA>  <NA>  <NA>  <NA>  <NA> 
      2     2 categorical b              <NA>  b     <NA>  <NA>  <NA>  <NA> 
      3     3 categorical c              <NA>  <NA>  c     <NA>  <NA>  <NA> 
      4     4 categorical d              <NA>  <NA>  <NA>  d     <NA>  <NA> 
      5     5 categorical e              <NA>  <NA>  <NA>  <NA>  e     <NA> 
      6     6 categorical <NA>           <NA>  <NA>  <NA>  <NA>  <NA>  f    

# prep_big_n() works

    Code
      prep_big_n(df, vars = c("b", "c"))
    Output
      # A tibble: 6 x 3
        stat_name context      stat_variable
        <chr>     <chr>        <chr>        
      1 n         continuous   a            
      2 max       continuous   a            
      3 min       continuous   a            
      4 bigN      hierarchical b            
      5 bigN      categorical  c            
      6 bigN      total_n      d            

---

    Code
      prep_big_n(df, vars = "b")
    Output
      # A tibble: 8 x 3
        stat_name context      stat_variable
        <chr>     <chr>        <chr>        
      1 n         continuous   a            
      2 max       continuous   a            
      3 min       continuous   a            
      4 bigN      hierarchical b            
      5 n         categorical  c            
      6 N         categorical  c            
      7 p         categorical  c            
      8 bigN      total_n      d            

# prep_label() works

    Code
      prep_label(df)
    Output
      # A tibble: 3 x 4
        variable_level stat_label context      label
        <chr>          <chr>      <chr>        <chr>
      1 d              a          categorical  d    
      2 e              b          continuous   b    
      3 f              c          hierarchical c    

# prep_label() returns the input when the required cols are missing

    Code
      prep_label(df)
    Message
      i Both `variable_level` and `stat_label` columns need to be present in the input data.
      * They are not so the input `data.frame` will be returned unmodified.
    Output
      # A tibble: 3 x 3
        x     stat_label context     
        <chr> <chr>      <chr>       
      1 d     a          categorical 
      2 e     b          continuous  
      3 f     c          hierarchical

---

    Code
      prep_label(df2)
    Message
      i Both `variable_level` and `stat_label` columns need to be present in the input data.
      * They are not so the input `data.frame` will be returned unmodified.
    Output
      # A tibble: 3 x 3
        variable_level y     context     
        <chr>          <chr> <chr>       
      1 d              a     categorical 
      2 e              b     continuous  
      3 f              c     hierarchical

# prep_hierarchical_fill() complains with `fill_from` other than 'left'

    Code
      prep_hierarchical_fill(df, vars = c("x", "y", "z"), fill_from = "foo")
    Condition
      Error in `prep_hierarchical_fill()`:
      ! `fill_from` must either be `NULL` or `"left"`. `"foo"` is not an accepted value.

# prep_hierarchical_fill() errors when `fill` is not character

    Code
      prep_hierarchical_fill(df, vars = c("x", "y", "z"), fill = 2)
    Condition
      Error in `prep_hierarchical_fill()`:
      ! `fill` must be a character vector of length 1.

# generate_pairs() complains

    Code
      generate_pairs(1:3)
    Condition
      Error in `generate_pairs()`:
      ! `x` must be a character vector. You have supplied an integer vector.

---

    Code
      generate_pairs("foo")
    Condition
      Error:
      ! `x` must contain at least 2 column names. It contains 1.

# replace_na_pairwise() complains

    Code
      replace_na_pairwise(tibble::tibble(x = c(1, 2, NA), y = c("a", NA, "b"), z = rep(
        NA, 3)), pair = 1:2)
    Condition
      Error:
      ! `pair` must be a character vector. You have supplied an integer vector.

---

    Code
      replace_na_pairwise(tibble::tibble(x = c(1, 2, NA), y = c("a", NA, "b"), z = rep(
        NA, 3)), pair = c("x", "y", "z"))
    Condition
      Error:
      ! `pair` must contain exactly 2 elements. The one you supplied has 3.

---

    Code
      replace_na_pairwise(tibble::tibble(x = c(1, 2, NA), y = c("a", NA, "b"), z = rep(
        NA, "3")), pair = c("y", "z"), fill_from = "right")
    Condition
      Error:
      ! `fill_from` must either be `NULL` or `"left"`. `"right"` is not an accepted value.

---

    Code
      replace_na_pairwise(tibble::tibble(x = c(1, 2, NA), y = c("a", NA, "b"), z = rep(
        NA, "3")), pair = c("y", "z"), fill = 2)
    Condition
      Error:
      ! `fill` must be a character vector of length 1.

