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

# prep_combine_vars() returns the input when context hierarchical

    Code
      prep_combine_vars(df, vars = c("b", "c", "d", "e", "f", "g"))
    Message
      i The `context` column indicates data comes from a hierarchical `ard` stack.
      * Unable to apply `prep_combine_vars()`.
    Output
      # A tibble: 6 x 8
            a context      b     c     d     e     f     g    
        <int> <chr>        <chr> <chr> <chr> <chr> <chr> <chr>
      1     1 hierarchical a     <NA>  <NA>  <NA>  <NA>  <NA> 
      2     2 hierarchical <NA>  b     <NA>  <NA>  <NA>  <NA> 
      3     3 hierarchical <NA>  <NA>  c     <NA>  <NA>  <NA> 
      4     4 hierarchical <NA>  <NA>  <NA>  d     <NA>  <NA> 
      5     5 hierarchical <NA>  <NA>  <NA>  <NA>  e     <NA> 
      6     6 hierarchical <NA>  <NA>  <NA>  <NA>  <NA>  f    

# prep_combine_vars() return input unchanged when length(vars)=1

    Code
      prep_combine_vars(df, vars = "b")
    Message
      i You supplied a single column in `vars`.
      * Unable to apply `prep_combine_vars()`.
    Output
      # A tibble: 6 x 8
            a context     b     c     d     e     f     g    
        <int> <chr>       <chr> <chr> <chr> <chr> <chr> <chr>
      1     1 categorical a     <NA>  <NA>  <NA>  <NA>  <NA> 
      2     2 categorical <NA>  b     <NA>  <NA>  <NA>  <NA> 
      3     3 categorical <NA>  <NA>  c     <NA>  <NA>  <NA> 
      4     4 categorical <NA>  <NA>  <NA>  d     <NA>  <NA> 
      5     5 categorical <NA>  <NA>  <NA>  <NA>  e     <NA> 
      6     6 categorical <NA>  <NA>  <NA>  <NA>  <NA>  f    

# prep_combine_vars() does not over unite

    Code
      prep_combine_vars(df, vars = c("b", "c", "d", "e", "f", "g"))
    Message
      i Combining the columns listed in `vars` would result in a loss of information.
      * Unable to apply `prep_combine_vars()`.
    Output
      # A tibble: 6 x 8
            a context     b     c     d     e     f     g    
        <int> <chr>       <chr> <chr> <chr> <chr> <chr> <chr>
      1     1 categorical a     <NA>  <NA>  <NA>  <NA>  <NA> 
      2     2 categorical <NA>  b     b     b     <NA>  <NA> 
      3     3 categorical <NA>  <NA>  <NA>  <NA>  <NA>  <NA> 
      4     4 categorical <NA>  <NA>  <NA>  <NA>  <NA>  <NA> 
      5     5 categorical <NA>  <NA>  <NA>  <NA>  e     <NA> 
      6     6 categorical <NA>  <NA>  <NA>  <NA>  <NA>  f    

# prep_combine_vars() informs when the context col is missing

    Code
      prep_combine_vars(df, vars = c("b", "c", "d", "e", "f", "g"))
    Message
      i Required column (`context`) not present in the input data.
      * Unable to apply `prep_combine_vars()`.
    Output
      # A tibble: 6 x 7
            a b     c     d     e     f     g    
        <int> <chr> <chr> <chr> <chr> <chr> <chr>
      1     1 a     <NA>  <NA>  <NA>  <NA>  <NA> 
      2     2 <NA>  b     b     b     <NA>  <NA> 
      3     3 <NA>  <NA>  <NA>  <NA>  <NA>  <NA> 
      4     4 <NA>  <NA>  <NA>  <NA>  <NA>  <NA> 
      5     5 <NA>  <NA>  <NA>  <NA>  e     <NA> 
      6     6 <NA>  <NA>  <NA>  <NA>  <NA>  f    

# prep_combine_vars() errors when `vars` is not character

    Code
      prep_combine_vars(df, vars = 1:3)
    Condition
      Error in `prep_combine_vars()`:
      ! `vars` must be a character vector. You have supplied an integer vector.

---

    Code
      prep_combine_vars(df, vars = c(TRUE, FALSE))
    Condition
      Error in `prep_combine_vars()`:
      ! `vars` must be a character vector. You have supplied a logical vector.

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

# prep_big_n() informs when required columns are missing

    Code
      prep_big_n(df, vars = "a")
    Message
      i Required column (`stat_name`) not present in the input data.
      * Unable to apply `prep_big_n()`.
    Output
      # A tibble: 9 x 3
        a     context      stat_variable
        <chr> <chr>        <chr>        
      1 n     continuous   a            
      2 max   continuous   a            
      3 min   continuous   a            
      4 n     hierarchical b            
      5 N     hierarchical b            
      6 p     hierarchical b            
      7 n     categorical  c            
      8 N     categorical  c            
      9 p     categorical  c            

# prep_big_n() errors when `vars` is not character

    Code
      prep_big_n(df, vars = 1)
    Condition
      Error in `prep_big_n()`:
      ! `vars` must be a character vector. You have supplied a number.

---

    Code
      prep_big_n(df, vars = TRUE)
    Condition
      Error in `prep_big_n()`:
      ! `vars` must be a character vector. You have supplied `TRUE`.

# prep_label() works

    Code
      prep_label(df)
    Output
      # A tibble: 9 x 5
        variable_level stat_label stat_name context      label
        <chr>          <chr>      <chr>     <chr>        <chr>
      1 a              n          n         categorical  a    
      2 a              N          N         categorical  a    
      3 a              %          p         categorical  a    
      4 b              N          N         continuous   N    
      5 b              Mean       mean      continuous   Mean 
      6 b              SD         sd        continuous   SD   
      7 c              n          n         hierarchical c    
      8 c              N          N         hierarchical c    
      9 c              %          p         hierarchical c    

# prep_label() returns the input when the required cols are missing

    Code
      prep_label(df)
    Message
      i Required columns (`variable_level` and `stat_name`) not present in the input data.
      * Unable to apply `prep_label()`.
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
      i Required columns (`stat_label` and `stat_name`) not present in the input data.
      * Unable to apply `prep_label()`.
    Output
      # A tibble: 3 x 3
        variable_level y     context     
        <chr>          <chr> <chr>       
      1 d              a     categorical 
      2 e              b     continuous  
      3 f              c     hierarchical

# prep_hierarchical_fill() returns input when `length(vars) < 2`

    Code
      prep_hierarchical_fill(df, vars = "y")
    Message
      i At least 2 columns must be supplied to `vars`.
      * Unable to apply `prep_hierarchical_fill()`.
    Output
      # A tibble: 3 x 3
            x y     z    
        <dbl> <chr> <lgl>
      1     1 a     NA   
      2     2 <NA>  NA   
      3    NA b     NA   

# prep_hierarchical_fill() errors when `vars` is not character

    Code
      prep_hierarchical_fill(df, vars = 1:3)
    Condition
      Error in `prep_hierarchical_fill()`:
      ! `vars` must be a character vector. You have supplied an integer vector.

---

    Code
      prep_hierarchical_fill(df, vars = c(TRUE, FALSE))
    Condition
      Error in `prep_hierarchical_fill()`:
      ! `vars` must be a character vector. You have supplied a logical vector.

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
      Error:
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
        NA, "3")), pair = c("y", "z"), fill = 2)
    Condition
      Error:
      ! `fill` must be a character vector of length 1.

