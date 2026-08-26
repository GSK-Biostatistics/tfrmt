# row_grp_plan

    Code
      test_grp_plan
    Output
      $struct_list
      $struct_list[[1]]
      $group_val
      [1] "A" "C"
      
      $block_to_apply
      $post_space
      [1] "---"
      
      $fill
      [1] TRUE
      
      attr(,"class")
      [1] "element_block" "element"      
      
      attr(,"class")
      [1] "row_grp_structure" "frmt_table"       
      
      $struct_list[[2]]
      $group_val
      [1] "B"
      
      $block_to_apply
      $post_space
      [1] " "
      
      $fill
      [1] TRUE
      
      attr(,"class")
      [1] "element_block" "element"      
      
      attr(,"class")
      [1] "row_grp_structure" "frmt_table"       
      
      
      $label_loc
      $location
      [1] "column"
      
      $indent
      [1] "  "
      
      attr(,"class")
      [1] "element_row_grp_loc" "element"            
      
      attr(,"class")
      [1] "row_grp_plan" "frmt_table"  

# row_grp_structure

    Code
      test_grp_structure
    Output
      $group_val
      $group_val$grp1
      [1] "A"
      
      $group_val$grp2
      [1] "b"
      
      
      $block_to_apply
      $post_space
      [1] " "
      
      $fill
      [1] TRUE
      
      attr(,"class")
      [1] "element_block" "element"      
      
      attr(,"class")
      [1] "row_grp_structure" "frmt_table"       

# row_grp_structure with unnamed list

    Code
      row_grp_structure(group_val = list(grp1 = "A", "b"), element_block = element_block(
        post_space = " "))
    Condition
      Error in `row_grp_structure()`:
      ! When `group_val` is a list, it must be a named list.

