# basic tfrmt - erroring args

    Code
      tfrmt(body_plan = body_plan(frmt_structure(frmt("XX")), ))
    Condition
      Error in `tfrmt()`:
      ! Error in evaluating argument `body_plan`:
       Error in body_plan(frmt_structure(frmt("XX")), ): argument is missing, with no default

# layering tfrmt - error when body_plan groups no longer match group arg

    Code
      basic_tfrmt %>% tfrmt(group = vars(new_group1, new_group2))
    Condition
      Error in `check_group_var_consistency()`:
      ! Inconsistencies between group and body_plan
      Invalid Format Structure in body_plan at position `2`:
        Malformed Group: group1
      
      Invalid Format Structure in body_plan at position `3`:
        Malformed Group: group2
      
      Invalid Format Structure in body_plan at position `4`:
        Malformed Group: group1, group2
      
      i You might need to update group names using "update_group(`new_group1` = `group1`,`new_group2` = `group2`)"

