# tentative_process handles errors with empty message

    Code
      result <- tentative_process("x", empty_msg_func)
    Message
      Unable to to apply empty_msg_func. Reason:
      Unable to to apply empty_msg_func. Reason: .f(...)

# frmt_struct_string handles no group variables

    Code
      apply_tfrmt(dat, tfrmt_no_group, mock = FALSE)
    Message
      Multiple param listed for the same group/label values.
      The following frmt_structures may be missing from the body_plan
      or the order may need to be changed to:
      - `frmt_structure(group_val = ".default", label_val = "n", frmt_combine("{n}, {n_2}",n = frmt("xx"), n_2 = frmt("xx")))`
    Output
      # A tibble: 1 x 2
        lbl   `1`      
      * <chr> <list>   
      1 n     <chr [2]>

# check_order_vars() messages when order variables cause mismatching rows

    Code
      check_order_vars(test_data, tfrmt_plan)
    Message
      Note: Some row labels have values printed over more than 1 line. This could be due to incorrect sorting variables. Each row in your output table should have only one sorting var combination assigned to it.

---

    Code
      check_order_vars(test_data2, tfrmt_plan2)
    Message
      Note: Some row labels have values printed over more than 1 line. This could be due to incorrect sorting variables. Each row in your output table should have only one sorting var combination assigned to it.
