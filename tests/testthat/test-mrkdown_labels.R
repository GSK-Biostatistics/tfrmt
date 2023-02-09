test_that("markdown column labels - no spanning",{

  mock_data <- tibble(rowlbl1 =c(rep("Completion Status",12),rep("Primary reason for withdrawal",28)),
                      rowlbl2 =c(rep("Completed",4),rep("Prematurely Withdrawn",4),rep("Unknown",4),rep("Adverse Event",4),rep("Lost to follow-up",4),rep("Protocol violation",4),rep("Subject decided to withdraw",4),rep("Protocol Violation",4),rep("Pre-Operative Dose[1]",4),rep("Other",4)),
                      param=c(rep(c("n","n","pct","pct"),10)),
                      column=c(rep(c("Placebo<br/>(N=20)","Treatment"),20)),
                      value=c(24,19,2400/48,1900/38,5,1,500/48,100/38,19,18,1900/48,1800/38,1,1,100/48,100/38,0,0,0,0,0,0,0,0,1,1,100/48,100/38,1,4,100/48,400/38,1,0,100/48,0,2,3,200/48,300/38)

  )


  test_tfrmt <- tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = column,
    param = param,
    value = value,
    # set formatting for values
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt_combine(
          "{n} {pct}",
          n = frmt("xxx"),
          pct = frmt_when("==100" ~ "",
                          "==0" ~ "",
                          TRUE ~ frmt("(xx.x %)"))
        )
      )
    ),

    # Specify row group plan
    # Indent the rowlbl2
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "indented")
    )
  ) %>%
    print_to_gt(mock_data) %>%
    tab_options(container.width = 1000)

  # test that format of column headers is markdown
  expect_equal(lapply(test_tfrmt$`_boxhead`$column_label,attr,"class"),
               as.list(rep("from_markdown",length(test_tfrmt$`_boxhead`$column_label)))
  )

})

test_that("markdown column labels - spanning", {

  # set up data for tfrmt
  mock_data <- tibble::tribble(
    ~`Pooled Id`,  ~`Site Id`,
    "701",  "701",
    "703",  "703",
    "704",  "704",
    "705",  "705",
    "708",  "708",
    "709",  "709",
    "710",  "710",
    "713",  "713",
    "716",  "716",
    "718",  "718",
    "900",  "702",
    "900",  "706",
    "900",  "707",
    "900",  "711",
    "900",  "714",
    "900",  "715",
    "900",  "717",
    "Total", " ") %>%
    crossing(col1 = c("Placebo (N=86)",
                      "Xanomeline Low Dose <br /> (N=84)",
                      "Xanomeline High Dose (N=84)",
                      "Total (N=254)"),
             col2 = factor(c("ITT </br> (N=10)", "Eff", "Com"), levels = c("ITT </br> (N=10)", "Eff", "Com"))) %>%
    mutate(val = rpois(216, 15),
           param = "val")

  # create output with spanning headers
  test_tfrmt<-tfrmt(
    param = "param",
    value = "val",
    column = vars(col1, col2),
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("XX"))
    ),
    row_grp_plan = row_grp_plan(label_loc =element_row_grp_loc("column")),
    col_plan = col_plan(
      `Pooled Id`,  `Site Id`,
      contains("Placebo"),
      contains("High Dose"),
      contains("Low Dose"),
      everything()
    )
  ) %>%
    print_to_gt(mock_data)

  # test that format of both column headers and spannning headers is markdown
  expect_equal(lapply(test_tfrmt$`_boxhead`$column_label,attr,"class"),
               as.list(rep("from_markdown",length(test_tfrmt$`_boxhead`$column_label)))
  )
  expect_equal(lapply(test_tfrmt$`_spanners`$spanner_label,attr,"class"),
               as.list(rep("from_markdown",length(test_tfrmt$`_spanners`$spanner_label)))
  )
})


test_that("markdown column labels - renamed", {

  mock_data <- tibble::tribble(
    ~group,     ~label,  ~my_col,    ~parm, ~val,
    "g1", "rowlabel1",  "col1"  ,  "value",    1,
    "g1", "rowlabel1",  "col2"  ,  "value",    1,
    "g1", "rowlabel1",  "mycol3",  "value",    1,
    "g1", "rowlabel1",  "col4"  ,  "value",    1,
    "g1", "rowlabel1",  "mycol5",  "value",    1,
    "g1", "rowlabel2",  "col1"  ,  "value",    2,
    "g1", "rowlabel2",  "col2"  ,  "value",    2,
    "g1", "rowlabel2",  "mycol3",  "value",    2,
    "g1", "rowlabel2",  "col4"  ,  "value",    2,
    "g1", "rowlabel2",  "mycol5",  "value",    2,
    "g2", "rowlabel3",  "col1"  ,  "value",    3,
    "g2", "rowlabel3",  "col2"  ,  "value",    3,
    "g2", "rowlabel3",  "mycol3",  "value",    3,
    "g2", "rowlabel3",  "col4"  ,  "value",    3,
    "g2", "rowlabel3",  "mycol5",  "value",    3)

  test_tfrmt <- tfrmt(
    group = group,
    label = label,
    param = parm,
    value = val,
    column = my_col,
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("x"))
    ),
    col_plan = col_plan(
      group,
      label,
      starts_with("col"),
      "test <br/> newline" = mycol3,
      -mycol5
    )
  ) %>%
    print_to_gt(mock_data)

  expect_equal(lapply(test_tfrmt$`_boxhead`$column_label,attr,"class"),
               as.list(rep("from_markdown",length(test_tfrmt$`_boxhead`$column_label)))
  )
})

test_that("column spanners and labels are appropriately aligned", {
  dat <- tibble::tribble(
    ~ group, ~label, ~span1,   ~span2,  ~lower, ~param,     ~val,
    "mygrp", "mylbl", "span01",  "span1", "lower1_a", "prm", 1,
    "mygrp", "mylbl", "span01",  "span1", "lower1_b", "prm",1,
    "mygrp", "mylbl", "span01",  "span2", "lower2_a", "prm",1,
    "mygrp", "mylbl", "span01",  "span2", "lower2_b", "prm",1,
    "mygrp", "mylbl", "span02",  "span3", "lower2_a", "prm",1,
    "mygrp", "mylbl", "span02",  "span3", "lower2_b", "prm",1
  )

  tfrmt_spec <- tfrmt(
    group = "group",
    label = "label",
    param = "param",
    column =c("span1", "span2","lower"),
    value = "val",
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("x.xx"))
    )
  )

 gt_out <- tfrmt_spec %>%
      print_to_gt(.data = dat)

  # reconstruct the original columns from the gt object

 # get spanner labels
  spans <-  gt_out$`_spanners` %>%
    select(var = vars, spanner_label, spanner_level ) %>%
    unnest(everything())
  # get lower labels
  lower <- gt_out$`_boxhead` %>%
    select(var, column_label) %>%
    unnest(everything())

  # get tfrmt cols from spec
  chr_cols <- map_chr(tfrmt_spec$column, as_name) %>% rev

  # combine spanner & lower labels and rename as per tfrmt spec
  gt_cols <- dplyr::full_join(lower, spans, by = "var", multiple = "all") %>%
    unique %>%
    dplyr::filter(!is.na(spanner_label)) %>%
    tidyr::pivot_wider(names_from = spanner_level,
                values_from = spanner_label) %>%
    dplyr::select(-var) %>%
    setNames(., chr_cols)

  # original data - keep tfrmt spec cols
  orig_cols <- dat %>%
    dplyr::select(all_of(chr_cols)) %>%
    unique

  expect_equal(gt_cols,
               orig_cols,
               ignore_attr = TRUE)

})
