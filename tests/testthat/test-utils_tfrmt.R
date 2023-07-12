
set.seed(1234)

raw_data_cat <- crossing(group = "A",label = c("w", "x", "y", "z"),
                         col = paste("Var", 1:4), param2 = c("count", "pct")) %>%
  rowwise() %>%
  mutate(ord1 = 1,
         ord2 = 26-which(label==letters),
         val2 = case_when(
           label == "w" & param2 == "pct" ~100.0,
           param2=="count" ~ as.double(rpois(n=1, lambda = 150)),
           param2 == "pct" ~runif(n=1,max = 100)
         )
  )

raw_data_cont <- crossing(group = "B",label = c("w", "i", "j", "k"),
                          col = paste("Var", 1:4), param2 = c("val")) %>%
  rowwise() %>%
  mutate(ord1 = 2,
         ord2 = which(label==letters),
         val2 = case_when(
           label == "w" ~ as.double(rpois(n=1, lambda = 150)),
           label == "i" ~ rnorm(n=1, mean = 75, 13),
           label == "j" ~ rnorm(n=1, mean = 10, 3),
           label == "k" ~ rnorm(n=1, mean = 72, 7)
         )
  )
raw_dat <- bind_rows(raw_data_cat, raw_data_cont)

plan  <- tfrmt(
  #These are the columns that control the general structure of the data
  group = vars(group),
  label = "label",
  param = "param2",
  value = "val2",
  column = "col",
  #This controls how the rows are sorted
  sorting_cols = vars(ord1, ord2),
  col_style_plan = col_style_plan(
    col_style_structure(align = c(" ", ",", "."), col= vars(starts_with("Var")))),
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt("XXX.XX")),
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine(
                     "{count} {pct}",
                     count = frmt("XXX"),
                     percent = frmt_when("==100"~ frmt(""),
                                         "==0"~ "",
                                         "TRUE" ~ frmt("(XXX.X%)"))
                   )),
    frmt_structure(group_val = c("B"), label_val = "w", # Value(s) in the label column where you would want to apply this fmt
                   frmt("XXX")),
    frmt_structure(group_val = "B", label_val = c("i", "k"), frmt("xx.x")),
    frmt_structure(group_val = "B", label_val = "j", frmt("xx.xx"))
  ),
  # These are the variables to keep
  col_plan= col_plan(everything(), -starts_with("ord")),
  row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc("spanning"))
)

test_that("Check apply_tfrmt", {


  man_df <-  tibble::tribble(
    ~group, ~label, ~`Var 1`,        ~`Var 2`,        ~`Var 3`,        ~`Var 4`,
    "A",     "w",     "135         ", "141         ", "143         ", "137         ",
    "A",     "x",     "129 ( 76.0%)", "139 ( 31.2%)", "153 ( 24.4%)", "158 ( 15.3%)",
    "A",     "y",     "150 (  4.2%)", "144 ( 56.5%)", "165 ( 66.8%)", "167 ( 89.9%)",
    "A",     "z",     "146 ( 13.2%)", "134 ( 56.5%)", "142 (  3.9%)", "156 ( 94.6%)",
    "B",     "i",     " 83.5       ", " 68.9       ", " 78.2       ", " 79.2       ",
    "B",     "j",     " 10.77      ", " 11.05      ", "  8.79      ", "  5.70      ",
    "B",     "k",     " 80.3       ", " 72.5       ", " 87.3       ", " 71.6       ",
    "B",     "w",     "147         ", "149         ", "143         ", "159         "
  ) %>%
    mutate(..tfrmt_row_grp_lbl = FALSE)

  expect_equal(
    apply_tfrmt(raw_dat, plan) %>% ungroup() %>% arrange(group, label) ,
    man_df %>% arrange(group, label),
    ignore_attr = c("class",".col_plan_vars",".footnote_locs")
  )

  plan$sorting_cols <- NULL

  man_df_ord <- man_df %>%
    arrange(group, label)

  expect_equal(apply_tfrmt(raw_dat, plan) %>% ungroup(),
               man_df_ord,
               ignore_attr = c("class",".col_plan_vars",".footnote_locs"))

  expect_error(
    apply_tfrmt(raw_dat, tfrmt(
      label = "label2")),
    "Variable Specified in 'label' doesn't exist in the supplied dataset. Please check the tfrmt and try again."
  )

  expect_error(
    apply_tfrmt(raw_dat, tfrmt(
      group = "label2")),
    "Variable Specified in 'group' doesn't exist in the supplied dataset. Please check the tfrmt and try again."
  )

})

test_that("Check apply_tfrmt for mock data",{

  # mock for example data above

  mock_dat <- raw_dat %>% select(-val2)

  mock_man_df <-  tibble::tribble(
    ~group, ~label, ~`Var 1`,        ~`Var 2`,        ~`Var 3`,        ~`Var 4`,
    "A",     "z",     "XXX (XXX.X%)", "XXX (XXX.X%)" ,"XXX (XXX.X%)" ,"XXX (XXX.X%)",
    "A",     "y",     "XXX (XXX.X%)", "XXX (XXX.X%)" ,"XXX (XXX.X%)" ,"XXX (XXX.X%)",
    "A",     "x",     "XXX (XXX.X%)", "XXX (XXX.X%)" ,"XXX (XXX.X%)" ,"XXX (XXX.X%)",
    "A",     "w",     "XXX (XXX.X%)", "XXX (XXX.X%)" ,"XXX (XXX.X%)" ,"XXX (XXX.X%)",
    "B",     "i",     " xx.x       ", " xx.x       " ," xx.x       " ," xx.x       ",
    "B",     "j",     " xx.xx      ", " xx.xx      " ," xx.xx      " ," xx.xx      ",
    "B",     "k",     " xx.x       ", " xx.x       " ," xx.x       " ," xx.x       ",
    "B",     "w",     "XXX         ", "XXX         " ,"XXX         " ,"XXX         ",
  ) %>%
    mutate("..tfrmt_row_grp_lbl" = FALSE) %>%
    arrange(group, label)

  expect_equal(apply_tfrmt(mock_dat, plan, mock = TRUE) %>% ungroup() %>% arrange(group, label) ,
               mock_man_df,
               ignore_attr = c("class",".col_plan_vars",".footnote_locs"))


  # mock for plan alone
  plan  <- tfrmt(
    #These are the columns that control the general structure of the data
    group = vars(group),
    label = "label",
    param = "param2",
    value = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default",
                     frmt_combine(
                       "{count} {pct}",
                       count = frmt("XXX"),
                       percent = frmt_when("==100"~ frmt(""),
                                           "==0"~ "",
                                           "TRUE" ~ frmt("(XXX.X%)"))
                     )),
      frmt_structure(group_val = c("B"), label_val = "w", # Value(s) in the label column where you would want to apply this fmt
                     frmt("XXX")),
      frmt_structure(group_val = "B", label_val = c("i", "k"), frmt("xx.x")),
      frmt_structure(group_val = "B", label_val = "j", frmt("xx.xx"))
    ),
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc("spanning")),
    # These are the variables to keep
    col_plan = col_plan(everything(), -starts_with("ord"))
  )


  mock_dat <- make_mock_data(plan, .default = 1:2, n_cols = 4)
  mock_man_df <-  tibble::tribble(
    ~group,  ~label,   ~ col1,         ~col2,        ~ col3,        ~ col4,
    "group_1", "label_1", "XXX (XXX.X%)", "XXX (XXX.X%)" ,"XXX (XXX.X%)" ,"XXX (XXX.X%)",
    "group_2", "label_1", "XXX (XXX.X%)", "XXX (XXX.X%)" ,"XXX (XXX.X%)" ,"XXX (XXX.X%)",
    "group_1", "label_2", "XXX (XXX.X%)", "XXX (XXX.X%)" ,"XXX (XXX.X%)" ,"XXX (XXX.X%)",
    "group_2", "label_2", "XXX (XXX.X%)", "XXX (XXX.X%)" ,"XXX (XXX.X%)" ,"XXX (XXX.X%)",
    "B"      , "w",     "XXX"          , "XXX"          ,"XXX"          ,"XXX",
    "B"      , "i",     "xx.x"         , "xx.x"         ,"xx.x"         ,"xx.x",
    "B"      , "k",     "xx.x"         , "xx.x"         ,"xx.x"         ,"xx.x",
    "B"      , "j",     "xx.xx"        , "xx.xx"        ,"xx.xx"        ,"xx.xx"
  ) %>%
    mutate("..tfrmt_row_grp_lbl" = FALSE) %>%
    arrange(group, label)

  expect_equal(
    apply_tfrmt(mock_dat, plan, mock = TRUE) %>% ungroup() %>% arrange(group, label) ,
    mock_man_df,
    ignore_attr = c("class",".col_plan_vars",".footnote_locs"))



  # plan with multiple group variables
  plan  <- tfrmt(
    group = vars(grp1, grp2, grp3, grp4),
    label = "my_label",
    param = "param2",
    value = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = list(grp1 = "A", grp2 = c("a","b"), grp3 = ".default", grp4 = ".default"), label_val = ".default", frmt("xx.x")),
      frmt_structure(group_val = list(grp1 = "B", grp2 = c("a","b"), grp3 = ".default", grp4 = ".default"), label_val = ".default", frmt("xx.x")),
      frmt_structure(group_val = list(grp1 = ".default", grp2 = ".default", grp3 = "C", grp4 = c("a","b")), label_val = ".default", frmt("xx.x")),
      frmt_structure(group_val = list(grp1 = ".default", grp2 = ".default", grp3 = "D", grp4 = c("a","b")), label_val = ".default", frmt("xx.x"))
    ),
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc("gtdefault"))
  )
  mock_dat <- make_mock_data(plan, .default = 1, n_col = 1) %>%
    apply_tfrmt(plan, mock =TRUE)

  expected_dat <-  tibble::tribble(
    ~grp1,   ~grp2,     ~grp3,     ~grp4,   ~my_label,   ~col1,
    "A"      ,"a"      ,"grp3_1" ,"grp4_1" ,"my_label_1" ,"xx.x" ,
    "A"      ,"b"      ,"grp3_1" ,"grp4_1" ,"my_label_1" ,"xx.x" ,
    "B"      ,"a"      ,"grp3_1" ,"grp4_1" ,"my_label_1" ,"xx.x" ,
    "B"      ,"b"      ,"grp3_1" ,"grp4_1" ,"my_label_1" ,"xx.x" ,
    "grp1_1" ,"grp2_1" ,"C"      ,"a"      ,"my_label_1" ,"xx.x" ,
    "grp1_1" ,"grp2_1" ,"C"      ,"b"      ,"my_label_1" ,"xx.x" ,
    "grp1_1" ,"grp2_1" ,"D"      ,"a"      ,"my_label_1" ,"xx.x" ,
    "grp1_1" ,"grp2_1" ,"D"      ,"b"      ,"my_label_1" ,"xx.x"
  )

  expect_equal(
    mock_dat,
    expected_dat,
    ignore_attr = c("class",".col_plan_vars",".footnote_locs")
  )

  # duplicate params for a single group/label combo
  plan  <- tfrmt(
    group = "grp1",
    label = "my_label",
    param = "param2",
    value = "val2",
    column = "col",
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", N = frmt("xxx")),
      frmt_structure(group_val = ".default", label_val = ".default", mean = frmt("xx.x"))
    ),
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc("gtdefault"))
  )
  mock_dat <- make_mock_data(plan, .default = 1:2, n_col = 2)

  make_mock_dat_message <- mock_dat %>%
    apply_tfrmt(plan, mock =TRUE) %>%
    capture_messages()

  ## capturing second message
  expect_equal(
    make_mock_dat_message,
    "Mock data contains more than 1 param per unique label value. Param values will appear in separate rows.\n"
  )

  test_dat <- mock_dat %>%
    quietly(apply_tfrmt)(plan, mock =TRUE) %>%
    .[["result"]]

  expect_equal(test_dat,
               tibble::tribble(
                 ~grp1,   ~my_label,   ~col1,  ~col2,
                 "grp1_1", "my_label_1", "xxx" ,  "xxx"  ,
                 "grp1_1", "my_label_1", "xx.x",  "xx.x" ,
                 "grp1_1", "my_label_2", "xxx" ,  "xxx"  ,
                 "grp1_1", "my_label_2", "xx.x",  "xx.x" ,
                 "grp1_2", "my_label_1", "xxx" ,  "xxx"  ,
                 "grp1_2", "my_label_1", "xx.x",  "xx.x" ,
                 "grp1_2", "my_label_2", "xxx" ,  "xxx"  ,
                 "grp1_2", "my_label_2", "xx.x",  "xx.x" ,
               ),
               ignore_attr = c("class",".col_plan_vars",".footnote_locs"))

})

test_that("Test body_plan missing", {
  input_data <- tibble(
    group = "groupvar",
    label = paste0("label", 1:10),
    param = "params",
    column = "col1",
    val = 1:10
  )

  expect_message(
    empty_body_plan <- tfrmt(
      group = group,
      label = label,
      param = param,
      column = column,
      value = val,
      row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc("gtdefault"))
    ) %>%
      apply_tfrmt(input_data, .),
    "The following rows of the given dataset have no format applied to them 1, 2, 3, 4, 5, 6, 7, 8, 9, 10"
  )

  expect_equal(empty_body_plan,
               input_data %>%
                 select(-param) %>%
                 mutate(val = as.character(val)) %>%
                 pivot_wider(names_from = column, values_from = val),
               ignore_attr = c("class",".col_plan_vars",".footnote_locs"))
})


test_that("incomplete body_plan where params share label",{

  dd <- tibble::tribble(
    ~rowlbl1, ~grp, ~rowlbl2, ~column, ~param, ~value,
    "topgrp", "lowergrp1", "n pct", "A",  "n",   1,
    "topgrp", "lowergrp1", "n pct", "A",  "pct",   50,
    "topgrp", "lowergrp1", "mean", "A",  "mean",   2,
    "topgrp", "lowergrp2", "n pct", "A",  "n",   2,
    "topgrp", "lowergrp2", "n pct", "A",  "pct",   40,
    "topgrp", "lowergrp2", "mean", "A",  "mean",   5
  )

  tfrmt_spec <- tfrmt(
    group = c(rowlbl1,grp),
    label = rowlbl2,
    column = column,
    param = param,
    value = value,
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = "mean", frmt("xx.x"))
    ),
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(location = "column")
    )
  )


    expect_message(
      auto_tfrmt <- apply_tfrmt(dd, tfrmt_spec),
      "The following rows of the given dataset have no format applied to them 1, 2, 4, 5"
    ) %>%
      expect_message(
        "Multiple param listed for the same group/label values"
      )

  man_tfrmt <- tibble::tribble(
    ~rowlbl1,  ~rowlbl2,    ~ A   ,       ~..tfrmt_row_grp_lbl,
    "topgrp", "lowergrp1", NA_character_, TRUE,
    "topgrp", "  n pct"  ,c("1","50"),   FALSE,
    "topgrp", "  mean"   , " 2.0",        FALSE,
    "topgrp", "lowergrp2", NA_character_, TRUE,
    "topgrp", "  n pct"  , c("2","40"),   FALSE,
    "topgrp", "  mean"   , " 5.0",        FALSE
  ) %>% group_by(rowlbl1)

  expect_equal(auto_tfrmt, man_tfrmt,
               ignore_attr = c("class",".col_plan_vars",".footnote_locs"))
})



test_that("incorrect footnote plan formats",{
  expect_error( tfrmt(
    # specify columns in the data
    group = c(rowlbl0,rowlbl1),
    label = rowlbl2,
    column = trt,
    param = param,
    value = value,
    # set formatting for values
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),

    # Specify row group plan
    # Indent the rowlbl2
    # row_grp_plan = row_grp_plan(
    #   row_grp_structure(group_val = ".default", element_block(post_space = " ")),
    #   label_loc = element_row_grp_loc(location = "indented")),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote",group_val="Test group"),
      marks="letters"
    )
  ),
  "when tfrmt contains multiple groups, group_val must be a named list")


  expect_error(tfrmt(
    # specify columns in the data
    group = c(rowlbl1),
    label = rowlbl2,
    column = c(col2,trt),
    param = param,
    value = value,
    # set formatting for values
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                  n = frmt("xxx"),
                                                                                  pct = frmt_when("==100" ~ "",
                                                                                                  "==0" ~ "",
                                                                                                  TRUE ~ frmt("(xx.x %)"))))),

    # Specify row group plan
    # Indent the rowlbl2
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "column")),
    footnote_plan = footnote_plan(
      footnote_structure("Test footnote 2",column_val="Treatment column"),
      marks="letters"

    )
  ),
  "when tfrmt contains multiple columns, column_val must be a named list")

})
