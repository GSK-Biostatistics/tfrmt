test_that("applying frmt", {

  sample_df <- data.frame(
    x = c(1234.5678, 345.6789, 56.7891, 4567.8910, 8.9101)
  )

  sample_frmt_no_dec <- frmt("xxx")
  sample_frmt_single_dec <- frmt("xxx.x")
  sample_frmt_double_dec <- frmt("xxx.xx")

  sample_df_no_dec_frmted <- apply_frmt.frmt(
    frmt_def = sample_frmt_no_dec,
    .data = sample_df,
    value = quo(x)
  )

  sample_df_single_dec_frmted <- apply_frmt.frmt(
    frmt_def = sample_frmt_single_dec,
    .data = sample_df,
    value = quo(x)
  )

  sample_df_double_dec_frmted <- apply_frmt.frmt(
    frmt_def = sample_frmt_double_dec,
    .data = sample_df,
    value = quo(x)
  )

  expect_equal(
    sample_df_no_dec_frmted$x,
    c("1235", "346", " 57", "4568", "  9")
  )

  expect_equal(
    sample_df_single_dec_frmted$x,
    c("1234.6", "345.7", " 56.8", "4567.9", "  8.9")
  )

  expect_equal(
    sample_df_double_dec_frmted$x,
    c("1234.57", "345.68", " 56.79", "4567.89", "  8.91")
  )

})

test_that("applying frmt - scientific", {

  sample_df <- data.frame(
    x = c(1234.5678, 345.6789, 56.7891, 4567.8910, 8.9101, 0.0678)
  )

  sample_frmt_10x <- frmt(expression = "xxx.x", scientific = " x10^x")
  sample_frmt_10xx <- frmt(expression = "xxx.x", scientific = " x10^xx")
  sample_frmt_ex <- frmt(expression = "xxx.x", scientific = "e^x")
  sample_frmt_exxxx <- frmt(expression = "xxx.x", scientific = "e^xxxx")

  sample_df_frmted_10x <- apply_frmt.frmt(
    .data = sample_df,
    value = sym("x"),
    frmt_def = sample_frmt_10x
  )

  sample_df_frmted_10xx <- apply_frmt.frmt(
    .data = sample_df,
    value = sym("x"),
    frmt_def = sample_frmt_10xx
  )

  sample_df_frmted_ex <- apply_frmt.frmt(
    .data = sample_df,
    value = sym("x"),
    frmt_def = sample_frmt_ex
  )

  sample_df_frmted_exxxx <- apply_frmt.frmt(
    .data = sample_df,
    value = sym("x"),
    frmt_def = sample_frmt_exxxx
  )

  expect_equal(
    sample_df_frmted_10x$x,
    c("  1.2 x10^3",
      "  3.5 x10^2",
      "  5.7 x10^1",
      "  4.6 x10^3",
      "  8.9 x10^0",
      "  6.8 x10^-2")
  )

  expect_equal(
    sample_df_frmted_10xx$x,
    c("  1.2 x10^ 3",
      "  3.5 x10^ 2",
      "  5.7 x10^ 1",
      "  4.6 x10^ 3",
      "  8.9 x10^ 0",
      "  6.8 x10^-2")
  )

  expect_equal(
    sample_df_frmted_ex$x,
    c("  1.2e^3",
      "  3.5e^2",
      "  5.7e^1",
      "  4.6e^3",
      "  8.9e^0",
      "  6.8e^-2")
  )
  expect_equal(
    sample_df_frmted_exxxx$x,
    c("  1.2e^   3",
      "  3.5e^   2",
      "  5.7e^   1",
      "  4.6e^   3",
      "  8.9e^   0",
      "  6.8e^  -2")
  )


})

test_that("applying frmt - preserves decimal places after rounding", {


  sample_df <- data.frame(
    x = c(10, 12.3647, 3, 100, 167.299)
  )

  sample_frmt_1dec <- frmt(expression = "xxx.x")
  sample_frmt_2dec <- frmt(expression = "xxx.xx")
  sample_frmt_10x <- frmt(expression = "xxx.x", scientific = " x10^x")

  sample_df_frmted_1dec <- apply_frmt.frmt(
    .data = sample_df,
    value = sym("x"),
    frmt_def = sample_frmt_1dec
  )

  sample_df_frmted_2dec <- apply_frmt.frmt(
    .data = sample_df,
    value = sym("x"),
    frmt_def = sample_frmt_2dec
  )

  sample_df_frmted_10x <- apply_frmt.frmt(
    .data = sample_df,
    value = sym("x"),
    frmt_def = sample_frmt_10x
  )


  expect_equal(
    sample_df_frmted_1dec$x,
    c(" 10.0",
      " 12.4",
      "  3.0",
      "100.0",
      "167.3")
  )

  expect_equal(
    sample_df_frmted_2dec$x,
    c(" 10.00",
      " 12.36",
      "  3.00",
      "100.00",
      "167.30")
  )

  expect_equal(
    sample_df_frmted_10x$x,
    c("  1.0 x10^1",
      "  1.2 x10^1",
      "  3.0 x10^0",
      "  1.0 x10^2",
      "  1.7 x10^2")
  )


})

test_that("applying frmt_combine - 2x", {

  sample_df <- tibble(
    group = "group",
    lab = rep(paste("lab",1:5),2),
    col = "col",
    y = rep(c("A","B"),each = 5),
    x = c(1234.5678, 2345.6789, 3456.7891, 4567.8910, 5678.9101,
          1.2345678, 2.3456789, 3.4567891, 4.5678910, 5.6789101)
  )

  sample_frmt <- frmt_combine(
    "{A} {B}",
    A = frmt("xxx.x"),
    B = frmt("(X.X%)")
  )

  sample_df_frmted <- apply_frmt.frmt_combine(
    frmt_def = sample_frmt,
    .data = sample_df,
    value = quo(x),
    param = quo(y),
    column = vars(col),
    label = quo(lab),
    group = vars(group)
  )

  expect_equal(
    sample_df_frmted,
    tibble(
      group = "group",
      lab = paste("lab",1:5),
      col = "col",
      y = "A",
      x = c("1234.6 (1.2%)", "2345.7 (2.3%)", "3456.8 (3.5%)", "4567.9 (4.6%)", "5678.9 (5.7%)")
    )
  )

})

test_that("applying frmt_combine missing",{
  #Both missing
  sample_df <- tibble(
    group = "group",
    lab = rep(paste("lab",1:5),2),
    col = "col",
    y = rep(c("A","B"),each = 5),
    x = c(1234.5678, 2345.6789, 3456.7891, 4567.8910, NA,
          1.2345678, 2.3456789, 3.4567891, 4.5678910, NA)
  )

  sample_frmt <- frmt_combine(
    "{A} {B}",
    A = frmt("xxx.x"),
    B = frmt("(X.X%)"),
    missing = "Missing"
  )

  sample_df_frmted <- apply_frmt.frmt_combine(
    frmt_def = sample_frmt,
    .data = sample_df,
    value = quo(x),
    param = quo(y),
    column = vars(col),
    label = quo(lab),
    group = vars(group)
  )

  expect_equal(
    sample_df_frmted,
    tibble(
      group = "group",
      lab = paste("lab",1:5),
      col = "col",
      y = "A",
      x = c("1234.6 (1.2%)", "2345.7 (2.3%)", "3456.8 (3.5%)", "4567.9 (4.6%)", "Missing")
    )
  )
  #One Missing
  sample_df <- tibble(
    group = "group",
    lab = rep(paste("lab",1:5),2),
    col = "col",
    y = rep(c("A","B"),each = 5),
    x = c(1234.5678, 2345.6789, 3456.7891, 4567.8910, NA,
          1.2345678, 2.3456789, 3.4567891, 4.5678910, 5.6789101)
  )

  sample_frmt <- frmt_combine(
    "{A} {B}",
    A = frmt("xxx.x"),
    B = frmt("(X.X%)"),
    missing = "Missing"
  )

  sample_df_frmted <- apply_frmt.frmt_combine(
    frmt_def = sample_frmt,
    .data = sample_df,
    value = quo(x),
    param = quo(y),
    column = vars(col),
    label = quo(lab),
    group = vars(group)
  )

  expect_equal(
    sample_df_frmted,
    tibble(
      group = "group",
      lab = paste("lab",1:5),
      col = "col",
      y = "A",
      x = c("1234.6 (1.2%)", "2345.7 (2.3%)", "3456.8 (3.5%)", "4567.9 (4.6%)", "NA (5.7%)")
    )
  )

})

test_that("applying frmt_combine - 3x", {

  sample_df <- tibble(
    group = "group",
    lab = rep(paste("lab",1:5),3),
    col = "col",
    y = rep(c("A","B","C"),each = 5),
    x = c(1234.5678, 2345.6789, 3456.7891, 4567.8910, 5678.9101,
          1.2345678, 2.3456789, 3.4567891, 4.5678910, 5.6789101,
          10,111,1112,13,114)
  )

  sample_frmt <- frmt_combine(
    "{A} {B} - {C}",
    A = frmt("xxx.x"),
    B = frmt("(X.X%)"),
    C = frmt("*XXXX.X")
  )

  sample_df_frmted <- apply_frmt.frmt_combine(
    frmt_def = sample_frmt,
    .data = sample_df,
    value = quo(x),
    param = quo(y),
    column = vars(col),
    label = quo(lab),
    group = vars(group)
  )

  expect_equal(
    sample_df_frmted,
    tibble(
      group = "group",
      lab = paste("lab",1:5),
      col = "col",
      y = "A",
      x = c("1234.6 (1.2%) - *  10.0", "2345.7 (2.3%) - * 111.0", "3456.8 (3.5%) - *1112.0", "4567.9 (4.6%) - *  13.0", "5678.9 (5.7%) - * 114.0")
    )
  )

})

test_that("applying frmt_combine - no unique labels, so unable to frmt_combine", {

  sample_df <- tibble(
    group = "group",
    lab = paste("lab",1:15),
    col = "col",
    y = rep(c("A","B","C"),each = 5),
    x = c(1234.5678, 2345.6789, 3456.7891, 4567.8910, 5678.9101,
          1.2345678, 2.3456789, 3.4567891, 4.5678910, 5.6789101,
          10,111,1112,13,114)
  )

  sample_frmt <- frmt_combine(
    "{A} {B} - {C}",
    A = frmt("xxx.x"),
    B = frmt("(X.X%)"),
    C = frmt("*XXXX.X")
  )

  expect_warning(
    sample_df_frmted <- apply_frmt.frmt_combine(
      frmt_def = sample_frmt,
      .data = sample_df,
      value = quo(x),
      param = quo(y),
      column = vars(col),
      label = quo(lab),
      group = vars(group)
    ),
    "Unable to apply `frmt_combine` due to uniqueness of column/row identifiers. Params that are to be combined need to have matching values across: "
  )

  expect_equal(
    sample_df_frmted,
    tibble(
      group = "group",
      lab = c("lab 1", "lab 10",
              "lab 11", "lab 12", "lab 13", "lab 14", "lab 15", "lab 2", "lab 3",
              "lab 4", "lab 5", "lab 6", "lab 7", "lab 8", "lab 9"),
      col = "col",
      y = c("A", "B", "C", "C",
           "C", "C", "C", "A", "A", "A", "A", "B", "B", "B", "B"),
      x = c("1234.6 NA - NA", "NA (5.7%) - NA", "NA NA - *  10.0", "NA NA - * 111.0",
            "NA NA - *1112.0", "NA NA - *  13.0", "NA NA - * 114.0", "2345.7 NA - NA",
            "3456.8 NA - NA", "4567.9 NA - NA", "5678.9 NA - NA", "NA (1.2%) - NA",
            "NA (2.3%) - NA", "NA (3.5%) - NA", "NA (4.6%) - NA")
    )
  )
})


test_that("applying frmt_when", {
  #Test frmt_when alone
  sample_df <- tibble(
    group = "group",
    lab = rep(paste("lab",1:5), 2),
    col = "col",
    y = rep(c("A","B"),each = 5),
    x = c(1234.5678, 2345.6789, 3456.7891, 4567.8910, 5678.9101,
          1.2345678, 2.3456789, 3.4567891, 4.5678910, 5.6789101)
  )

  sample_frmt <- frmt_when(
    ">1000" ~ frmt("XXX.X"),
    "TRUE" ~ "Undectable"
  )

  sample_df_frmted <- apply_frmt(
    frmt_def = sample_frmt,
    .data = sample_df,
    value = quo(x),
    param = "y",
    column = vars(col),
    label = quo(lab),
    group = vars(group)
  )

  man_df <- tibble::tribble(
    ~group, ~lab,        ~col, ~y,    ~x,
    "group", "lab 1", "col",   "A",     "1234.6",
    "group", "lab 2", "col",   "A",     "2345.7",
    "group", "lab 3", "col",   "A",     "3456.8",
    "group", "lab 4", "col",   "A",     "4567.9",
    "group", "lab 5", "col",   "A",     "5678.9",
    "group", "lab 1", "col",   "B",     "Undectable",
    "group", "lab 2", "col",   "B",     "Undectable",
    "group", "lab 3", "col",   "B",     "Undectable",
    "group", "lab 4", "col",   "B",     "Undectable",
    "group", "lab 5", "col",   "B",     "Undectable",
  )

  expect_equal(sample_df_frmted, man_df)


  #Test in combination
  sample_frmt_combo <- frmt_combine(
    "{A} {B}",
    A = frmt("xxx.x"),
    B = frmt_when(">3" ~ frmt("(X.X%)"),
                  "<=3" ~ frmt("Undetectable")
    )
  )

  sample_df_frmted <- apply_frmt.frmt_combine(
    frmt_def = sample_frmt_combo,
    .data = sample_df,
    value = quo(x),
    param = quo(y),
    column = vars(col),
    label = quo(lab),
    group = vars(group)
  )

  man_df_combo <- tibble::tribble(
    ~group, ~lab,   ~col,   ~y,     ~x,
    "group", "lab 1", "col",  "A",     "1234.6 Undetectable",
    "group", "lab 2", "col",  "A",     "2345.7 Undetectable",
    "group", "lab 3", "col",  "A",     "3456.8 (3.5%)",
    "group", "lab 4", "col",  "A",     "4567.9 (4.6%)",
    "group", "lab 5", "col",  "A",     "5678.9 (5.7%)",
  )
  expect_equal(sample_df_frmted, man_df_combo)


})


test_that("mocks return correctly", {

  #frmt
  frmt_mock <- apply_frmt.frmt(
    frmt_def = frmt("xxx.x"),
    .data = iris,
    value = quo(mock),
    mock = TRUE
  )%>%
    pull(mock)
  expect_equal(frmt_mock, rep("xxx.x", nrow(iris)))

  # frmt_when
  frmt_when_true <- apply_frmt.frmt_when(frmt_when("==100"~ frmt(""),
                                                   "==0"~ "",
                                                   "TRUE" ~ frmt("(XXX.X%)")),
                                         .data = iris, sym("value"),mock = TRUE) %>%
    pull(value)
  expect_equal(frmt_when_true, rep("(XXX.X%)", nrow(iris)))


  frmt_when_no_true <-apply_frmt.frmt_when(frmt_when("==100"~ frmt("Hello"),
                                                     "==0"~ ""),
                                           .data = iris, sym("value"),mock = TRUE) %>%
    pull(value)
  expect_equal(frmt_when_no_true, rep("Hello", nrow(iris)))

  #frmt_combine
  sample_df <- tibble(
    group = "group",
    lab = rep(paste("lab",1:5),2),
    col = "col",
    y = rep(c("A","B"),each = 5)
  )

  sample_frmt <- frmt_combine(
    "{A} {B}",
    A = frmt("xxx.x"),
    B = frmt("(X.X%)"),
    missing = "Missing"
  )

  sample_df_frmted <- apply_frmt.frmt_combine(
    frmt_def = sample_frmt,
    .data = sample_df,
    value = quo(x),
    param = quo(y),
    column = vars(col),
    label = quo(lab),
    group = vars(group),
    mock = TRUE
  ) %>%
    pull(x)

  expect_equal(sample_df_frmted, rep("xxx.x (X.X%)", 5))

})



test_that("Space in Param", {
  no_ten <- frmt_combine("{LM mean} ({LM stderr})",
                         `LM mean` = frmt("xx.x"),
                         `LM stderr` = frmt("xx.xx")
  )


  expect_equal(no_ten$expression, "{`LM mean`} ({`LM stderr`})")

  mixed <- frmt_combine("{mean} ({CV %})",
                        mean = frmt("xx.x"),
                        `CV %` = frmt("xx.xx")
  )

  expect_equal(mixed$expression, "{mean} ({`CV %`})")


  data <- tibble::tribble(
    ~group,	~type,	~label,	~column,	~param,	~value,
    "baseline",	"description",	"Week 12 analysis",	"Placebo",	"LM mean",	79.0,
    "baseline",	"description",	"Week 12 analysis",	"Placebo",	"LM stderr",	5.0,
    "Primary analysis",	"trt comparison",	"Week 12 analysis",	"TRT - PBO",	"LM mean",	-0.3,
    "Primary analysis",	"trt comparison",	"Week 12 analysis",	"TRT - PBO",	"LM stderr",	0.4
  )


  space_combo <- frmt_combine("{`LM mean`} ({`LM stderr`})",
                              `LM mean` = frmt("xx.x"),
                              `LM stderr` = frmt("xx.xx")

  )

  expect_equal(space_combo$expression, "{`LM mean`} ({`LM stderr`})")

  sample_df_frmted <- apply_frmt.frmt_combine(
    frmt_def = space_combo,
    .data = data,
    value = quo(value),
    param = quo(param),
    column = vars(column),
    label = quo(label),
    group = vars(group, type),
    mock = FALSE
  ) %>%
    pull(value)


  expect_equal(sample_df_frmted, c("79.0 ( 5.00)", "-0.3 ( 0.40)"))


})


test_that("frmt_combine only applies when all parameters are in the data", {
  data <- tibble(Group = rep(c("Age (y)", "Sex", "Age (y)", "Sex"), c(3, 3, 6,12)),
                 Label = rep(c("n", "Mean (SD)", "Male","Female"), c(6, 6,6,6)),
                 Column = rep(c("Placebo", "Treatment", "Total"), times = 8),
                 Param = rep(c("n", "mean", "sd", "n", "pct", "n", "pct"),  c(6, 3, 3, 3,3,3,3)),
                 Value = c(15,13,28,14,13,27,73.56, 74.231,71.84,9.347,7.234,8.293,8,7,15,8/14,7/13,15/27,6,6,12,6/14,6/13,12/27
                 )
  ) %>%
    # Note because tfrmt only does rounding we will need to have the percents multiplied by 100
    mutate(Value = case_when(Param == "pct" ~ Value * 100,
                             TRUE ~ Value),
           ord1 = if_else(Group == "Age (y)", 1, 2),
           ord2 = if_else(Label == "n", 1, 2),
           TEMP_row = row_number())


  test_combo <- frmt_structure(group_val = ".default", label_val = ".default",
                               frmt_combine("{n} ({pct}%)",
                                            n = frmt("XX"),
                                            pct = frmt("x.x"))
  )

  rows_to_use <- fmt_test_data(test_combo, data, group= vars(Group),
                label = quo(Label), param = quo(Param) )
  expected <- data %>%
    filter(Label %in% c("Male","Female")) %>%
    pull(TEMP_row)

  expect_equal(rows_to_use, expected)
})

test_that("frmt_combine fills with partially missing values where a column is missing the value", {


  data <- tibble(
      Group = rep(c("Age (y)"), c(6)),
      Label = rep(c("Mean (SD)"), c(6)),
      Column = rep(c("Placebo", "Treatment", "Total"), each = c(2)),
      Param = rep(c("mean", "sd"),  times = c(3)),
      Value = c(1, 2, 3, 4, 5, 6)
    ) %>%
    .[-1,] # remove first row - where a "mean" is, but is otherwise complete


  test_combo <- frmt_combine(
      "{mean} {sd}",
      mean = frmt("XX", missing = " -"),
      sd = frmt("(xx)")
    )

  sample_df_frmted <- apply_frmt.frmt_combine(
    frmt_def = test_combo,
    .data = data,
    value = quo(Value),
    param = quo(Param),
    column = vars(Column),
    label = quo(Label),
    group = vars(Group),
    mock = FALSE
  )

  expect_equal(sample_df_frmted,
               tibble(
                 Group = rep(c("Age (y)"), c(3)),
                 Label = rep(c("Mean (SD)"), c(3)),
                 Column = c("Placebo", "Total", "Treatment"),
                 Param = c("sd","mean","mean"),
                 Value = c(" - ( 2)", " 5 ( 6)", " 3 ( 4)")
               ))
  # Test the NA still comes through when missing isn't provided
  test_combo_na <- frmt_combine(
    "{mean} {sd}",
    mean = frmt("XX"),
    sd = frmt("(xx)")
  )

  sample_df_frmted <- apply_frmt.frmt_combine(
    frmt_def = test_combo_na,
    .data = data,
    value = quo(Value),
    param = quo(Param),
    column = vars(Column),
    label = quo(Label),
    group = vars(Group),
    mock = FALSE
  )

  expect_equal(sample_df_frmted,
               tibble(
                 Group = rep(c("Age (y)"), c(3)),
                 Label = rep(c("Mean (SD)"), c(3)),
                 Column = c("Placebo", "Total", "Treatment"),
                 Param = c("sd","mean","mean"),
                 Value = c("NA ( 2)", " 5 ( 6)", " 3 ( 4)")
               ))


})

