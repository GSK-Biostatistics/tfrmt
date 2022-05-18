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
    values = quo(x)
    )

  sample_df_single_dec_frmted <- apply_frmt.frmt(
    frmt_def = sample_frmt_single_dec,
    .data = sample_df,
    values = quo(x)
    )

  sample_df_double_dec_frmted <- apply_frmt.frmt(
    frmt_def = sample_frmt_double_dec,
    .data = sample_df,
    values = quo(x)
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
    values = sym("x"),
    frmt_def = sample_frmt_10x
  )

  sample_df_frmted_10xx <- apply_frmt.frmt(
    .data = sample_df,
    values = sym("x"),
    frmt_def = sample_frmt_10xx
  )

  sample_df_frmted_ex <- apply_frmt.frmt(
    .data = sample_df,
    values = sym("x"),
    frmt_def = sample_frmt_ex
  )

  sample_df_frmted_exxxx <- apply_frmt.frmt(
    .data = sample_df,
    values = sym("x"),
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
    values = quo(x),
    param = quo(y),
    column = quo(col),
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
    values = quo(x),
    param = quo(y),
    column = quo(col),
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
    values = quo(x),
    param = quo(y),
    column = quo(col),
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
    values = quo(x),
    param = quo(y),
    column = quo(col),
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
      x = c("1234.6 (1.2%) - *  10", "2345.7 (2.3%) - * 111", "3456.8 (3.5%) - *1112", "4567.9 (4.6%) - *  13", "5678.9 (5.7%) - * 114")
    )
  )

})

test_that("appling frmt_when", {
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
    values = quo(x),
    param = "y",
    column = quo(col),
    label = quo(lab),
    group = vars(group)
  )

  man_df <- tribble(
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
    values = quo(x),
    param = quo(y),
    column = quo(col),
    label = quo(lab),
    group = vars(group)
  )

  man_df_combo <- tribble(
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
    values = quo(mock),
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
    values = quo(x),
    param = quo(y),
    column = quo(col),
    label = quo(lab),
    group = vars(group),
    mock = TRUE
  ) %>%
    pull(x)

  expect_equal(sample_df_frmted, rep("xxx.x (X.X%)", 5))





})
