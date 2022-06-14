### generate example demography ARD from tplyr

library(tidyverse)
library(Tplyr)

pct_bool <- function(x, ...){
  mean(x, ...) * 100
}

# copied from https://github.com/atorus-research/Tplyr/blob/master/vignettes/adsl.Rdata
load("dev/data/adsl.Rdata")

demog_ARD <- adsl %>%
  mutate(
    SEX_numeric = as.numeric(factor(
      SEX,
      levels = c(
        "M",
        "F"
      )
    )),
    RACE_numeric = as.numeric(factor(
      RACE,
      levels = c(
        "AMERICAN INDIAN OR ALASKA NATIVE",
        "ASIAN",
        "BLACK OR AFRICAN AMERICAN",
        "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
        "WHITE",
        "MULTIPLE"
      )
    )),
    ETHNIC_numeric = as.numeric(factor(
      ETHNIC,
      levels = c(
        "HISPANIC OR LATINO",
        "NOT HISPANIC OR LATINO"
      )
    ))
  ) %>%
  tplyr_table(TRT01P) %>%
  add_layer(
    group_desc(SEX_numeric, by = "Sex n (%)") %>%
      set_distinct_by(USUBJID) %>%
      set_custom_summaries(
          n_female = sum(.var == 2, na.rm = TRUE),
          pct_female = pct_bool(.var == 2),
          n_male = sum(.var == 1, na.rm = TRUE),
          pct_male = pct_bool(.var == 1)
      ) %>%
      set_format_strings(
          "n_int"        = f_str("xx", n),
          "Female_int"   = f_str("xx", n_female),
          "Female_pct" = f_str("xx.xxxxxxx", pct_female),
          "Male_int"     = f_str("xx", n_male),
          "Male_pct"   = f_str("xx.xxxxxxx", pct_male),
          "Missing_int"  = f_str("xx", missing)
      )
  ) %>%
  add_layer(
    group_desc(AGE, by = "Age (years)") %>%
      set_distinct_by(USUBJID) %>%
      set_format_strings(
        "n_int"           = f_str("xx", n),
        "Mean_n"     = f_str("xx.xxxxxxx", mean),
        "SD_n"         = f_str("xx.xxxxxxx", sd),
        "Median_n" = f_str("xx.xxxxxxx", median),
        "Q1, Q3_q1"         = f_str("xx.xxxxxxx", q1),
        "Q1, Q3_q3"         = f_str("xx.xxxxxxx", q3),
        "Min, Max_min"       = f_str("xx.xxxxxxx", min),
        "Min, Max_max"       = f_str("xx.xxxxxxx", max),
        "Missing_int"     = f_str("xx", missing)
      )
  ) %>%
  add_layer(
    group_desc(AGE, by = "Age Categories n (%)") %>%
      set_distinct_by(USUBJID) %>%
      set_custom_summaries(
        n_lt_65 = sum(.var<65, na.rm = TRUE),
        n_gte_65_lt_75 = sum(.var>=65 & .var <75, na.rm = TRUE),
        n_gte_75_lt_85 = sum(.var>=75 & .var <85, na.rm = TRUE),
        n_gte_85 = sum(.var>=85, na.rm = TRUE),
        pct_lt_65 = pct_bool(.var<65, na.rm = TRUE),
        pct_gte_65_lt_75 = pct_bool(.var>=65 & .var <75, na.rm = TRUE),
        pct_gte_75_lt_85 = pct_bool(.var>=75 & .var <85, na.rm = TRUE),
        pct_gte_85 = pct_bool(.var>=85, na.rm = TRUE)
      ) %>%
      set_format_strings(
        "n_int"          = f_str("xx", n),
        "<65_int"        = f_str("xx", n_lt_65),
        "<65_pct"      = f_str("xx.xxxxxxx", pct_lt_65),
        ">=65 and <75_int" = f_str("xx", n_gte_65_lt_75),
        ">=65 and <75_pct" = f_str("xx.xxxxxxx", pct_gte_65_lt_75),
        ">=75 and <85_int" = f_str("xx", n_gte_75_lt_85),
        ">=75 and <85_pct" = f_str("xx.xxxxxxx", pct_gte_75_lt_85),
        ">=85_int" = f_str("xx", n_gte_85),
        ">=85_pct" = f_str("xx.xxxxxxx", pct_gte_85),
        "Missing_int"  = f_str("xx", missing)
      )
  ) %>%
  add_layer(
    group_desc(AGE, by = "Age Categories n (%)") %>%
      set_distinct_by(USUBJID) %>%
      set_custom_summaries(
        n_gte_65 = sum(.var>=65, na.rm = TRUE),
        n_gte_75 = sum(.var>=75, na.rm = TRUE),
        pct_gte_65 = pct_bool(.var>=65, na.rm = TRUE),
        pct_gte_75 = pct_bool(.var>=75, na.rm = TRUE)
      ) %>%
      set_format_strings(
        ">=65_int"   = f_str("xx", n_gte_65),
        ">=65_pct" = f_str("xx.xxxxxxx", pct_gte_65),
        ">=75_int"   = f_str("xx", n_gte_75),
        ">=75_pct" = f_str("xx.xxxxxxx",  pct_gte_75)
      )
  ) %>%
  add_layer(
    group_desc(RACE_numeric, by = "Race n (%)") %>%
      set_distinct_by(USUBJID) %>%
      set_custom_summaries(
        n_american_indian_or_alaska_native = sum(.var == 1, na.rm = TRUE),
        pct_american_indian_or_alaska_native = pct_bool(.var == 1, na.rm = TRUE),
        n_asian = sum(.var == 2, na.rm = TRUE),
        pct_asian = pct_bool(.var == 2, na.rm = TRUE),
        n_black_or_african_american = sum(.var == 3, na.rm = TRUE),
        pct_black_or_african_american = pct_bool(.var == 3, na.rm = TRUE),
        n_native_hawaiian_or_other_pacific_islander = sum(.var == 4, na.rm = TRUE),
        pct_native_hawaiian_or_other_pacific_islander = pct_bool(.var == 4, na.rm = TRUE),
        n_white = sum(.var == 5, na.rm = TRUE),
        pct_white = pct_bool(.var == 5, na.rm = TRUE),
        n_multiple = sum(.var == 6, na.rm = TRUE),
        pct_multiple = pct_bool(.var == 6, na.rm = TRUE),
      ) %>%
      set_format_strings(
        "n_int"        = f_str("xx", n),
        "American Indian or Alaska Native_int" = f_str("xx", n_american_indian_or_alaska_native),
        "American Indian or Alaska Native_pct" = f_str("xx.xxxxxxx", pct_american_indian_or_alaska_native),
        "Asian_int" = f_str("xx", n_asian),
        "Asian_pct" = f_str("xx.xxxxxxx", pct_asian),
        "Black or African American_int" = f_str("xx", n_black_or_african_american),
        "Black or African American_pct" = f_str("xx.xxxxxxx", pct_black_or_african_american),
        "Native Hawaiian or Other Pacific Islander_int" = f_str("xx", n_native_hawaiian_or_other_pacific_islander),
        "Native Hawaiian or Other Pacific Islander_pct" = f_str("xx.xxxxxxx", pct_native_hawaiian_or_other_pacific_islander),
        "White_int" = f_str("xx", n_white),
        "White_pct" = f_str("xx.xxxxxxx", pct_white),
        "Multiple_int" = f_str("xx", n_multiple),
        "Multiple_pct" = f_str("xx.xxxxxxx", pct_multiple),
        "Missing_int"  = f_str("xx", missing)
      )
  ) %>%
  add_layer(
    group_desc(ETHNIC_numeric, by = "Ethnicity n (%)") %>%
      set_distinct_by(USUBJID) %>%
      set_custom_summaries(
        n_hispanic_or_latino = sum(.var == 1, na.rm = TRUE),
        pct_hispanic_or_latino = pct_bool(.var == 1, na.rm = TRUE),
        n_not_hispanic_or_latino = sum(.var == 2, na.rm = TRUE),
        pct_not_hispanic_or_latino = pct_bool(.var == 2, na.rm = TRUE),
      ) %>%
      set_format_strings(
        "n_int"        = f_str("xx", n),
        "Hispanic or Latino_int" = f_str("xx", n_hispanic_or_latino),
        "Hispanic or Latino_pct" = f_str("xx.xxxxxxx", pct_hispanic_or_latino),
        "Not Hispanic or Latino_int" = f_str("xx", n_not_hispanic_or_latino),
        "Not Hispanic or Latino_pct" = f_str("xx.xxxxxxx", pct_not_hispanic_or_latino)
      )
  ) %>%
  add_layer(
    group_desc(WEIGHTBL, by = "Weight (kg)") %>%
      set_distinct_by(USUBJID) %>%
      set_format_strings(
        "n_int"        = f_str("xx", n),
        "Mean_n"= f_str("xx.xxxxxxx", mean),
        "SD_n"= f_str("xx.xxxxxxx", sd)
      )
  ) %>%
  build() %>%
  pivot_longer(
    starts_with("var1"),
    names_to = "treatment",
    values_to = "value",
    names_prefix = "var1_"
  ) %>%
  separate(
    row_label2,
    into = c("row_label2","param"),
    sep = "_"
  ) %>%
  mutate(
    value = as.numeric(value)
  )


tplyr_demog_template <- function(tfrmt_obj, sigfigs = 1){

  basic_int <- frmt("xx")
  basic_pct_frmt <- frmt_when("==100" ~ "","==0" ~ "",TRUE ~ frmt("(x.x)"))
  basic_int_pct_frmt <- frmt_combine("{int} {pct}", int = basic_int, pct = basic_pct_frmt)

  sig_fig_n <- function(sigfigs = 1) {frmt(paste0("xx.",paste0(rep("x", sigfigs),collapse="")))}

  basic_two_param_int <- function(p1,p2) {
    arg_list <- list(
      paste0("{",p1,"}, {",p2,"}"),
      basic_int,
      basic_int
    )
    names(arg_list) <- c("expression", p1, p2)
    do.call('frmt_combine',arg_list)
  }

  demog_template <- tfrmt(
    # specify columns in the data
    group = row_label1,
    label = row_label2,
    column = treatment,
    param = param,
    values = value,
    sorting_cols = c(ord_layer_index, ord_layer_1, ord_layer_2),

    # Specify body plan
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", basic_int_pct_frmt),
      frmt_structure(group_val = ".default", label_val = "Q1, Q3", basic_two_param_int("q1","q3")),
      frmt_structure(group_val = ".default", label_val = "Min, Max", basic_two_param_int("min","max")),
      frmt_structure(group_val = ".default", label_val = c("Mean","Median"), sig_fig_n(sigfigs = sigfigs)),
      frmt_structure(group_val = ".default", label_val = c("SD"), sig_fig_n(sigfigs = sigfigs+1)),
      frmt_structure(group_val = ".default", label_val = c("n","Missing"), basic_int)
    ),

    # Specify row group plan
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "column")
    ),

    # Specify column alignment plan
    col_align = col_align_plan(
      element_align(align = c(".",","," "), col = vars(everything()))
    ),

    # remove extra cols
    col_plan = col_plan(
      -starts_with("ord"),
      span_structure(
        label = "Xanomeline Dose",
        "High" = `Xanomeline High Dose`,
        "Low" = `Xanomeline Low Dose`
      ),
      Placebo
      )
  )


  if(!missing(tfrmt_obj)){
    demog_template <- layer_tfrmt(tfrmt_obj, demog_template)
  }

  demog_template

}

tplr_demog_table_tfrmt <- tfrmt(
    title = "Table 7.1 Demography Summary",
    subtitle = "Tplyr/tlang Example Demography\n Table",
    footer = "Using Tplyr, the dataset were built\n and then this table is generated using tlang"
  ) %>%
  tplyr_demog_template(sigfigs = 2)

## real data
tplr_demog_table_tfrmt %>%
  print_to_gt(demog_ARD)

## mock
tplr_demog_table_tfrmt %>%
  print_mock_gt(demog_ARD %>% select(-value))



tplr_demog_table_tfrmt %>%
  print_to_gt(demog_ARD %>%
                mutate(
                  row_label2 = gsub(" or "," or\n", row_label2)
                )) %>%
  cols_width(

  )
