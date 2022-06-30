### generate example demography ARD from tplyr

library(tidyverse)
library(Tplyr)

pct_bool <- function(x, ...){
  mean(x, ...) * 100
}

# copied from https://github.com/atorus-research/Tplyr/blob/master/vignettes/adsl.Rdata
load("dev/data/adsl.Rdata")

demog <- tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_desc(AGE, by = "Age (years)") %>%
      set_summaries(
        "Mean (SD)" = vars(mean, sd),
        "Min, Max" = vars(min, max),
        "IQR" = vars(iqr)
      )
  ) %>%
  add_layer(
    group_count(AGEGR1, by = "Age Groups") %>%
      add_total_row() %>%
      set_summaries(
        "n (%)" = vars(n, pct)
      )
  ) %>%
  add_layer(
    group_count(RACE, by = "Race") %>%
      add_total_row() %>%
      set_summaries(
        "n (%)" = vars(n, pct)
      )
  ) %>%
  add_layer(
    group_desc(WEIGHTBL, by = "Weight at Baseline") %>%
      set_summaries(
        "Mean (SD)" = vars(mean, sd),
        "Min, Max" = vars(min, max),
        "IQR" = vars(iqr)
      )
  ) %>%
  get_numeric_data()


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
