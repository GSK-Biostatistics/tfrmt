### generate example demography ARD from tplyr

library(tidyverse)
library(Tplyr)

# copied from https://github.com/atorus-research/Tplyr/blob/master/vignettes/adsl.Rdata
load("dev/data/adsl.Rdata")

demog_tplyr_raw <- tplyr_table(adsl, TRT01P) %>%
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

demog <- demog_tplyr_raw %>%
  mutate(
    labels = case_when(
      row_label2 %in%  c("Age Groups", "Race") ~ row_label1,
      TRUE ~ row_label3
    ),
    row_label2 = case_when(
      row_label2 %in%  c("Age Groups", "Race") ~ paste0(row_label2, " (", row_label3,")"),
      TRUE ~ row_label2
    ),
    value = case_when(
      param == "pct" ~ value * 100,
      TRUE ~ value
    )
  ) %>%
  select(-row_label1)



tplyr_demog_template <-
  function(sigfigs,
           group_col = c("row_label2","row_label3"),
           label_col = "labels",
           colname_col = "col1",
           tfrmt_obj
  ) {

  basic_n <- frmt("xx")
  basic_pct_frmt <- frmt_when("==100" ~ "","==0" ~ "",TRUE ~ frmt("(x.x)"))
  basic_n_pct_frmt <- frmt_combine("{n} {pct}", n = basic_int, pct = basic_pct_frmt)

  sig_fig_n <- function(sigfigs = 1) {
    expr <- "xx"
    if(sigfigs > 0){
      expr <- paste0(expr,".",paste0(rep("x", sigfigs),collapse=""))
    }
    frmt(expr)
  }

  basic_two_param <- function(p1,p2) {
    arg_list <- list(
      paste0("{",p1,"}, {",p2,"}"),
      basic_n,
      basic_n
    )
    names(arg_list) <- c("expression", p1, p2)
    do.call('frmt_combine',arg_list)
  }

  basic_two_param_dbl <- function(p1,p2, sf1 = 1, sf2 = sf1) {
    arg_list <- list(
      paste0("{",p1,"}, {",p2,"}"),
      sig_fig_n(sf1),
      sig_fig_n(sf2)
    )
    names(arg_list) <- c("expression", p1, p2)
    do.call('frmt_combine',arg_list)
  }

  demog_template <- tfrmt(
    # specify columns in the data
    group = group_col,
    label = label_col,
    column = colname_col,
    param = param,
    values = value,

    # Specify body plan
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", sig_fig_n(sigfigs = sigfigs)),
      frmt_structure(group_val = ".default", label_val = ".default", basic_two_param_dbl("n", "pct", 0, 1)),
      frmt_structure(group_val = ".default", label_val = ".default", basic_two_param("min","max")),
      frmt_structure(group_val = ".default", label_val = ".default", basic_two_param_dbl("mean","sd", 1, 2))
      ),

    # Specify row group plan
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = " ")),
      label_loc = element_row_grp_loc(location = "indent")
    ),

    # Specify column alignment plan
    col_align = col_align_plan(
      element_align(align = c(".",","," "), col = vars(everything()))
    )
  )


  if(!missing(tfrmt_obj)){
    demog_template <- layer_tfrmt(tfrmt_obj, demog_template)
  }

  demog_template

}

tplr_demog_table_tfrmt <- tplyr_demog_template(sigfigs = 2) %>%
  tfrmt(
    title = "Table 7.1 Demography Summary",
    subtitle = "Tplyr/tlang Example Demography\n Table",
    footer = "Demography data processed by Tplyr and the table is generated using tfrmt",
    # remove extra cols
    col_plan = col_plan(
      row_label2,
      labels,
      span_structure(
        label = "Xanomeline Dose",
        "High" = `Xanomeline High Dose`,
        "Low" = `Xanomeline Low Dose`
      ),
      Placebo
    )
  )


## real data
tplr_demog_table_tfrmt %>%
  print_to_gt(demog)

## mock
tplr_demog_table_tfrmt %>%
  print_mock_gt(demog %>% select(-value))



tplr_demog_table_tfrmt %>%
  print_to_gt(demog_ %>%
                mutate(
                  row_label2 = gsub(" or "," or\n", row_label2)
                )) %>%
  cols_width(

  )
