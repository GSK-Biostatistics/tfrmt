
# CDISC Pilot table 14-5.01 -----------------------------------------------
# Code adapted from: https://github.com/atorus-research/CDISC_pilot_replication/blob/master/programs/t-14-5-01.R


# load libraries ----------------------------------------------------------

library(tidyverse)
library(safetyData)
library(glue)


# Source functions --------------------------------------------------------

source("dev/data/funcs.R")

create_tbl_ae_data <- function(){

  # Read in ADSL
  adae <- safetyData::adam_adae %>%
    filter(SAFFL == 'Y' & TRTEMFL == 'Y')

  adsl <- safetyData::adam_adsl

  # Header N ----
  header_n <- adsl %>%
    get_header_n()

  # Overall counts
  overall <- ae_counts(adae, N_counts = header_n) %>%
    mutate(AETERM = 'ANY BODY SYSTEM', AEBODSYS = 'ANY BODY SYSTEM', ord1=0, ord2 =0)

  # System Organ Class counts
  bodsys <- ae_counts(adae, AEBODSYS, N_counts = header_n) %>%
    arrange(AEBODSYS) %>%
    mutate(AETERM = AEBODSYS, ord1 = row_number(), ord2 = 0)

  # Individual term counts
  term <- ae_counts(adae, AEBODSYS, AETERM, sort=TRUE, N_counts = header_n) %>%
    group_by(AEBODSYS, AETERM) %>%
    arrange(desc(ord3), AETERM) %>%
    mutate(ord2=row_number())

  # Bring the data together
  combined <- bind_rows(overall, bodsys,  term) %>%
    select(-ord3) %>%
    group_by(AEBODSYS) %>%
    fill(ord1) %>%
    arrange(ord1, ord2)


  # Make long & prep for tlang ----------------------------------------------------------

  tab_ae_data <- combined %>%
    select(-contains("no_event_")) %>%
    pivot_longer(-c(AEBODSYS, AETERM, ord1, ord2, p_low, p_high)) %>%
    separate(name, c("param", "column"), sep = "_") %>%
    mutate(column = case_when(
      column == 0 ~ "Placebo",
      column==54 ~ "Xanomeline Low Dose",
      column==81 ~ "Xanomeline High Dose"
    )) %>%
    pivot_wider(names_from = param, values_from = value) %>%
    mutate(pct = 100*n/N) %>%
    select(-N) %>%
    pivot_longer(c(n, pct, AEs, p_low, p_high), names_to = "param", values_to = "value") %>%
    mutate(column = ifelse(str_detect(param, "p_"), param, column),
           lab = case_when(
             param %in% c("n","pct") ~ "n (%)",
             param == "AEs" ~ "[AEs]",
             str_detect(param, "p_") ~ NA_character_,
             TRUE ~ param),
           param = ifelse(str_detect(param,"p_"), "pval", param)) %>%
    unite("column", c(column, lab), remove = TRUE, sep = "__", na.rm=TRUE) %>%
    unique

   return(tab_ae_data)
}
