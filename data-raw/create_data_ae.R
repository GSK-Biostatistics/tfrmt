
# CDISC Pilot table 14-5.01 -----------------------------------------------
# Code adapted from: https://github.com/atorus-research/CDISC_pilot_replication/blob/master/programs/t-14-5-01.R


# load libraries ----------------------------------------------------------

library(tidyverse)
library(safetyData)
library(glue)


# Source functions --------------------------------------------------------

# Functions adapted from:
# https://github.com/atorus-research/CDISC_pilot_replication/blob/master/programs/funcs.R

get_header_n <- function(.data, trtp = TRT01P, trtpn = TRT01PN) {
  # Extract header N's into a dataframe to be used on merges or for display

  trtp = enquo(trtp)
  trtpn = enquo(trtpn)

  # Get the header N's ----
  .data %>%
    group_by(!!trtp, !!trtpn) %>%
    summarize(N = n()) %>%
    mutate(
      trtp = !!trtp,
      labels = str_replace_all(str_wrap(glue('{trtp} (N={N})'), width=10), "\n", function(x) "\\line ")
    ) %>%
    ungroup() %>%
    arrange(!!trtpn) %>%
    select(-!!trtp, -trtp)
}

# Count of subjects with an adverse event
ae_counts <- function(.data, ..., N_counts = NULL, sort=FALSE) {

  # Get the grouping
  grouped_data <- .data %>%
    group_by(TRTAN, TRTA, ...) %>%
    select(TRTA, TRTAN, ..., USUBJID)

  # Counts of each subject
  subject_counts <- grouped_data %>%
    distinct() %>%
    summarize(n = n())

  # Count of adverse events
  event_counts <- grouped_data %>%
    summarize(AEs = n())

  # Join the subject and event counts, pivot out by treatment
  counts <- subject_counts %>%
    left_join(event_counts) %>%
    pivot_wider(id_cols=c(...), names_from=TRTAN, values_from=c(n, AEs))

  # If no events for a treatment group, they won't be in the pivoted table, so create
  for (g in unique(N_counts$TRT01PN)) {
    cnames <- c(paste0('n_', g), paste0('AEs_', g))
    if (!all(cnames %in% names(counts))) {
      # If one is missing, they're both missing
      counts[cnames[1]] <- 0
      counts[cnames[2]] <- 0
    }
  }

  # Add in subject counts
  counts['N_0'] <- N_counts[N_counts$TRT01PN == 0, 'N']
  counts['N_54'] <- N_counts[N_counts$TRT01PN == 54, 'N']
  counts['N_81'] <- N_counts[N_counts$TRT01PN == 81, 'N']

  # Fill all NAs with 0
  counts[is.na(counts)] <- 0

  # Find no event counts
  counts['no_event_0'] <- counts$N_0 - counts$n_0
  counts['no_event_54'] <- counts$N_54 - counts$n_54
  counts['no_event_81'] <- counts$N_81 - counts$n_81

  # Calculate p-values
  counts['p_low']  <- apply(counts[, c('n_0', 'n_54', 'no_event_0', 'no_event_54')], MARGIN=1, FUN=fisher_test_ae)
  counts['p_high'] <- apply(counts[, c('n_0', 'n_81', 'no_event_0', 'no_event_81')], MARGIN=1, FUN=fisher_test_ae)


  counts %>% mutate(ord3 = n_81)
}


# Fisher test built for row-wise derivations suited for our AE tables
fisher_test_ae <- function(.data) {

  # If there were no events in either treatment arm then don't compute
  if (sum(.data[1:2]) == 0){
    return(NA)
  }

  # convert to a 2X2 matrix
  dim(.data) <- c(2, 2)

  # Return the p-value of interest
  fisher.test(.data)$p.value

}


# Create AE table data
create_tbl_data_ae <- function(){

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

  tab_data_ae <- combined %>%
    select(-contains("no_event_")) %>%
    pivot_longer(-c(AEBODSYS, AETERM, ord1, ord2, p_low, p_high)) %>%
    separate(name, c("stat", "col2"), sep = "_") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(pct = 100*n/N) %>%
    select(-N) %>%
    pivot_longer(c(n, pct, AEs, p_low, p_high), names_to = "stat", values_to = "value") %>%
    mutate(col2 = ifelse(str_detect(stat,"p_"), 999, col2)) %>%
    arrange(ord1, ord2, col2) %>%
    mutate(col2 = case_when(
      col2 == 0 ~ "Placebo",
      col2==54 ~ "Xanomeline Low Dose",
      col2==81 ~ "Xanomeline High Dose"
    )) %>%
    mutate(col2 = ifelse(str_detect(stat, "p_"), "fisher_pval", col2),
           param = ifelse(str_detect(stat, "p_"), "pval", stat),
           col1 = case_when(
             stat %in% c("n","pct") ~ "n_pct",
             TRUE ~ stat)) %>%
    select(AEBODSYS, AETERM, col2, col1, param, value, ord1, ord2) %>%
    unique

  return(tab_data_ae)
}

data_ae <- create_tbl_data_ae()
usethis::use_data(data_ae, overwrite = TRUE)
