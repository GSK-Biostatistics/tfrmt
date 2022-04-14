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

# Attach P-value to the first row of a dataframe
attach_p <- function(.data, p_value, digits = 4) {
  # Empty column
  .data[['p']] = character(nrow(.data)) %>% as.numeric
  .data[1, 'p'] = p_value

  .data

}




sum_subgrp <- function(.data, subgroup_var, order_var = NULL, include.n=TRUE, pad.row=TRUE, header_n = header_n) {
  # Create n (%) subgroups by TRT01P

  # Pull from adsl with totals
  subgrps <- .data %>%
    # Keep only the gtwo group variables and group byC:\Users\16105\OneDrive - ATorus\Documents\Projects\Explore\test2.rtf
    select(TRT01PN, {{ subgroup_var }}, {{ order_var }}) %>%
    filter(!is.na({{ subgroup_var }})) %>%
    group_by(TRT01PN, {{ subgroup_var }}, {{ order_var }}) %>%
    # Summarize counts
    summarize(
      n = n()
    ) %>%
    arrange(TRT01PN, {{ order_var }}) %>%
    # Merge with big Ns
    left_join(header_n, by = 'TRT01PN') %>%
    rowwise() %>%
    # Create the n (%) string
    mutate(
      pct = 100*n/N
      # res = n_pct(n, N)
    ) %>%
    # Drop unnecessary vars
    select(-N,  -labels, -{{ order_var }}) %>%
    # Transpose
    pivot_longer(c(n, pct), names_to = "param", values_to = "vals") %>%
    pivot_wider(names_from = TRT01PN, values_from = vals)  %>%
    # # Rename row label column
    rename(rowlbl2 = {{ subgroup_var }})

  if (include.n){
    subgrps <- bind_rows(desc_stats(.data, {{subgroup_var}}, include='n')[1,],
                         subgrps)
  }

  subgrps

}

desc_stats <- function(.data, var, group = TRT01PN, na.rm=TRUE, int_len=3, size=10, include=c('n', 'Mean', 'SD', 'Median', 'Min', 'Max')) {
  # Provides descriptive statistics of provided variable, by TRT01PN
  # n, Mean, SD, Median, Min, Max

  # Ensure that the include argument was valid
  include = match.arg(include, several.ok=TRUE)

  # This is gonna get wonky - store each summary as an expression
  #TODO: Allow flexibility in significant digits - right now it's hard coded
  summaries <- list(
    n      = rlang::expr(n()),
    Mean   = rlang::expr(mean({{ var }})),
    SD     = rlang::expr(sd({{ var }})),
    Median = rlang::expr(median({{ var }})),
    Min    = rlang::expr(min({{ var }})),
    Max    = rlang::expr(max({{ var }}))
  )[include] # this is a named list, so subset based on the input arguments

  # Pull from ADSL with totals
  .data %>%
    # Pick of TRT01PN and the variable of interest
    select({{ group }}, {{ var }}) %>%
    # Filter out missing values
    filter(!is.na({{ var }})) %>%
    # Group by treatment
    group_by({{ group }}) %>%
    # Summarize each statistic and use num_fmt for rounding/formatting
    summarize(!!!summaries) %>% # unpack the expressions into syntax to be evaluated
    # Transpose statistics into one column
    pivot_longer(-{{ group }}, names_to = 'rowlbl2', values_to = 'temp') %>%
    # Transpose treatments into separate columns
    pivot_wider(names_from = {{ group }}, values_from = temp)

}


# P-value for anova test
aov_p <- function(.data, forumula) {
  # Run the anova test
  a <- aov(forumula, .data, na.action=na.omit)

  # Extract the P value
  p <- summary(a)[[1]][['Pr(>F)']][1]
}


# P-value for chi-squared
chi_p <- function(data, results, categories) {
  # get the arguments as a off of the function call
  arguments <- as.list(match.call())
  # Evaluate the arguments within the dataframe environment
  # This is all just so I can allow acceptance of variables without quotes
  cats <- factor(eval(arguments$categories, data))
  res <- factor(eval(arguments$results, data))

  chisq.test(res, cats)$p.value
}

# P-vaule for fisher's test
fish_p <- function(data, results, categories, width = 10) {
  # get the arguments as a off of the function call
  arguments <- as.list(match.call())
  # Evaluate the arguments within the dataframe environment
  # This is all just so I can allow acceptance of variables without quotes
  cats <- factor(eval(arguments$categories, data))
  res <- factor(eval(arguments$results, data))

  fisher.test(res, cats)$p.value
}

