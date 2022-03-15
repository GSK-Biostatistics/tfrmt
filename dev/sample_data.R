library(safetyData)
library(gsktable)
library(tidyr)


age_grp <- adam_adsl %>%
  freq_tbl(count_by = ARM, AGEGR1, include_raw = TRUE, total_row = TRUE, total_col = TRUE) %>%
  mutate(row_label1 = "Age Grp",
         AGEGR1 = as.character(AGEGR1),
         row_label2 = case_when(AGEGR1 == "TOTAL" ~ "n",
                                TRUE ~ AGEGR1),
         ord_layer_1 = 3,
         ord_layer_2 = row_number()
         ) %>%
  select(starts_with("row"), everything(), -AGEGR1, -Placebo,
         -`Xanomeline High Dose`, -`Xanomeline Low Dose`)

sex <- adam_adsl %>%
  freq_tbl(count_by = ARM, SEX, include_raw = TRUE, total_row = TRUE, total_col = TRUE) %>%
  mutate(row_label1 = "Sex",
         SEX = as.character(SEX),
         row_label2 = case_when(SEX == "TOTAL" ~ "n",
                                TRUE ~ SEX),
         ord_layer_1 = 1,
         ord_layer_2 = row_number()) %>%
  select(starts_with("row"), everything(), -SEX, -Placebo,
         -`Xanomeline High Dose`, -`Xanomeline Low Dose`)


race <- adam_adsl %>%
  freq_tbl(count_by = ARM, RACE, include_raw = TRUE, total_row = TRUE, total_col = TRUE) %>%
  mutate(row_label1 = "Race",
         RACE = as.character(RACE),
         row_label2 = case_when(RACE == "TOTAL" ~ "n",
                                TRUE ~ RACE),
         ord_layer_1 = 3,
         ord_layer_2 = row_number()) %>%
  select(starts_with("row"), everything(), -RACE, -Placebo,
         -`Xanomeline High Dose`, -`Xanomeline Low Dose`)

eth <- adam_adsl %>%
  freq_tbl(count_by = ARM, ETHNIC, include_raw = TRUE, total_row = TRUE, total_col = TRUE) %>%
  mutate(row_label1 = "Ethinicty",
         ETHNIC = as.character(ETHNIC),
         row_label2 = case_when(ETHNIC == "TOTAL" ~ "n",
                                TRUE ~ ETHNIC),
         ord_layer_1 = 5,
         ord_layer_2 = row_number()) %>%
  select(starts_with("row"), everything(), -ETHNIC, -Placebo,
         -`Xanomeline High Dose`, -`Xanomeline Low Dose`)

age <- adam_adsl %>%
  group_by(ARM) %>%
  sum_stats(AGE) %>%
  transpose_tbl(wide_column = ARM)
age <- adam_adsl %>%
  sum_stats(AGE) %>%
  pivot_longer(everything()) %>%
  select(total = value) %>%
  bind_cols(age, .) %>%
  mutate(row_label1 = "Age",
         ord_layer_1 = 2,
         ord_layer_2 = row_number()) %>%
  rename(row_label2 = LABEL) %>%
  select(starts_with("row"), everything())

weight <- adam_adsl %>%
  group_by(ARM) %>%
  sum_stats(WEIGHTBL) %>%
  transpose_tbl(wide_column = ARM)
weight <- adam_adsl %>%
  sum_stats(WEIGHTBL) %>%
  pivot_longer(everything()) %>%
  select(total = value) %>%
  bind_cols(weight, .) %>%
  mutate(row_label1 = "Weight",
         ord_layer_1 = 6,
         ord_layer_2 = row_number()) %>%
  rename(row_label2 = LABEL) %>%
  select(starts_with("row"), everything())


data <- bind_rows(age_grp, sex, race, eth, age, weight)

