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
         ord_layer_1 = 4,
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
  sum_stats(AGE, include_raw = TRUE) %>%
  select(contains("raw")) %>%
  pivot_longer(contains("raw")) %>%
  ungroup()
age <- adam_adsl %>%
  sum_stats(AGE, include_raw = TRUE) %>%
  select(contains("raw")) %>%
  pivot_longer(everything()) %>%
  mutate(ARM = "total") %>%
  bind_rows(age, .) %>%
  mutate(row_label1 = "Age",
         ord_layer_1 = 2,
         row_label2 = str_remove(name, "_raw$"),
         column = ARM) %>%
  select(-ARM, -name)

weight <- adam_adsl %>%
  group_by(ARM) %>%
  sum_stats(WEIGHTBL, include_raw = TRUE) %>%
  select(contains("raw")) %>%
  pivot_longer(contains("raw")) %>%
  ungroup()
weight <- adam_adsl %>%
  sum_stats(WEIGHTBL, include_raw = TRUE) %>%
  select(contains("raw")) %>%
  pivot_longer(everything()) %>%
  mutate(ARM = "total") %>%
  bind_rows(weight, .) %>%
  mutate(row_label1 = "Weight",
         ord_layer_1 = 6,
         row_label2 = str_remove(name, "_raw$"),
         column = ARM) %>%
  select(-ARM, -name)


data_freq <- bind_rows(age_grp, sex, race, eth) %>%
  mutate(across(contains("percent"), ~.*100)) %>%
  pivot_longer(contains("raw"),
               names_to = c("column", "param", "raw"),
               names_pattern = "(.*)_(.*)_(.*)") %>%
  select(-total, -raw)

data_stats <- bind_rows(age, weight) %>%
  mutate(param = row_label2,
         row_label2 = case_when(row_label2 == "Min" ~ "Min., Max.",
                                row_label2 == "Max" ~ "Min., Max.",
                                TRUE ~ row_label2),
         ord_layer_2 = case_when(row_label2 == "n" ~ 1L,
                                 row_label2 == "Mean" ~ 2L,
                                 row_label2 == "Std" ~ 3L,
                                 row_label2 == "Median" ~ 4L,
                                 row_label2 == "Min., Max." ~ 5L,)
         )


data <- bind_rows(data_freq, data_stats) %>%
  arrange(ord_layer_1, ord_layer_2, row_label1, row_label2, column, param)
