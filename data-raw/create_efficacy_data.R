# data for table 3.01
# code adapted from Atorus cdisc pilot replication https://github.com/atorus-research/CDISC_pilot_replication/blob/master/programs/funcs.R

library(dplyr)
library(forcats)
library(lme4)
library(emmeans)
library(broom)
library(gt)
library(tidyr)

adas <- safetyData::adam_adqsadas %>%
  filter(EFFFL == "Y" &               # efficacy population
           ITTFL == "Y" &             # ITT population
           PARAMCD == 'ACTOT' &       # ADAS-Cog(11) subscore
           ANL01FL == 'Y'             # Analysis record flag 01
           ) %>%
  mutate(AVISIT = fct_reorder(AVISIT, AVISITN),
         TRTP = fct_reorder(TRTP, TRTPN)) %>%
  select(USUBJID, SITEGR1, SEX, TRTP, TRTPN, AVISIT, AVISITN, BASE, CHG, AVAL)

### 1. summary data
summ_dat <- adas %>%
  select(USUBJID, AVISIT, TRTP, AVAL, CHG) %>%
  filter(AVISIT %in% c("Baseline","Week 24")) %>%
  group_by(AVISIT, TRTP) %>%
  summarise(across(c(AVAL,CHG), list(n = length, mean = mean, sd = sd, median = median, min = min, max = max))) %>%
  pivot_longer(-c(AVISIT, TRTP), names_to = "param", values_to = "value") %>%
  filter(!(AVISIT=="Baseline" & str_detect(param, "CHG"))) %>%
  separate(param, c("val", "param"), sep = "_") %>%
  mutate(label = case_when(
    param == "n" ~ "n",
    param %in% c("mean","sd") ~ "Mean (SD)",
    TRUE ~ "Median (Range)"
  ),
  AVISIT = as.character(AVISIT),
  AVISIT= ifelse(val=="CHG","Change from Baseline", AVISIT)) %>%
  rename(column = TRTP,
         group = AVISIT) %>%
  select(-val)

### 2. dose-response p-value
adas2 <- adas %>% filter(AVISIT=="Week 24")
mod2 <- lm(CHG ~ TRTPN + SITEGR1 + BASE, data=adas2)
dose_resp_p <- tibble(param = "p.value",
                      label = "p-value (Dose Response)",
                      value = car::Anova(mod2, type=3)[2, 'Pr(>F)'],
                      column = "Xanomeline High Dose",
                      group = "p-value (Dose Response)")

### 3. emmeans results
mod1 <- lm(CHG ~ TRTP + SITEGR1 + BASE, data=adas2)
emm <- emmeans(mod1, ~TRTP, weights='proportional')
emm_diff <- emm %>% contrast(method = "revpairwise", adjust = "none") %>% summary(infer = TRUE)

# tidy up
emm_diff_df <- emm_diff %>% as.data.frame() %>%
  select(TRTP = contrast, diff = estimate, diff_se = SE, diff_lcl = lower.CL, diff_ucl = upper.CL, p.value)
efficacy_dat <- emm_diff_df %>%
  pivot_longer(-TRTP, names_to = "param", values_to = "value") %>%
  na.omit() %>%
  mutate(label = case_when(
    param %in% c("mean","se") ~ "LS Means (SE)",
    param %in% c("diff_lcl","diff_ucl") ~ "95% CI",
    param %in% c("diff", "diff_se") ~ "Diff of LS Means (SE)",
    param == "p.value" & TRTP=="Xanomeline High Dose - Xanomeline Low Dose" ~ "p-value (Xan High - Xan Low)",
    param == "p.value" & TRTP=="Xanomeline High Dose - Placebo" ~ "p-value (Xan - Placebo)",
    param == "p.value" & TRTP=="Xanomeline Low Dose - Placebo" ~ "p-value (Xan - Placebo)"),
    group = case_when(
      label=="LS Means (SE)" | str_detect(label, "p-value") ~ label,
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(param_ord = as.numeric(factor(param, levels = c("mean","se","p.value","diff","diff_se", "diff_lcl","diff_ucl"))),
         column = str_extract(TRTP, "[^-]+") %>% trimws()) %>%
  arrange(TRTP, column, param_ord) %>%
  fill(group, .direction = "down") %>%
  select(-c(TRTP, param_ord)) %>%
  ungroup %>%
  select(group, label, column, param, value)

# all combined
efficacy_data <- summ_dat %>%
  bind_rows(dose_resp_p) %>%
  bind_rows(efficacy_dat) %>%
  select(group, label, column, param, value)%>%
  ungroup  %>%
  mutate(ord1 = as.numeric(factor(group, levels = c("Baseline", "Week 24", "Change from Baseline", "p-value (Dose Response)","p-value (Xan - Placebo)","p-value (Xan High - Xan Low)"))),
         ord2 = case_when(
           param == "n" ~ 1,
           param %in% c("mean","se") ~ 2,
           param %in% c("median", "min", "max") ~ 3,
           param =="p.value" ~ 4,
           param %in% c("diff", "diff_se") ~ 5,
           TRUE ~ 6)) %>%
  arrange(ord1, ord2)


usethis::use_data(efficacy_data, overwrite = TRUE)

