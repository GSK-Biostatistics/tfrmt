# data for CDISC Pilot table 14-6.01 -----------------------------------------------
# Code adapted from: https://github.com/atorus-research/CDISC_pilot_replication/blob/master/programs/t-14-6-01.R



library(dplyr)
library(forcats)
library(purrr)
library(stringr)
library(tidyr)

# Read in the ADLB datasets
adlbc <- safetyData::adam_adlbc %>%
  filter(SAFFL == 'Y' & (AVISITN != 99 | (AVISITN == 99 & AENTMTFL=='Y'))) %>%
  filter(!str_detect(PARAM, "change from previous visit")) %>%
  mutate(PARAM = recode(PARAM,
                     "Alanine Aminotransferase (U/L)" = "ALANINE AMINOTRANSFERASE",
                     "Albumin (g/L)" = "ALBUMIN",
                     "Alkaline Phosphatase (U/L)" = "ALKALINE PHOSPHATASE",
                     "Aspartate Aminotransferase (U/L)" = "ASPARTATE AMINOTRANSFERASE",
                     "Bilirubin (umol/L)" = "BILIRUBIN",
                     "Calcium (mmol/L)" = "CALCIUM",
                     "Chloride (mmol/L)" = "CHLORIDE",
                     "Cholesterol (mmol/L)" = "CHOLESTEROL",
                     "Creatine Kinase (U/L)" = "CREATINE KINASE",
                     "Creatinine (umol/L)" = "CREATININE",
                     "Gamma Glutamyl Transferase (U/L)" = "GAMMA GLUTAMYL TRANSFERASE",
                     "Glucose (mmol/L)" = "GLUCOSE",
                     "Phosphate (mmol/L)" = "PHOSPHATE",
                     "Potassium (mmol/L)" = "POTASSIUM",
                     "Protein (g/L)" = "PROTEIN",
                     "Sodium (mmol/L)" = "SODIUM",
                     "Urate (umol/L)" = "URATE",
                     "Blood Urea Nitrogen (mmol/L)" = "UREA NITROGEN")) %>%
  filter(!AVISIT=="UNSCHEDULED") %>%
  mutate(AVISIT = fct_reorder(AVISIT, AVISITN))

adlbh <- safetyData::adam_adlbh %>%
  filter(SAFFL == 'Y' & !(PARAM %in% c('Anisocytes', 'Poikilocytes', 'Microcytes', 'Macrocytes', 'Polychromasia'))
         & (AVISITN != 99 | (AVISITN == 99 & AENTMTFL=='Y'))) %>%
  filter(!str_detect(PARAM, "change from previous visit")) %>%
  mutate(PARAM = recode(PARAM,
                     "Basophils (GI/L)" = "BASOPHILS",
                     "Eosinophils (GI/L)" = "EOSINOPHILS",
                     "Ery. Mean Corpuscular HGB Concentration (mmol/L)" = "ERY. MEAN CORPUSCULAR HB CONCENTRATION",
                     "Ery. Mean Corpuscular Hemoglobin (fmol(Fe))" = "ERY. MEAN CORPUSCULAR HEMOGLOBIN",
                     "Ery. Mean Corpuscular Volume (fL)" = "ERY. MEAN CORPUSCULAR VOLUME",
                     "Erythrocytes (TI/L)" = "ERYTHROCYTES",
                     "Hematocrit" = "HEMATOCRIT",
                     "Hemoglobin (mmol/L)" = "HEMOGLOBIN",
                     "Leukocytes (GI/L)" = "LEUKOCYTES",
                     "Lymphocytes (GI/L)" = "LYMPHOCYTES",
                     "Monocytes (GI/L)" = "MONOCYTES",
                     "Platelet (GI/L)" = "PLATELET")) %>%
  filter(!AVISIT=="UNSCHEDULED") %>%
  mutate(AVISIT = fct_reorder(AVISIT, AVISITN))

test_summary <- function(x, df_=NULL) {
  # Build up the visit table and attach on the end visit (using flag)

  visits <- df_ %>%
    # Filter to the specified test
    filter(PARAM == x)

  # Summarize results by visit and treatment
  res <- visits %>%
    group_by(PARAM, AVISIT, TRTP) %>%
    summarize(n = n(),
              mean_res = mean(AVAL, na.rm=TRUE),
              sd_res = sd(AVAL, na.rm=TRUE))

  # Summarize change from baseline by visit and treatment
  chgbl <- visits %>%
    filter(AVISITN != 1) %>%
    group_by(PARAM, AVISIT, TRTP) %>%
    summarize(mean_cbl = mean(CHG, na.rm=TRUE),
              sd_cbl = sd(CHG, na.rm=TRUE))

  # Build the display string
  df <- full_join(res, chgbl) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

}


# Summarize all the chemistry data
chem <- map_dfr(sort(unique(adlbc$PARAM)), test_summary, adlbc) %>%
  mutate(group = "CHEMISTRY")

# Summarize all the hematology data
hema <- map_dfr(sort(unique(adlbh$PARAM)), test_summary, adlbh) %>%
  mutate(group = "HEMATOLOGY")

# combine
labs_data <- bind_rows(chem, hema) %>%
  pivot_longer(c(n, contains("mean"), contains("sd")), names_to = "name", values_to = "value") %>%
  separate(name, c("param_val", "column"), sep= "_", remove = FALSE, fill = "right") %>%
  mutate(column = coalesce(column, name)) %>%
  select(group1 = group, group2 = PARAM, rowlbl = AVISIT, col1 = TRTP, col2 = column, param = param_val, value) %>%
  na.omit() %>%
  arrange(group1, group2, rowlbl) %>%
  mutate(ord1 = fct_inorder(group1) %>% as.numeric,
         ord2 = fct_inorder(group2) %>% as.numeric,
         ord3 = as.numeric(rowlbl)) %>%
  mutate(across(where(is.factor), as.character))

levels(labs_data$rowlbl) <- c("Bsln", "Wk 2", "Wk 4", "Wk 6", "Wk 8", "Wk 12",
                              "Wk 16", "Wk 20", "Wk 24", "Wk 26", "End[1]")


# save
usethis::use_data(labs_data, overwrite = TRUE)
