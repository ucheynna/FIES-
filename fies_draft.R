# library(tidyverse)
# library(RM.weights)

df <- read.csv("s08a_me_sen2021.csv") %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, s08aq01, s08aq02, s08aq03, s08aq04, s08aq05, s08aq06, s08aq07, s08aq08) %>%
  rename(
    WORRIED = s08aq01,
    HEALTHY = s08aq02,
    FEWFOOD = s08aq03,
    SKIPPED = s08aq04,
    ATELESS = s08aq05,
    RUNOUT  = s08aq06,
    HUNGRY  = s08aq07,
    WHLDAY  = s08aq08
  ) %>%
  mutate(across(
    WORRIED:WHLDAY,
    ~ case_when(.x == 1 ~ 1,
                .x == 2 ~ 0,
                TRUE ~ NA_real_)
  )) %>%
  drop_na(WORRIED:WHLDAY)   # exclude rows with values not 1 or 2

sen_hh <- read.csv("sen_ehcvm2122_hh_info.csv") %>%
  mutate(iso3 = "SEN") %>%
  select(hhid, iso3, survey_wgt)

sen_country <- df %>%
  left_join(sen_hh, by = "hhid")


XX <- sen_country %>% select(2:9)
wt <- sen_country$survey_wgt

rr <- RM.w(XX, wt)

sen_country$raw_score <- rowSums(XX, na.rm = TRUE)

hh_results <- sen_country %>%
  mutate(
    severity = rr$a[raw_score + 1],
    severity_se = rr$se.a[raw_score + 1],
    prob_insecure = 1 / (1 + exp(-severity))
  )

hh_results_sen <- hh_results %>%
  mutate(
    fies_cat = case_when(
      severity < rr$b[["ATELESS"]] ~ "Mild/None",
      severity >= rr$b[["ATELESS"]] & severity < rr$b[["WHLDAY"]] ~ "Moderate",
      severity >= rr$b[["WHLDAY"]] ~ "Severe"
    )
  )


b_fao <- c(
  WORRIED = -1.22,
  HEALTHY = -0.85,
  FEWFOOD = -1.11,
  SKIPPED = 0.35,
  ATELESS = -0.31,
  RUNOUT  = 0.51,
  HUNGRY  = 1.75,
  WHLDAY  = 1.88
)

theta_survey <- hh_results$severity
theta_fao_mean <- mean(rr$b)         # FAO mean difficulty
theta_fao_sd   <- sd(rr$b)           # FAO SD (or use official FAO values)

theta_survey_mean <- mean(theta_survey)
theta_survey_sd   <- sd(theta_survey)

theta_equated <- (theta_survey - theta_survey_mean) / theta_survey_sd * theta_fao_sd + theta_fao_mean

hh_results <- hh_results %>%
  mutate(severity_equated = theta_equated)

hh_results <- hh_results %>%
  mutate(
    fies_cat_equated = case_when(
      severity_equated < b_fao["ATELESS"] ~ "Mild/None",
      severity_equated >= b_fao["ATELESS"] & severity_equated < b_fao["WHLDAY"] ~ "Moderate",
      severity_equated >= b_fao["WHLDAY"] ~ "Severe"
    )
  )

hh_results %>%
  count(fies_cat_equated) %>%
  mutate(prevalence = n / sum(n) * 100)