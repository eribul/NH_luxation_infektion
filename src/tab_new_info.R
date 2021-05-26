suppressMessages({library(ProjectTemplate); load.project()})

load("cache/models.RData")
load("cache/df.RData")

# Table of added values according to Harrell:
# https://www.fharrell.com/post/addvalue/

se_mods_lrm <-
  models %>%
    filter(country == "se") %>%
    transmute(
      Model,
      `AUC (95 % CI)` = sprintf("%.2f (%.2f; %.2f)", AUC_est, AUC_lo, AUC_hi),
      lrm = map(fit, ~ lrm(formula(.), df)),
      R2  = map_dbl(lrm, ~ .$stats['R2'])
    )

# PLocka ut R2
base_R2 <-
  se_mods_lrm %>%
  filter(R2 == min(R2)) %>%
  select(R2) %>%
  pluck(1)

tab_new_info <-
  se_mods_lrm %>%
  mutate(
    FNI = map_dbl(R2, ~ 1 - base_R2 / .)
  ) %>%
  transmute(
    Model, `AUC (95 % CI)`, `R2 (%)` = R2 * 100, `FNI (%)` = FNI * 100)

cache("tab_new_info")
