suppressMessages({library(ProjectTemplate); load.project()})

load("cache/model_reduced.RData")
load("cache/df.RData")

# Hitta alla koefficientnamn som krävs för både 90 dagar och 2 år
model_vars <-
  model_reduced %>%
  coef() %>%
  names() %>%
  unique() %>%
  {.[-1]} %>%
  {gsub("TRUE|I{2,3}|Male", "", .)} %>%
  {gsub("(P_BMI)(.*)", "\\1", ., perl = TRUE)} %>%
  {gsub("(P_DiaGrp)(.*)", "\\1", ., perl = TRUE)} %>%
  unique()

# Export data frame of data needed for external validation
ext_val_required_data <-
  df %>%
  select(one_of(model_vars))

cache("ext_val_required_data")

