df <-
  df %>%
  mutate(
    P_SurgYear                = P_SurgYear - min(P_SurgYear),
    outcome_infection_90d_f   = as.factor(outcome_infection_90d),
    outcome_infection_2y_f    = as.factor(outcome_infection_2y)
  )

# "unregularized least-square fit restricted to variables in J" /[@Bach2008]
form <- function(nms) {
  paste("outcome ~", paste(unique(gsub(paste0(
    "_TRUE.|_X[23]|_(I|II|III|Man|Kvinna|Male|Female|married|single|widow.widower|",
    "under.normal.weight|overweight|class.I.obesity|class.II.III.obesity|",
    "Idiopathic.necrosis|Inflammatory.joint.disease|Secondary.osteoarthritis)"), "",
    nms)), collapse = " + ")
  )
}

# Splines for age - Har testat med både 2 och 3 knots!
# Ingen skillnad så behåller med 3
fns3 <- function(coefs) {
  paste0(form(setdiff(coefs, "P_Age")), " + splines::ns(P_Age, 3)")
}


glmdf <- function(f, outcome){
  df$outcome <- df[[outcome]]
  glm(f, data = df, family = binomial())
}


compare_models <- function(brlasso_coefs, outcome) {

  best_coefs         <- brlasso_coefs$best_coefs
  best_coefs_reduced <- brlasso_coefs$best_coefs_reduced

  tribble(
    ~Model,         ~fit,
    "Reduced model",glmdf(form(best_coefs_reduced), outcome),
    "Main model",   glmdf(form(best_coefs), outcome),
    "Charlson",     glmdf("outcome ~ CCI_index_quan_original", outcome),
    "Elixhauser",   glmdf("outcome ~ ECI_index_sum_all", outcome),
    "Rx Risk V",    glmdf("outcome ~ Rx_index_pratt", outcome),
    "ASA class",    glmdf("outcome ~ P_ASA", outcome),
  ) %>%
  mutate(
    tidy          = map(fit, broom::tidy, conf.int = TRUE, exponentiate = TRUE),
    AIC           = map_dbl(fit, AIC),
    pred          = map(fit, predict, type = "response"),
    obspred       = map(pred, ~ tibble(pred = ., obs = df[[outcome]])),
    ROC           = map(obspred, pROC::roc, "obs", "pred", levels = c(control = "FALSE", case = "TRUE"), direction = "<"),
    AUCci         = furrr::future_map(ROC, pROC::ci.auc, method = "bootstrap", boot.stratified = FALSE, .progress = TRUE),
    AUC_lo        = map_dbl(AUCci, 1),
    AUC_est       = map_dbl(AUCci, 2),
    AUC_hi        = map_dbl(AUCci, 3),
    roc_auc       = map(obspred, yardstick::roc_auc, obs, pred,   options = list(direction = ">")),
    roc_auc       = map_dbl(roc_auc, ".estimate"),
    roc_curve     = map(obspred, yardstick::roc_curve, obs, pred, options = list(direction = ">"))
  ) %>%
  select(-pred, -ROC, -AUCci)
}


# Add models --------------------------------------------------------------

infection_data <-
  infection_data %>%
  mutate(
    outcome_var_f = paste0(outcome_var, "_f"),
    all_models    = map2(coefs_selected, outcome_var_f, compare_models)
  )

cache("infection_data")


# Save only the GLM models for later use ----------------------------------

fit_brl <-
  infection_data %>%
  select(outcome, time, all_models) %>%
  unnest("all_models") %>%
  filter(Model == "Reduced model") %>%
  select(outcome, time, Model, fit)

cache("fit_brl")


# Save only the smalles possible parts for export -------------------------

fit_brl_reduced_lean <-
  fit_brl %>%
  mutate(fit = map(fit, strip_glm))

cache("fit_brl_reduced_lean")
