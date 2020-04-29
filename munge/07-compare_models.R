df <-
  df %>%
  mutate(
    P_SurgYear                = P_SurgYear - min(P_SurgYear),
    outcome_dislocation_90d_f = as.factor(outcome_dislocation_90d),
    outcome_infection_90d_f   = as.factor(outcome_infection_90d),
    outcome_dislocation_2y_f  = as.factor(outcome_dislocation_2y),
    outcome_infection_2y_f    = as.factor(outcome_infection_2y),
  )

# "unregularized least-square fit restricted to variables in J" /[@Bach2008]
form <- function(nms) {
  paste("outcome ~", paste(unique(gsub(
    "_TRUE.|_X[23]|_(Man|Kvinna|Male|Female|married|single|widow.widower|Idiopathic.necrosis|Inflammatory.joint.disease|Sequelae)", "",
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
  glm <-  glm(f, data = df, family = binomial())

  # I tried to use LASSO to avoid over-estimation but found that best penalty-terms were virtually 0 for all models.
  # I describe this as a sensitivity analysis, but I will not use it any further.
  lambda <- purrr::possibly(glmnet::cv.glmnet, list(lambda.min = 0))(
    model.matrix(as.formula(f), df)[, -1, drop = FALSE],
    df$outcome,
    family = "binomial"
  )$lambda.min
  lrm <-  rms::lrm(as.formula(f), data = df, x = TRUE, y = TRUE, penalty = lambda)

  list(glm = glm, lrm = lrm)
}


compare_models <- function(brlasso_coefs, outcome) {

  best_coefs         <- brlasso_coefs$best_coefs
  best_coefs_reduced <- brlasso_coefs$best_coefs_reduced

  all_models_tmp <-
    tribble(
      ~Model,                                            ~tab, ~fig,   ~fit,
      "BRL reduced (age as main effect)",                TRUE,  TRUE,  glmdf(form(best_coefs_reduced), outcome),
      "BRL reduced (age as RCS)",                        TRUE,  FALSE, glmdf(fns3(best_coefs_reduced), outcome),
      "BRL reduced without cancer (age as main effect)", TRUE,  FALSE, glmdf(form(setdiff(best_coefs_reduced, "c_cancer_TRUE.")), outcome),
      "BRL (age as main effect)",                        TRUE,  TRUE,  glmdf(form(best_coefs), outcome),
      "BRL (age as RCS)",                                TRUE,  FALSE, glmdf(fns3(best_coefs), outcome),
      "Charlson",                                        TRUE,  TRUE,  glmdf("outcome ~ CCI_index_quan_original", outcome),
      "Elixhauser",                                      TRUE,  TRUE,  glmdf("outcome ~ ECI_index_sum_all", outcome),
      "Rx Risk V",                                       TRUE,  TRUE,  glmdf("outcome ~ Rx_index_pratt", outcome),
      "ASA",                                             TRUE,  TRUE,  glmdf("outcome ~ P_ASA", outcome),
      "Age and sex (age as main effect)",                TRUE,  TRUE,  glmdf("outcome ~ P_Age + P_Sex", outcome),
      "Age and sex (age as RCS)",                        TRUE,  FALSE, glmdf("outcome ~ splines::ns(P_Age, 3) + P_Sex", outcome)
    ) %>%
    mutate(
      glm = map(fit, pluck, "glm"),
      lrm = map(fit, pluck, "lrm")
    ) %>%
    select(-fit)

  # Devide into two steps for easier debugging

  all_models <-
    all_models_tmp %>%
    mutate(
      tidy          = map(glm, broom::tidy, conf.int = TRUE, exponentiate = TRUE),
      AIC           = map_dbl(glm, AIC),
      pred          = map(glm, predict, type = "response"),
      obspred       = map(pred, ~ tibble(pred = ., obs = df[[outcome]])),
      ROC           = map(obspred, pROC::roc, "obs", "pred", levels = c(control = "FALSE", case = "TRUE"), direction = "<"),
      AUCci         = furrr::future_map(ROC, pROC::ci.auc, method = "bootstrap", boot.stratified = FALSE, .progress = TRUE),
      AUC_lo        = map_dbl(AUCci, 1),
      AUC_est       = map_dbl(AUCci, 2),
      AUC_hi        = map_dbl(AUCci, 3),
      roc_auc       = map(obspred, yardstick::roc_auc, obs, pred,   options = list(direction = ">")),
      roc_auc       = map_dbl(roc_auc, ".estimate"),
      roc_curve     = map(obspred, yardstick::roc_curve, obs, pred, options = list(direction = ">")),

      lrm_validate  = furrr::future_map(lrm, rms::validate, B = 100),
      lrm_calibrate = furrr::future_map(lrm, calibrate),
      optimism      = map_dbl(lrm_validate, ~ .["Dxy", "optimism"]),
      AUC_est.corr  = AUC_est - optimism,
      AUC_lo.corr   = AUC_lo  - optimism,
      AUC_hi.corr   = AUC_hi  - optimism,

      cal_belt = future_map(
        obspred, ~ givitiR::givitiCalibrationBelt(
          as.numeric(.$obs == "TRUE"), .$pred, devel = "internal")
                             ),
      cal_belt.p = map_dbl(cal_belt, "p.value")
    ) %>%
    select(-pred, -ROC, -AUCci)

  all_models
}


# Add models --------------------------------------------------------------

model_data <-
  model_data %>%
  mutate(
    outcome_var_f = paste0(outcome_var, "_f"),
    all_models = map2(coefs_selected, outcome_var_f, compare_models)
  )

# Not able to cache the usual way since object is too large
saveRDS(model_data, "cache/model_data.rds")


# Save only the GLM models for later use ----------------------------------

fit_brl <-
  model_data %>%
  select(outcome, time, all_models) %>%
  unnest("all_models") %>%
  filter(grepl("BRL (reduced )?\\(age as main effect\\)", Model)) %>%
  select(outcome, time, Model, glm)

cache("fit_brl")


# Save only the smalles possible parts for export -------------------------

fit_brl_reduced_lean <-
  fit_brl %>%
  filter(Model == "BRL (age as main effect)") %>%
  mutate(glm = map(glm, strip_glm))

cache("fit_brl_reduced_lean")
