# df model data
dfm <-
  df %>%
  mutate(
    P_SurgYear = P_SurgYear - min(P_SurgYear),
    outcome_f  = as.factor(outcome)
  )

# "unregularized least-square fit restricted to variables in J" /[@Bach2008]
form <- function(nms) {
  paste("outcome ~", paste(unique(gsub(paste0(
    "_TRUE.|_X[23]|_(I|II|III|Man|Kvinna|Male|Female|married|single|widow.widower|",
    "University|County|Rural|Provate",
    "under.normal.weight|overweight|class.I.obesity|class.II.III.obesity|",
    "Avascular.necrosis.of.the.femoral.head..AVN.|Inflammatory.joint.disease|",
    "Secondary.osteoarthritis|Sequelae.after.childhood.hip.disease)"), "",
    nms)), collapse = " + ")
  )
}

# Splines for age - Har testat med b�de 2 och 3 knots!
# Ingen skillnad s� beh�ller med 3
fns3 <- function(coefs) {
  paste0(form(setdiff(coefs, "P_Age")), " + splines::ns(P_Age, 3)")
}


glmdf <- function(f){
  glm(f, data = dfm, family = binomial())
}

future::plan("multiprocess", workers = 2)

models <-
  tribble(
    ~Model,         ~fit,
    "Reduced model",glmdf(form(coefs_selected$best_coefs_reduced)),
    "Main model",   glmdf(form(coefs_selected$best_coefs)),
    "Charlson",     glmdf("outcome ~ CCI_index_quan_original"),
    "Elixhauser",   glmdf("outcome ~ ECI_index_sum_all"),
    "Rx Risk V",    glmdf("outcome ~ Rx_index_pratt"),
    "ASA class",    glmdf("outcome ~ P_ASA"),
  ) %>%
  mutate(
    tidy          = map(fit, broom::tidy, conf.int = TRUE, exponentiate = TRUE),
    AIC           = map_dbl(fit, AIC),
    pred          = map(fit, predict, type = "response"),
    obspred       = map(pred, ~ tibble(pred = ., obs = dfm$outcome_f)),
    ROC           = map(obspred, pROC::roc, "obs", "pred",
                        levels = c(control = "FALSE", case = "TRUE"),
                        direction = "<"),
    AUCci         = furrr::future_map(
                      ROC,
                      pROC::ci.auc,
                      method = "bootstrap",
                      boot.stratified = FALSE,
                      .progress = TRUE,
                      .options = furrr_options(seed = TRUE)
                    ),
    AUC_lo        = map_dbl(AUCci, 1),
    AUC_est       = map_dbl(AUCci, 2),
    AUC_hi        = map_dbl(AUCci, 3),
    roc_curve     = map(obspred, yardstick::roc_curve, obs, pred,
                        event_level = "second")
  ) %>%
  select(-pred, -ROC, -AUCci)

cache("models")


# Save only the GLM models for later use ----------------------------------

model_reduced <-
  models %>%
  filter(Model == "Reduced model") %>%
  select(fit) %>%
  pluck(1, 1)

cache("model_reduced")


# Save only the smalles possible parts for export -------------------------

model_reduced_lean <-
  model_reduced %>%
  strip_glm()

cache("model_reduced_lean")