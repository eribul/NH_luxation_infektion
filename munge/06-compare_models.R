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

# Splines for age - Har testat med både 2 och 3 knots!
# Ingen skillnad så behåller med 3
fns3 <- function(coefs) {
  paste0(form(setdiff(coefs, "P_Age")), " + splines::ns(P_Age, 3)")
}


glmdf <- function(f){
  glm(f, data = dfm, family = binomial())
}

future::plan("multiprocess", workers = 2)

models <-
  tribble(
    ~Model,                    ~ fit,
    "Reduced model",           glmdf(form(coefs_selected$best_coefs_reduced)),
    "Main model",              glmdf(form(coefs_selected$best_coefs)),
    "Charlson",                glmdf("outcome ~ CCI_index_quan_original"),
    "Charlson + Age + Sex",    glmdf("outcome ~ CCI_index_quan_original + P_Age + P_Sex"),
    "Elixhauser",              glmdf("outcome ~ ECI_index_sum_all"),
    "Elixhauser + Age + Sex",  glmdf("outcome ~ ECI_index_sum_all + P_Age + P_Sex"),
    "Rx Risk",                 glmdf("outcome ~ Rx_index_pratt"),
    "Rx Risk + Age + Sex",     glmdf("outcome ~ Rx_index_pratt"),
    "ASA",                     glmdf("outcome ~ P_ASA"),
    "ASA + Age + Sex",         glmdf("outcome ~ P_Age + P_Sex + P_ASA"),
    "BMI + Age + Sex",         glmdf("outcome ~ P_Age + P_Sex + P_BMI"),
    "ASA + BMI + Age + Sex",   glmdf("outcome ~ P_Age + P_Sex + P_ASA + P_BMI")
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
                      method = "delong",
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

dk_models <-
  tribble(
    ~Model,                    ~AUC_est,   ~AUC_lo,   ~AUC_hi,
    "Reduced model",          0.6644978, 0.6383293, 0.6906663,
    "Reduced model (DK)",     0.6718784, 0.6458812, 0.6978755,
    "ASA",                    0.5827459, 0.5593311, 0.6061608,
    "ASA + Age + Sex",        0.6050408, 0.5795086, 0.6305730,
    "Elixhauser",             0.5199970, 0.4986380, 0.5413560,
    "Elixhauser + Age + Sex", 0.5730051, 0.5457002, 0.6003100,
    "Charlson",               0.5279087, 0.5083930, 0.5474245,
    "Charlson + Age + Sex",   0.5731801, 0.5460877, 0.6002724,
    "Rx Risk",                0.5791562, 0.5504993, 0.6078130,
    "Rx Risk + Age + Sex",    0.6023291, 0.5768198, 0.6278383,
    "BMI + Age + Sex",        0.6398137, 0.6126092, 0.6670181,
    "ASA + BMI + Age + Sex",  0.6464293, 0.6199900, 0.6728687
  )

models <-
  bind_rows(
    se = models,
    dk = dk_models,
    .id = "country"
  )

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
