#' ---
#' title: Fitting Swedish hip replacement model to subsampled data
#' author: ute
#' date: 25.3.2021
#' ---
#'
#' Is the discrepancy between the coefficients of Eriks model, when fitted
#' to Danish data, perhaps just due to the smaller sample size?
#'
#' As a first experiment, we could look at the behaviour when fitted to
#' subsamples of the Swedish data set. The following code collects results
#' from 1000 subsamples and should not take long to run.
#'
#' It would be fantastic if you could share the results with me :-)
#'

# change here for your own data paths, and adjust names where necessary

# ugly but useful
if (!endsWith(getwd(), "validation"))
  setwd("validation")

load("../cache/model_reduced_lean.RData")
load("../cache/df.RData")
SE_data <- df

N_SE <- nrow(SE_data)
N_DK <- 18854L

nsubsamp <- 1000

if (!file.exists("swedish_subsample_fits.Rdata")) {
  result <- replicate(nsubsamp,
      unlist(
          glm(model_reduced_lean$formula,
              binomial,
              data = SE_data[sample(N_SE, N_DK, replace = FALSE), ]
          )[c("coefficients", "deviance", "aic")]
      )
  )

  save(result, file="swedish_subsample_fits.Rdata")
}

load("swedish_subsample_fits.Rdata")

summary(t(result))



#' # Compare model coefficients

suppressPackageStartupMessages(library(tidyverse))

load("model_dk_superlean.RData")

cfs <-
  left_join(
    enframe(coef(model_reduced_lean)),
    enframe(coef(model_dk_superlean)),
    suffix = c(".Sweden", ".Denmark"),
    by = "name"
  ) %>%
  rename(coef = name) %>%
  filter(!grepl("Intercept", coef)) %>%
  pivot_longer(starts_with("value"), names_to = "orig", names_prefix = "value.") %>%
  mutate(coef = clean_names(coef))


#' # Plot

#' Help function to plot the coefficients
plot_data <- function(results) {
  result %>%
    as_tibble(rownames = "coef") %>%
    filter(
      startsWith(coef, "coef"),
      !grepl("Intercept", coef),
    ) %>%
    pivot_longer(-coef) %>%
    filter(value > -5) %>%
    mutate(
      coef = gsub("coefficients\\.", "", coef),
      coef = clean_names(coef),
      coef = reorder(coef, value, mean)
    )
}
ci_data <- function(d) {
  d %>%
    group_by(coef) %>%
    summarise(lo = quantile(value, 0.025), hi = quantile(value, 0.975)) %>%
    pivot_longer(c(lo, hi))

}
plot_help <- function(d, ci, title) {
  d %>%
    ggplot(aes(coef, value, color = coef)) +
    geom_point(position = position_dodge2(1), alpha = .33, show.legend = FALSE) +
    geom_boxplot(alpha = .5, size = 1, show.legend = FALSE) +
    geom_point(aes(shape = orig), data = cfs, size = 5, alpha = 1, color = "red") +
    geom_point(data = ci, size = 5, alpha = 1, color = "black") +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "orange", size = 3) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = c(0, 1), legend.justification = c(0, 1))
    ggtitle(title)
}

plot_result <- function(result, title) {
  d  <- plot_data(result)
  ci <- ci_data(d)
  p  <- plot_help(d, ci, title)
  ggsave(paste0(title, ".png"), height = 20, width = 40, units = "cm")
  list(d = d, ci = ci, p = p)
}

#' ## Based on Swedish model

#' Note! This figure is to small. Se separate figure instead!
#' Red dots indicates the Swedish coefficients and red triangles the DK coefficients.
#' Small colored dots indicates Swedish coefficients based on smaller samples.
#' Large black dots indicates emperical lower and upper confidence bounds for a 95 % CI.
swedish_model <- plot_result(result, "Swedish_model")

#' **Comment:** There were some additional outliers far outside of the plot
#' Some of the coefficients are not different from 0 (no significant effect).
#' This was expected for ASA II, which is only included since ASA III is.
#' The difference between the Swedish and Danish estimates are relatively small.



# Compare to DK model (supplementary) -------------------------------------


emp_ci <-
  swedish_model$ci %>%
  group_by(coef) %>%
  summarise(coef_ci = sprintf("(%.2f, %.2f)", min(value), max(value))) %>%
  rename(term = coef) %>%
  separate(term, c("variable", "level"), sep = ": ") %>%
  arrange(variable)


load("cache/baseline.RData")
load("cache/tbl_coefs.RData")
reduced_dk_refitted <-
  model_dk_superlean %>%
  coef() %>%
  enframe() %>%
  transmute(
    term = clean_names(name),
    term = gsub("Male sex", "Sex: Male", term),
    beta = value,
  ) %>%
  bind_rows(select(baseline, term, beta)) %>%
  separate(term, c("variable", "level"), sep = ": ") %>%
  arrange(variable)

compare_se_dk_coefs <-
  tbl_coefs %>%
  select(variable, level, beta) %>%
  left_join(reduced_dk_refitted, c("variable", "level"), suffix = c(".se", ".dk")) %>%
  left_join(emp_ci,c("variable", "level")) %>%
  mutate(variable = replace(variable, duplicated(variable), "")) %>%
  rename(
    Swedish = beta.se,
    Danish = beta.dk,
    CI = coef_ci
  )
cache("compare_se_dk_coefs")



#' ## Based on Danish model

if (!file.exists("dk_swedish_subsample_fits.Rdata")) {
  result_dk <-
    replicate(
      nsubsamp,
      unlist(
        glm(model_dk_superlean$formula,
            binomial,
            data = SE_data[sample(N_SE, N_DK, replace = FALSE), ]
        )[c("coefficients", "deviance", "aic")]
      )
    )

  save(result_dk, file="dk_swedish_subsample_fits.Rdata")
}
load("dk_swedish_subsample_fits.Rdata")

summary(t(result_dk))
plot_result(result_dk, "Danish model")




#' # AIC and deviance

aic_dev <-
  tibble(
    orig     = rep(c("se", "dk"), each = nsubsamp),
    AIC      = c(result["aic",], result_dk["aic",]),
    deviance = c(result["deviance",], result_dk["deviance",])
  ) %>%
  pivot_longer(c(AIC, deviance))

ggplot(aic_dev, aes(value, fill = orig)) +
  geom_density(alpha = .5) +
  geom_vline(
    aes(xintercept = x),
    tibble(name = "AIC", x = model_dk_superlean$aic),
    size = 3, color = "red") +
  geom_vline(
    aes(xintercept = x),
    tibble(name = "deviance", x = model_dk_superlean$deviance),
    size = 3, color = "red") +
  facet_wrap(~ name) +
  theme_minimal() +
  theme(legend.position = "bottom")

#' AIC values from both models overlap.
#' The red line indicates the AIC from the Danish data.
#' Hence, the model works better with Danish data.
#' Significantly so? How many of the estimated values are at least as extreme
#' as the one observed (should thos be two-tailed?):
mean(sort(result["aic",]) <= model_dk_superlean$aic)



#' # Predictive power of the Danish model on the Swedish data
#'
#' We can try to use the model coefficients from the Danish data applied to the
#' whole Swedish data set and estimate predictive power the same way as was done
#' reversely before (as suggested by Ute).

set.seed(123)

#' Useful packages
purrr::walk(
  c("pROC", "rms", "givitiR"),
  ~suppressPackageStartupMessages(library(., character.only = TRUE))
)

#' use DK model with Swedish data
obspred <-
  tibble(
    obs  = df$outcome,
    pred = predict(model_dk_superlean, df, type = "response")
  )

#' ## ROC curve
ROC <- pROC::roc(obspred, "obs", "pred", direction = "<")
plot(ROC)

#' ## AUC
AUCci <-
  pROC::ci.auc(
    ROC,
    method          = "bootstrap",
    boot.n          = 100,     # Remove this argument and use the default!
    boot.stratified = FALSE
  )
AUCci

#' ## Calibration
calibration <-
  givitiR::givitiCalibrationBelt(
    as.numeric(obspred$obs),
    obspred$pred,
    devel = "external"
  )
plot(calibration, xlim = c(0, 0.45), ylim = c(0, 0.45))


#' ## Recalibrate intercept

Z <- predict(model_dk_superlean, df, type = "response")

# Refit the intercept using Z = a + Xb from above as offset
fit2 <- glm(df$outcome ~ 1, offset = Z)

# Same calibration and validation as above
obspred2     <- tibble(obs  = df$outcome, pred = predict(fit2, type = "response"))
ROC2         <- pROC::roc(obspred2, "obs", "pred", direction = "<")
plot(ROC2)
AUCci2 <-
  pROC::ci.auc(
    ROC2,
    method = "bootstrap",
    boot.n = 100,     # Remove this argument and use the default!
    boot.stratified = FALSE
  )
AUCci2

calibration2 <-
  givitiR::givitiCalibrationBelt(
    as.numeric(obspred2$obs),
    obspred2$pred,
    devel = "external"
  )
plot(calibration2, xlim = c(0, 0.45), ylim = c(0, 0.45))
#' No good!



#' ## Re-calibration of intercenpt and calibration slope

fit3         <- glm(df$outcome ~ 1 + Z)
obspred3     <- tibble(obs  = df$outcome, pred = predict(fit3, type = "response"))
ROC3         <- pROC::roc(obspred3, "obs", "pred", direction = "<")
plot(ROC3)

AUCci3 <-
  pROC::ci.auc(
    ROC3,
    method = "bootstrap",
    boot.n = 100,     # Remove this argument and use the default!
    boot.stratified = FALSE
  )
AUCci3

calibration3 <-
  givitiR::givitiCalibrationBelt(
    as.numeric(obspred3$obs),
    obspred3$pred,
    devel = "external"
  )
plot(calibration3, xlim = c(0, 0.45), ylim = c(0, 0.45))
