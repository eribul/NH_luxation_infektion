suppressMessages({library(ProjectTemplate); load.project()})

# Find coefs for 90-day model
# writeLines(paste(names(coef(fit_brl$fit[[1]])), collapse = " = c(, ),\n"))

load("cache/fit_brl.RData")

examples <-
  tibble(
    c_psoriasis      = c(FALSE,                     TRUE),
    P_BMI            = c("under/normal weight",     "overweight"),
    P_DiaGrp         = c("Primary osteoarthritis",  "Secondary osteoarthritis"),
    P_Sex            = c("Female",                  "Male"),
    c_cns_disease    = c(FALSE,                     TRUE),
    c_liver_disease  = c(FALSE,                     FALSE),

    P_ASA                         = c("I",     "III"),
    c_pancreatic_insufficiency    = c(FALSE, FALSE),
    c_drug_alcohol_abuse          = c(FALSE, FALSE),
    c_rheumatic_disease           = c(FALSE, FALSE),
    c_cancer                      = c(FALSE, FALSE),
    c_peptic_ulcer                = c(FALSE, FALSE),
    c_fluid_electrolyte_disorders = c(FALSE, FALSE),
    c_lung_airways_disease        = c(FALSE, FALSE)
  )

p <- predict(fit_brl$fit[[1]], newdata = examples, type = "response")

examples$p <- predict(fit_brl$fit[[1]], newdata = examples, type = "response")

examples <-
  examples %>%
  mutate(
    p = sprintf("%.1f %%", p * 100),
    desc = c(
      "a female with normal BMI, primary osteoarthritis and no co-morbidities",
      "a male with overweight, secondary osteoarthritis, psoriasis, dementia
       (CNS disease) and ASA class III"),
    desc = sprintf("%s would have a probability of %s", desc, p)
  )

cache("examples")
