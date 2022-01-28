suppressMessages({library(ProjectTemplate); load.project()})

load("cache/model_reduced.RData")

# Find coefs for 90-day model
# writeLines(paste(names(coef(model_reduced)), collapse = " = c(, ),\n"))

examples <-
  tibble(
    c_psoriasis      = c(FALSE,                     TRUE),
    P_BMI            = c("under/normal weight",     "overweight"),
    P_DiaGrp         = c("Primary osteoarthritis",  "Secondary osteoarthritis"),
    P_Sex            = c("Female",                  "Male"),
    c_cns_disease    = c(FALSE,                     TRUE),
    c_liver_disease  = c(FALSE,                     FALSE),

    P_Age                         = c(60, 85),
    P_ASA                         = c("I",     "III"),
    c_pancreatic_insufficiency    = c(FALSE, FALSE),
    c_drug_alcohol_abuse          = c(FALSE, FALSE),
    c_rheumatic_disease           = c(FALSE, FALSE),
    c_cancer                      = c(FALSE, FALSE),
    c_peptic_ulcer                = c(FALSE, FALSE),
    c_fluid_electrolyte_disorders = c(FALSE, FALSE),
    c_arrhythmia                  = c(FALSE, FALSE),
    c_lung_airways_disease         = c(FALSE, FALSE)
  )

examples$p <- predict(model_reduced, newdata = examples, type = "response")

examples <-
  examples %>%
  mutate(
    p = sprintf("%.1f %%", p * 100),
    desc = c(
      "a 60-year-old female with normal BMI, primary osteoarthritis and without comorbidities",
      "an 85-year-old male with overweight, secondary osteoarthritis, psoriasis, dementia and an ASA class III")
  )

cache("examples")
