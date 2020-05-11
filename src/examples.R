suppressMessages({library(ProjectTemplate); load.project()})

examples <-
  tibble(
    c_psoriasis      = c(FALSE,                     TRUE),
    P_BMI            = c("under/normal weight",     "overweight"),
    P_DiaGrp         = c("Primary osteoarthritis",  "Secondary osteoarthritis"),
    P_Sex            = c("Female",                  "Male"),
    c_cns_disease    = c(FALSE,                     FALSE),
    c_liver_disease  = c(FALSE,                     FALSE),
  )

p <- predict(fit_brl$fit[[1]], newdata = examples, type = "response")

examples$p <- predict(fit_brl$fit[[1]], newdata = examples, type = "response")

examples <-
  examples %>%
  mutate(
    p = sprintf("%.1f %%", p * 100),
    desc = c(
      "a female of normal weigth with primary osteoarthritis and no co-morbidities",
      "a male with overweight, secondary osteoarthritis and proriasis"),
    desc = sprintf("%s would have a probability of %s", desc, p)
  )

cache("examples")
