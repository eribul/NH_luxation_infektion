suppressMessages({library(ProjectTemplate); load.project()})

# Table to present --------------------------------------------------------

baseline <-
  crossing(
    time = factor(c("90 days", "2 years")),
    term = c(
      paste("Sex:", levels(df$P_Sex)[1]),
      paste("BMI:", levels(df$P_BMI)[1]),
      paste("Diagnosis:", levels(df$P_DiaGrp)[1])
    )
  ) %>%
  mutate(beta = 0, `OR 95 % CI` = "(baseline)", p = "")



tbl_coefs <-
  infection_data %>%
  unnest("all_models") %>%
  filter(
    Model == "Reduced model"
  ) %>%
  select(time, tidy) %>%
  unnest(tidy) %>%
  transmute(
    time = factor(time, c("90d", "2y"), c("90 days", "2 years")),
    term = gsub("Male sex", "Sex: Male", term),
    term = clean_names(term),
    beta = log(estimate),
    `OR 95 % CI` = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    p    = pvalue_format(.01)(p.value)
  ) %>%
  bind_rows(baseline) %>%
  separate(term, c("variable", "level"), sep = ": ") %>%
  arrange(time, variable, `OR 95 % CI`) %>%
  mutate_at(vars(`OR 95 % CI`, p), ~ if_else(variable == "(Intercept)", "", .)) %>%
  group_by(time) %>%
  mutate(variable = replace(variable, duplicated(variable), "")) %>%
  ungroup() %>%
  mutate(
    time = as.character(time),
    time = replace(time, duplicated(time), "")
  )

cache("tbl_coefs")
