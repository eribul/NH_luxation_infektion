suppressMessages({library(ProjectTemplate); load.project()})

# Table to present --------------------------------------------------------

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
    term = clean_names(term),
    beta = log(estimate),
    `OR 95 % CI` = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    p    = pvalue_format(.01)(p.value)
  ) %>%
  arrange(time) %>%
  mutate_at(vars(`OR 95 % CI`, p), ~ if_else(term == "(Intercept)", "", .))

cache("tbl_coefs")
