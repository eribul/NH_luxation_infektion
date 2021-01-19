suppressMessages({library(ProjectTemplate); load.project()})

load("cache/infection_data.RData")

# Baseline levels for cqategorical variables -----------------------------------
baseline <-
  crossing(
    time = factor(c("90 days", "2 years")),
    term = c(
      paste("Sex:", levels(df$P_Sex)[1]),
      paste("ASA class:", levels(df$P_ASA)[1]),
      paste("BMI:", levels(df$P_BMI)[1]),
      paste("Diagnosis:", levels(df$P_DiaGrp)[1])
    )
  ) %>%
  mutate(beta = 0, `OR 95 % CI` = "(baseline)", p = "")


# Prepare temp table ------------------------------------------------------

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
    term = gsub("Male sex", "Sex: Male", term),
    beta = log(estimate),
    `OR 95 % CI` = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    p    = pvalue_format(.01)(p.value)
  ) %>%
  bind_rows(baseline) %>%
  separate(term, c("variable", "level"), sep = ": ") %>%
  arrange(time, variable, `OR 95 % CI`) %>%
  mutate_at(vars(`OR 95 % CI`, p), ~ if_else(variable == "(Intercept)", "", .))


cache("tbl_coefs")


# Table to present in a nice way -----------------------------------------------

tbl_coefs_present <-
  tbl_coefs %>%
  group_by(time) %>%
  mutate(variable = replace(variable, duplicated(variable), "")) %>%
  ungroup() %>%
  mutate(
    time = as.character(time),
    time = replace(time, duplicated(time), "")
  )

cache("tbl_coefs_present")


# List covariates as text -------------------------------------------------

coefs_text <-
  tbl_coefs %>%
  filter(
    variable != "(Intercept)",
    !duplicated(tibble(variable, level)) # ONly unique factors for 2 years
  ) %>%
  mutate(fct = !is.na(level)) %>%
  select(time, fct, variable) %>%
  mutate(
    variable = tolower(variable),
    variable = case_when(
      variable == "bmi" ~ "body mass index (BMI)",
      variable == "diagnosis" ~ "the underlaying diagnosis for THA",
      variable == "sex" ~ "gender",
      variable == "asa class" ~ "American Society for Anesthesiologists (ASA) class",
      variable == "cns disease" ~ "CNS disease",
      variable == "pancreatiinsufficiency" ~ "pancreatic insufficiency",
      variable == "rheumatidisease" ~ "rheumatic disease",
      TRUE ~ variable
    )
  ) %>%
  add_count(time, variable, sort = TRUE) %>%
  distinct() %>%
  group_by(time, !fct) %>%
  summarise(
    text = if_else(
      !fct,
      paste("the precense of", glue::glue_collapse(variable, sep = ", ", last = " or ")),
      paste(variable, collapse = ", ")
    )
  ) %>%
  distinct(time, text) %>%
  group_by(time) %>%
  summarise(text = paste(text, collapse = ", and ")) %>%
  deframe()

cache("coefs_text")
