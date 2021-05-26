suppressMessages({library(ProjectTemplate); load.project()})

load("cache/model_reduced.RData")
load("cache/df.RData")
load("cache/models.RData")


# Baseline levels for cqategorical variables -----------------------------------
baseline <-
  crossing(
    term = c(
      paste("Sex:", levels(df$P_Sex)[1]),
      paste("ASA class:", levels(df$P_ASA)[1]),
      paste("BMI:", levels(df$P_BMI)[1]),
      paste("Diagnosis:", levels(df$P_DiaGrp)[1])
    )
  ) %>%
  mutate(beta = 0, `OR 95 % CI` = "(ref)", p = "")
cache("baseline")


# Prepare temp table ------------------------------------------------------

tbl_coefs <-
  models %>%
  filter(
    Model == "Reduced model"
  ) %>%
  select(tidy) %>%
  unnest(tidy) %>%
  transmute(
    term = clean_names(term),
    term = gsub("Male sex", "Sex: Male", term),
    beta = log(estimate),
    `OR 95 % CI` = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    p    = pvalue_format(.001)(p.value)
  ) %>%
  bind_rows(baseline) %>%
  separate(term, c("variable", "level"), sep = ": ") %>%
  arrange(variable, `OR 95 % CI`) %>%
  mutate_at(vars(`OR 95 % CI`, p), ~ if_else(variable == "(Intercept)", "", .))


cache("tbl_coefs")


# Table to present in a nice way -----------------------------------------------

tbl_coefs_present <-
  tbl_coefs %>%
  mutate(variable = replace(variable, duplicated(variable), "")) %>%
  ungroup()

cache("tbl_coefs_present")


# List covariates as text -------------------------------------------------

coefs_text <-
  tbl_coefs %>%
  filter(
    variable != "(Intercept)",
    !duplicated(tibble(variable, level)) # ONly unique factors for 2 years
  ) %>%
  mutate(fct = !is.na(level)) %>%
  select(fct, variable) %>%
  mutate(
    variable = tolower(variable),
    variable = case_when(
      variable == "bmi" ~ "body mass index (BMI)",
      variable == "diagnosis" ~ "the underlaying diagnosis for THA",
      # variable == "sex" ~ "gender",
      variable == "asa class" ~ "American Society for Anesthesiologists (ASA) class",
      variable == "cns disease" ~ "CNS disease",
      variable == "pancreatiinsufficiency" ~ "pancreatic insufficiency",
      variable == "rheumatidisease" ~ "rheumatic disease",
      variable == "lung airways disease" ~ "lung and airways disease",
      TRUE ~ variable
    ),
    ordinary = fct | variable == "age"
  ) %>%
  add_count(variable, sort = TRUE) %>%
  distinct() %>%
  group_by(ordinary) %>%
  summarise(
    text = if_else(
      !ordinary,
      paste("the presence of", glue::glue_collapse(variable, sep = ", ", last = " or ")),
      paste(variable, collapse = ", ")
    )
  ) %>%
  distinct(text) %>%
  ungroup() %>%
  arrange(!ordinary) %>%
  summarise(text = paste(text, collapse = ", and ")) %>%
  deframe()

cache("coefs_text")


