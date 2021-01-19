suppressMessages({library(ProjectTemplate); load.project()})

load("cache/df.RData")

dft1 <-
  df %>%
  mutate(
    Charlson   = replace(CCI_index_quan_original, CCI_index_quan_original == 4, "4+"),
    Elixhauser = replace(ECI_index_sum_all, ECI_index_sum_all == 3, "3+"),
    RxRiskV    = Rx_index_pratt,
    P_BMI      = relevel(P_BMI, "under/normal weight")
  ) %>%
  select(
    `PJI within two years` = outcome_infection_2y,
    `PJI within 90 days` = outcome_infection_90d,
    P_Age,
    P_Sex,
    P_BMI,
    P_ASA,
    P_DiaGrp,
    cemented_stem,
    cemented_cup,
    P_TypeOfHospital,
    education,
    civil_status,
    Charlson,
    Elixhauser,
    RxRiskV,
    starts_with("c_"),
  ) %>%
  setNames(gsub(": ", "", clean_names(names(.))))

  fct <- c("Sex", "ASA class", "Elixhauser",
           "Charlson", "Diagnosis", "Cemented stem", "Cemented cup")

t1_2y <-
  tableone::CreateTableOne(
    strata = "PJI within two years",
    vars = setdiff(names(dft1), "PJI within two years"),
    factorVars = fct,
    data = select(dft1, -`PJI within 90 days`),
    test = FALSE
  ) %>% print(
    printToggle = FALSE
  ) %>%
  as_tibble(rownames = "what") %>%
  rename(
    `PJI < 2 years` = `TRUE`,
    `No PJI < 2 years` = `FALSE`
  )

t1_90d <-
  tableone::CreateTableOne(
    strata = "PJI within 90 days",
    vars = setdiff(names(dft1), "PJI within 90 days"),
    factorVars = fct,
    data = select(dft1, -`PJI within two years`),
    test = FALSE
  ) %>%
  print(
    printToggle = FALSE
  ) %>%
  as_tibble() %>%
  rename(
    `PJI < 90 days` = `TRUE`,
    `No PJI < 90 days` = `FALSE`
  )

t1_tot <-
  tableone::CreateTableOne(
    vars = setdiff(names(dft1), c("PJI within two years", "PJI within 90 days")),
    factorVars = fct,
    data = dft1,
    test = FALSE
  ) %>%
  print(
    printToggle = FALSE
  ) %>%
  as_tibble() %>%
  rename(Total = Overall)


table1 <-
  bind_cols(
    t1_2y,
    t1_90d,
    t1_tot
  ) %>%
  mutate(
    level = trimws(ifelse(startsWith(what, " "), what, "")),
    level = paste0(toupper(substr(level, 1, 1)), substring(level, 2)),
    what = trimws(ifelse(level == "", gsub(" = TRUE", "", what), ""))
  ) %>%
  mutate_at(vars(-what, -level), zero) %>%
  select(
    what,
    level,
    `PJI < 90 days`, `No PJI < 90 days`,
    `PJI < 2 years`, `No PJI < 2 years`,
    Total
  )


cache("table1")
