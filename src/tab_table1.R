suppressMessages({library(ProjectTemplate); load.project()})

dft1 <-
  df %>%
  mutate(
    Charlson   = replace(CCI_index_quan_original, CCI_index_quan_original == 4, "4+"),
    Elixhauser = replace(ECI_index_sum_all, ECI_index_sum_all == 3, "3+"),
    RxRiskV    = Rx_index_pratt
  ) %>%
  select(
    `Infection within two years` = outcome_infection_2y,
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
  setNames(clean_names(names(.))) %>%
  rename(`ASA class` = ASA)

  fct <- c("Sex", "ASA class", "Elixhauser",
           "Charlson", "Diagnos", "Cemented stem", "Cemented cup")

  t1 <-
  tableone::CreateTableOne(
    strata = "Infection within two years",
    vars = setdiff(names(dft1), "Infection within two years"),
    factorVars = fct,
    data = dft1,
    test = FALSE
  )

t1_all <-
  tableone::CreateTableOne(
    vars = setdiff(names(dft1), "Infection within two years"),
    factorVars = fct,
    data = dft1,
    test = FALSE
  ) %>%
  print(
    printToggle = FALSE
  ) %>%
  as_tibble()

table1 <-
  t1 %>%
  print(
    printToggle = FALSE
  ) %>%
  as_tibble(rownames = "what") %>%
  add_column(Total = t1_all$Overall) %>%
  mutate(
    level = trimws(ifelse(startsWith(what, " "), what, "")),
    level = paste0(toupper(substr(level, 1, 1)), substring(level, 2)),
    what = trimws(ifelse(level == "", gsub(" = TRUE", "", what), ""))
  ) %>%
  mutate_at(vars(`TRUE`, `FALSE`), zero) %>%
  select(what, level, `TRUE`, `FALSE`, Total)


cache("table1")
