suppressMessages({library(ProjectTemplate); load.project()})


# Prepare data ------------------------------------------------------------
tab1 <- function(df) {
  dft1 <-
    df %>%
    mutate(
      Charlson   = replace(CCI_index_quan_original, CCI_index_quan_original == 4, "4+"),
      Elixhauser = replace(ECI_index_sum_all, ECI_index_sum_all == 3, "3+"),
      RxRiskV    = Rx_index_pratt
    ) %>%
    select(
      outcome,
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
      strata = "Outcome",
      vars = setdiff(names(dft1), "Outcome"),
      factorVars = fct,
      data = dft1,
      test = FALSE
    )

  t1_all <-
    tableone::CreateTableOne(
      vars = setdiff(names(dft1), "Outcome"),
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

  list(t1 = t1_all, table1 = table1)
}

table1_dislocation <-
  df %>%
  rename(outcome = outcome_dislocation_2y) %>%
  select(-starts_with("outcome_infection")) %>%
  tab1()

table1_infection <-
  df %>%
  rename(outcome = outcome_infection_2y) %>%
  select(-starts_with("outcome_dislocation")) %>%
  tab1()

cache("table1_dislocation")
cache("table1_infection")
