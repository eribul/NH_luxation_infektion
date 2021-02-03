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
    PJI = outcome,
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

t1 <-
  tableone::CreateTableOne(
    strata = "PJI",
    vars = setdiff(names(dft1), "PJI"),
    factorVars = fct,
    data = dft1,
    test = FALSE
  ) %>% print(
    printToggle = FALSE
  ) %>%
  as_tibble(rownames = "what") %>%
  rename(
    `PJI` = `TRUE`,
    `No PJI` = `FALSE`
  )


t1_tot <-
  tableone::CreateTableOne(
    vars = setdiff(names(dft1), "PJI"),
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
    t1,
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
    PJI,
    `No PJI`,
    Total
  )


cache("table1")
