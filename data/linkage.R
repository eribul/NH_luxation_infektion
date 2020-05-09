lisa <- tbl(con, "lisa_1yr_before") %>%
  select(LopNr, P_Side, civil_status, education)

sql_shpr_orig <-
  tbl(con, "primary") %>%
  select(
    LopNr,
    P_Side,
    DateOfDeath,
    P_TypeOfHospital,
    P_ProstType,
    P_SurgDate,
    P_Gender,
    P_ASA,
    P_BMI,
    P_Age,
    P_DiaGrp,
    P_KVA1,
    P_AcetCupCemMix,
    P_FemStemCemMix
  ) %>%
  # NOTE! from 2006 to identify if second THA within two years (which should be exluded)
  filter(between(P_SurgDate, "2006-01-01", "2015-12-31")) %>%
  left_join(tbl(con, "operations_factors_opnr"),  c("LopNr", "P_Side")) %>%
  left_join(tbl(con, "elix_1yr_before"),          c("LopNr", "P_Side")) %>%
  left_join(tbl(con, "charlson_1yr_before"),      c("LopNr", "P_Side")) %>%
  left_join(tbl(con, "rxriskv_pratt_1yr_before"), c("LopNr", "P_Side")) %>%
  left_join(lisa, c("LopNr", "P_Side"))

df_shpr_orig <-
  sql_shpr_orig %>%
  collect() %>%
  mutate_if(is.character, na_if, "NA") %>%
  mutate_at(vars(DateOfDeath, P_SurgDate), as.Date, format = "%Y-%m-%d") %>%
  mutate_at(vars(LopNr, P_ASA, P_Age), as.integer) %>%
  mutate_at(vars(P_BMI), as.numeric) %>%
  mutate_at(vars(matches("elix|charlson|rxriskv"), -contains("index")), as.logical) %>%
  mutate_if(is.character, as.factor)

cache("df_shpr_orig")



# Reoperation -------------------------------------------------------------

# Needs reoperation if reason is dislocation or infection
# Should also filter on time (< 2 years from primary)
df_reops <-
  sql_shpr_orig %>%
  inner_join(tbl(con, "reoperations"), c("LopNr", "P_Side")) %>%
  filter(
    IND_ReSurgReason1 %in% c("Infektion", "Luxation, instabilitet, subluxation") |
    IND_ReSurgReason2 %in% c("Infektion", "Luxation, instabilitet, subluxation")
  ) %>%
  select(LopNr, P_Side, P_SurgDate, R_SurgDate, IND_ReSurgReason1, IND_ReSurgReason2) %>%
  collect()

cache("df_reops")


# Get all KVA- and ICD-10-data for all patients (later used to identify AE) ----
df_kva_orig <-
  sql_shpr_orig %>%
  select(LopNr, P_Side, P_SurgDate) %>%
  left_join(tbl(con, "par_kva_utdatum"), c("LopNr" = "id")) %>%
  collect() %>%
  mutate(
    P_SurgDate = as.Date(P_SurgDate)
  )
cache("df_kva_orig")

df_icd10_orig <-
  sql_shpr_orig %>%
  select(LopNr, P_Side, P_SurgDate) %>%
  left_join(tbl(con, "par_icd_indatum"), c("LopNr" = "id")) %>%
  collect()

cache("df_icd10_orig")
