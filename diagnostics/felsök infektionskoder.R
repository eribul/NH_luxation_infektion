library(ProjectTemplate)
load.project()

df_icd10 <-
  df_icd10_orig %>%
  mutate(hospital = as.Date(code_date, origin = "1970-01-01")) %>%
  select(LopNr, code, hospital) %>%
  as.data.table(key = c("LopNr", "code", "hospital"))

df_kva <-
  df_kva_orig %>%
  mutate(
    LopNr = as.character(LopNr),
    hospital = as.Date(code_date, origin = "1970-01-01")
  ) %>%
  select(LopNr, code, hospital) %>%
  as.data.table(key = c("LopNr", "code", "hospital"))






# FIltrera ut koder som identifierar infektion enl ICD-10
inf <- df_icd10[
  grepl(set_classcodes(hip_ae_hailer, stop = TRUE)$icd10[[1]], code)
]

inf_kva <- df_kva[
  grepl("^(NFS[0-9]{0,2}|NFA12|TNF(05|10))$", code)
]

# Hitta pat som identifieras via reoperation
reops <-
  outcome_reop_90d %>%
  filter(roep_infektion) %>%
  select(LopNr, P_SurgDate, reop = roep_infektion)

# Identifiera patienter av intresse
pats <-
  df_shpr %>%
  transmute(
    LopNr = as.character(LopNr),
    P_SurgDate
  )


# Filtrera fram infektioner inom 90 dagar enligt ICD-10
cd <-
  codify(
    pats, inf,
    id = "LopNr", code = "code", date = "P_SurgDate",
    code_date = "hospital", days = c(1, 90)
  ) %>%
  filter(in_period) %>%
  distinct(LopNr, P_SurgDate, code)

# Motsv enl KVA
cd_kva <-
  codify(
    pats, inf_kva,
    id = "LopNr", code = "code", date = "P_SurgDate",
    code_date = "hospital", days = c(1, 90)
  ) %>%
  filter(in_period) %>%
  distinct(LopNr, P_SurgDate, code)

# Kolla vilka som endast blir identifierade via ICD och på inget annat sätt
cd2 <-
  cd %>%
  anti_join(cd_kva, c("LopNr", "P_SurgDate")) %>%
  as_tibble() %>%
  mutate(P_SurgDate = as.Date(P_SurgDate)) %>%
  anti_join(reops, c("LopNr", "P_SurgDate"))

# Kolla vilka de vanligaste koderna är
count(cd2, code, sort = TRUE) %>%
  as_tibble() %>%
  mutate(
    p = round(n / sum(n) * 100), # andel (procent) med resp kod
    desc = decoder::decode(code, decoder::icd10se)
  )



# Saknade T846F ----------------------------------------------------------

# NH har upptäckt att vi nog även borde ha inkluderat T846F (tidig djup infektion)
# Vi bör kolla hur vanligt det är att dena kod förekommer utan reop

T846F <- df_icd10[
  grepl("T846F", code)
]

# inom 90 dagar
cd_T846F <-
  codify(
    pats, T846F,
    id = "LopNr", code = "code", date = "P_SurgDate",
    code_date = "hospital", days = c(1, 90)
  ) %>%
  filter(in_period) %>%
  distinct(LopNr, P_SurgDate, code)

# nrow(cd_T846F) # 142 st

# Hur många av dessa saknar reoperation?

cd2_T846F <-
  cd_T846F %>%
  as_tibble() %>%
  mutate(P_SurgDate = as.Date(P_SurgDate)) %>%
  anti_join(reops, c("LopNr", "P_SurgDate"))

# nrow(cd2_T846F) # 85
