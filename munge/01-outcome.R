
# Outcome from NPR --------------------------------------------------------
load("cache/df_kva_orig.RData")
load("cache/df_icd10_orig.RData")
load("cache/df_shpr.RData")
load("cache/df_reops.RData")

df_kva <-
  df_kva_orig %>%
  mutate(
    LopNr = as.character(LopNr),
    hospital = as.Date(code_date, origin = "1970-01-01")
    ) %>%
  select(LopNr, code, hospital) %>%
  as.data.table(key = c("LopNr", "code", "hospital"))

df_icd10 <-
  df_icd10_orig %>%
  mutate(hospital = as.Date(code_date, origin = "1970-01-01")) %>%
  select(LopNr, code, hospital) %>%
  as.data.table(key = c("LopNr", "code", "hospital"))

df_shpr <-
  df_shpr %>%
  mutate(LopNr = as.character(LopNr)) %>%
  as.data.table(key = c("LopNr", "P_Side", "P_SurgDate"))

ae_npr <- function(df_npr, regex, endday) {
  df_shpr %>%
    select(LopNr, P_SurgDate) %>%
    categorize(
      codedata = df_npr,
      cc = "hip_ae_hailer",
      id = "LopNr",
      code = "code",
      codify_args = list(
        date = "P_SurgDate",
        code_date = "hospital",
        days = c(0, endday)
      ),
      cc_args = list(
        regex = regex,
        tech_names = TRUE,
        stop = TRUE
      )
    )
}

outcome_kva <- ae_npr(df_kva,   "kva",   90)
outcome_icd <- ae_npr(df_icd10, "icd10", 90)

# Outcome from SHAR -------------------------------------------------------
outcome_reop <-
  df_reops %>%
  mutate_at(vars(P_SurgDate, R_SurgDate), as.Date) %>%
  filter(R_SurgDate - P_SurgDate < 90) %>%
  # Räcker med outcome vid en av ev flera reop under perioden.
  group_by(LopNr, P_SurgDate) %>%
  summarise(
    roep_infektion =
      any(IND_ReSurgReason1 == "Infektion") |
      any(IND_ReSurgReason2 == "Infektion")
  )

cache("outcome_reop")


# Combine -----------------------------------------------------------------


df_outcome <-
  as_tibble(outcome_icd) %>%
  left_join(as_tibble(outcome_kva), by = c("LopNr", "P_SurgDate")) %>%
  left_join(outcome_reop,           by = c("LopNr", "P_SurgDate")) %>%
  mutate(across(
    c(roep_infektion, starts_with("hip_ae_hailer")),
    coalesce, FALSE
    )
  ) %>%
  transmute(
    LopNr,
    P_SurgDate,
    outcome =
      roep_infektion |
      hip_ae_hailer_icd10_infection |
      hip_ae_hailer_kva_infection
  ) %>%
  mutate(outcome = coalesce(outcome, FALSE))

df_shpr <-
  df_shpr %>%
  as_tibble() %>%
  left_join(df_outcome, by = c("LopNr", "P_SurgDate"))

cache("df_shpr")





# Orsak till identifierad infektion ---------------------------------------

df_outcome_reasons <-
  as_tibble(outcome_icd) %>%
  left_join(as_tibble(outcome_kva), by = c("LopNr", "P_SurgDate")) %>%
  left_join(outcome_reop,           by = c("LopNr", "P_SurgDate")) %>%
  mutate(
    across(c(roep_infektion, starts_with("hip_ae_hailer")), coalesce, FALSE)
  ) %>%
  select(
    kva   = hip_ae_hailer_kva_infection,
    icd10 = hip_ae_hailer_icd10_infection,
    reop  = roep_infektion
  )

# Korstabulera orsakerna till varför vi identifierar en infektion
df_outcome_reasons %>%
  count(kva, icd10, reop, sort = TRUE)

# Vi ser att det finns många (1119) fall som identifieras via ICD-10-kod
# men som saknar reoperatilon och KVÅ-kod
# Kan vi se vilke ICD-10-koder som ligger bakom?
cd <-
  coder::codify(
    select(df_shpr, LopNr, P_SurgDate), df_icd10,
    id = "LopNr", code = "code", date = "P_SurgDate", code_date = "hospital",
    days = c(0, 90)
  )

# Kollar orsakerna till klassning som infektion
icd_reasons <-
  cd %>%
  filter(
    in_period,
    grepl(paste0("^(", coder::hip_ae_hailer$icd10[[1]], ")$"), code)
  ) %>%
  mutate(
    code_text = decoder::decode(code, decoder::icd10se)
  )

# OBS! Samma individ kan ha fler än en kod!
icd_reasons %>%
  distinct(LopNr, code, .keep_all = TRUE) %>%
  count(code, code_text, sort = TRUE) %>%
  as_tibble() %>%
  knitr::kable()

# OBS! Endast en kod per person (godtyckligt vald)
# Endast de som inte har reop eller KVÅ-kod
icd_reasons %>%
  distinct(LopNr, code, .keep_all = TRUE) %>%
  anti_join(outcome_reop) %>%
  anti_join(filter(outcome_kva, hip_ae_hailer_kva_infection)) %>%
  count(code, code_text, sort = TRUE) %>%
  as_tibble() %>%
  knitr::kable()
