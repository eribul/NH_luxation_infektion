
# Outcome from NPR --------------------------------------------------------

df_kva <-
  df_kva_orig %>%
  mutate(
    id = as.character(LopNr),
    code_date = as.Date(code_date, origin = "1970-01-01")
    ) %>%
  select(id, code, code_date) %>%
  as.codedata()

df_icd10 <-
  df_icd10_orig %>%
  mutate(code_date = as.Date(code_date, origin = "1970-01-01")) %>%
  select(id = LopNr, code, code_date) %>%
  as.codedata()

ae_npr <- function(df_npr, regex, endday) {
  df_shpr %>%
    select(LopNr, P_SurgDate) %>%
    mutate(LopNr = as.character(LopNr)) %>%
    categorize(
      df_npr,
      cc = "hip_ae_hailer",
      id = "LopNr",
      codify_args = list(
        date = "P_SurgDate",
        days = c(0, endday)
      ),
      cc_args = list(
        regex = regex,
        tech_names = TRUE,
        stop = TRUE
      )
    )
}

outcome_kva_90d <- ae_npr(df_kva,   "regex_kva",   90)
outcome_icd_90d <- ae_npr(df_icd10, "regex_icd10", 90)
outcome_kva_2y  <- ae_npr(df_kva,   "regex_kva",   2 * 365)
outcome_icd_2y  <- ae_npr(df_icd10, "regex_icd10", 2 * 365)

# Outcome from SHAR -------------------------------------------------------
outcome_reop <- function(endday) {
  df_reops %>%
    mutate_at(vars(P_SurgDate, R_SurgDate), as.Date) %>%
    filter(R_SurgDate - P_SurgDate < endday) %>%
    # Räcker med outcome vid en av ev flera reop under perioden.
    group_by(LopNr, P_SurgDate) %>%
    summarise(
      reop_luxation =
        any(IND_ReSurgReason1 == "Luxation, instabilitet, subluxation") |
        any(IND_ReSurgReason2 == "Luxation, instabilitet, subluxation"),
      roep_infektion =
        any(IND_ReSurgReason1 == "Infektion") |
        any(IND_ReSurgReason2 == "Infektion")
    )
}

outcome_reop_90d <- outcome_reop(90)
outcome_reop_2y  <- outcome_reop(2 * 356.241)


# Combine -----------------------------------------------------------------


df_outcome <- function(outcome_icd10, outcome_kva, outcome_reop) {
  as_tibble(outcome_icd10) %>%
    left_join(as_tibble(outcome_kva), by = c("LopNr", "P_SurgDate")) %>%
    left_join(outcome_reop,           by = c("LopNr", "P_SurgDate")) %>%
    mutate(across(
      c(roep_infektion, reop_luxation, starts_with("hip_ae_hailer_regex")),
      coalesce, FALSE
      )
    ) %>%
    mutate(
      LopNr = as.numeric(LopNr),
      P_SurgDate,
      outcome_infection =
        roep_infektion |
        hip_ae_hailer_regex_icd10_infection |
        hip_ae_hailer_regex_kva_infection,
      outcome_dislocation =
        reop_luxation |
        hip_ae_hailer_regex_icd10_dislocation |
        hip_ae_hailer_regex_kva_dislocation
    ) %>%
    mutate_at(vars(starts_with("outcome_")), coalesce, FALSE)
}

df_outcome_90d <- df_outcome(outcome_icd_90d, outcome_kva_90d, outcome_reop_90d)
df_outcome_2y  <- df_outcome(outcome_icd_2y, outcome_kva_2y, outcome_reop_2y)

df_shpr <-
  df_shpr %>%
  left_join(df_outcome_90d, c("LopNr", "P_SurgDate")) %>%
  left_join(df_outcome_2y, c("LopNr", "P_SurgDate"), suffix = c("_90d", "_2y"))

cache("df_shpr")
