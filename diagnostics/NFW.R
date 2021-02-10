# Q: Identify wether NOMESCO codes NFW[56]9 would make any difference in the inclusion list.

library(ProjectTemplate)
load.project()

load("cache_T846F/df_icd10_orig.RData")
load("cache_T846F/df_shpr.RData")
load("cache_T846F/df.RData")

df_kva <-
  df_kva_orig %>%
  mutate(
    LopNr = as.character(LopNr),
    hospital = as.Date(code_date, origin = "1970-01-01")
    ) %>%
  select(LopNr, code, hospital) %>%
  as.data.table(key = c("LopNr", "code", "hospital"))

df_shpr <- as.data.table(df_shpr)[, .(LopNr, P_SurgDate, outcome_infection_90d)]

# Identifiera fall med NFW[56]9
nfw <-
  df_kva[
    J(df_shpr), on = "LopNr"][
      hospital > P_SurgDate &
      hospital - 90 < P_SurgDate &
      grepl("NFW[56]9", code) &
      !outcome_infection_90d]

# A: No, it does nt matter!
uniqueN(nfw$LopNr) # 2
