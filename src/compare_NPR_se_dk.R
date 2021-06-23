suppressMessages({library(ProjectTemplate); load.project()})

load("cache/df_icd10_orig.RData")

# VI har mer komorbiditet i Sv jmfrt med DK.
# Enligt [@Schmidt2015]:
# > Since the adaption of the tenth revision of the International Classification
# of Diseases (ICD-10) in 1994, 23% of hospital contacts have had one or more
# secondary diagnoses recorded. The median number of secondary diagnoses per
# contact in this period was 1 (interquartile range: 1-2 diagnoses).

df_icd10 <-
  df_icd10_orig %>%
  mutate(hospital = as.Date(code_date, origin = "1970-01-01")) %>%
  select(LopNr, code, hospital) %>%
  as.data.table(key = c("LopNr", "code", "hospital"))

# Andel av de som har sjukhusvistelse som har fler än en diagnos
df_icd10[, .N, .(LopNr, hospital)][, .(
  prop   = mean(N > 1),
  median = median(N),
  q1     = quantile(N, .25),
  q3     = quantile(N, .75)
)]
# 47 % > 23 % DK
# median och IQR samma; 1, 1-2
