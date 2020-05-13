# Add important variables to data

df <-
  df_shpr %>%
  mutate_if(is.logical, coalesce, FALSE) %>%
  mutate_at(vars(contains("index")), coalesce, 0) %>%
  mutate(
    P_SurgYear = as.numeric(substr(P_SurgDate, 1, 4)),
    P_ASA = factor(P_ASA, 1:3, c("I", "II", "III")),

    # Civilstånd
    civil_status = as.character(civil_status),
    civil_status =
      ifelse(
        civil_status %in% c("divorced", "unmarried"),
        "single", civil_status) %>%
      as.factor() %>% relevel("married"),

    education =
      factor(education, levels(education), c("low", "middle", "high")),
    P_TypeOfHospital =
      factor(P_TypeOfHospital, levels(P_TypeOfHospital),
        c("University", "County", "Rural", "Private"),
      ),
    P_Sex = factor(P_Gender, c("Kvinna", "Man"), c("Female", "Male")),
    P_BMI = cut(
      P_BMI,
      c(0, 25, 30, 35, 50),
      c("under/normal weight", "overweight", "class I obesity",
        "class II-III obesity"),
      right = FALSE
    ),

    # Survival
    stime   =
      as.numeric(coalesce(DateOfDeath, as.Date("2018-02-01")) - P_SurgDate),
    status  = !is.na(DateOfDeath),

    # truncate comorbidity indices
    charlson_icd10_index_quan_original = pmin(charlson_icd10_index_quan_original, 4),
    elix_icd10_index_sum_all           = pmin(elix_icd10_index_sum_all, 3),
    rxriskv_index_pratt                = pmin(rxriskv_regex_pratt_index_index_pratt, 10),

    # fixation
    cemented_stem = P_FemStemCemMix != "Cementfritt",
    cemented_cup  = P_AcetCupCemMix != "Cementfritt",

    # Diagnos
    P_DiaGrp = factor(droplevels(P_DiaGrp),
      c(
         "Primär artros",
         "Annan sekundär artros",
         "Följdtillstånd efter barnsjukdom i höftleden",
         "Idiopatisk nekros",
         "Inflammatorisk ledsjukdom",
         "Komplikation eller följdtillstånd efter fraktur el annat trauma"
      ),
      c(
         "Primary osteoarthritis",
         "Secondary osteoarthritis",
         "Sequelae after childhood hip disease",
         "Avascular necrosis of the femoral head (AVN)",
         "Inflammatory joint disease",
         "Secondary osteoarthritis"                   # merge of categories
       )
      )
  ) %>%

  # Remove variables not needed for analysis
  select(
    -P_AcetCupCemMix,
    -P_FemStemCemMix,
    -LopNr,
    -P_Side,
    -opnr,
    -DateOfDeath,
    -P_SurgDate,
    -P_KVA1,
    -P_ProstType,
    -P_Gender,
    -op_first, -op_last,
    -contains("index"),
    elix_icd10_index_sum_all,
    charlson_icd10_index_quan_original,
    rxriskv_index_pratt
  ) %>%
  # Shorter names
  rename_all(~ gsub("charlson_icd10", "CCI", .)) %>%
  rename_all(~ gsub("elix_icd10", "ECI", .)) %>%
  rename_all(~ gsub("rxriskv(_regex_pratt)?", "Rx", .))

