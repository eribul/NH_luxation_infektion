suppressMessages({library(ProjectTemplate); load.project()})

load("cache/df.RData")

dft1 <-
  df %>%
  mutate(
    Charlson   = replace(CCI_index_quan_original, CCI_index_quan_original > 1, "2+"),
    Elixhauser = replace(ECI_index_sum_all, ECI_index_sum_all > 2, "3+"),
    RxRiskV    = cut(Rx_index_pratt, c(-Inf, 0, 3, 6, Inf), c("-5-0", "1-3", "4-6", "7+")),
    P_BMI      = relevel(P_BMI, "under/normal weight"),
    P_Age      = cut(P_Age, c(0, 49, 59, 69, 79, Inf), c("<50", "50-<60", "60-<70", "70-<80", "80+"))
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

t1 <-
  tableone::CreateTableOne(
    strata = "PJI",
    vars = setdiff(names(dft1), "PJI"),
    data = dft1,
    test = FALSE
  ) %>% print(
    printToggle = FALSE,
    showAllLevels = TRUE
  ) %>%
  as_tibble(rownames = "what") %>%
  rename(
    `SE PJI` = `TRUE`,
    `SE No PJI` = `FALSE`
  ) %>%
  mutate(
    what = na_if(what, ""),
    what = zoo::na.locf(what),
    what = gsub(":? \\(%\\)", "", what)
  )


t1_tot <-
  tableone::CreateTableOne(
    vars = setdiff(names(dft1), "PJI"),
    data = dft1,
    test = FALSE,
  ) %>%
  print(
    printToggle = FALSE,
    showAllLevels = TRUE
  ) %>%
  as_tibble(rownames = "what") %>%
  rename(`SE Total` = Overall) %>%
  mutate(
    what = na_if(what, ""),
    what = zoo::na.locf(what),
    what = gsub(":? \\(%\\)", "", what)
  )


se_table1 <-
  left_join(
    t1,
    t1_tot
  ) %>%
  filter(level != "FALSE") %>%
  mutate_at(vars(-what, -level), zero) %>%
  select(
    what,
    level,
    `SE PJI`,
    `SE No PJI`,
    `SE Total`
  ) %>%
  mutate(
    what = case_when(
      what == "n"               ~ "Total",
      what == "AIDS/HIV hiv"    ~ "AIDS/HIV",
      what == "Heart infarct"   ~ "Myocardial infarction",
      what == "Peptiulcer"      ~ "Peptic ulcer",
      what == "Rheumatidisease" ~ "Rheumatic disease",
      TRUE  ~ what
    ),
    level = tolower(level),
    level = na_if(level, ""),
    level = na_if(level, "true")
  )



# DK table ----------------------------------------------------------------

dk1 <-
  docxtractr::read_docx("validation/DK_table1_20210325.docx") %>%
  docxtractr::docx_extract_tbl()

names(dk1) <- c("what", "No_PJI_N", "No_PJI_prop", "PJI_N", "PJI_prop", "Tot_N", "Tot_prop")
dk1 <- dk1[-(1:2), ] # Remove headers and age (which should not be categorized)


dk1 <- mutate(dk1, across(-what, as.numeric))

# Identifiera namn på faktorer till skillnad från levels
dk1$factors <- dk1$what %in% dk1$what[which(is.na(dk1$Tot_N)) -1]



dk2 <-
  dk1 %>%
  # FLytta ner icke-NA-värden till resp level
  mutate(across(c(-what, -factors), zoo::na.locf)) %>%
  # Ta bort värdena från faktor-rubriken
  mutate(across(c(-what, -factors), ~ replace(., factors, NA))) %>%
  # Ta bort de värden som inte fanns från början
  mutate(across(c(-what, -factors, -Tot_N, -Tot_prop),
                ~ replace(., what %in% c("Sequelae after childhood hip disease",
                                         "Inflammatory joint disease"), NA))) %>%
  mutate(
    level = replace(what, factors, NA),
    what  = replace(what, !factors & what != "Total", NA),
    what = zoo::na.locf(what)
  ) %>%
  filter(
    level != "No",
    what != "Region"
  )


# Slå samman civilstånd "." och "divorced till single
sing <-
  dk2 %>%
  filter(level %in% c(".", "Divorced")) %>%
  select(-level) %>%
  group_by(what, factors) %>%
  summarise(across(everything(), sum)) %>%
  mutate(level = "Single")

dk2 <-
  dk2 %>%
  filter(!level %in% c(".", "Divorced")) %>%
  bind_rows(sing) %>%
  transmute(
    what,
    level,
    `DK PJI`    = sprintf("%d (%.1f)", PJI_N, PJI_prop),
    `DK No PJI` = sprintf("%d (%.1f)", No_PJI_N, No_PJI_prop),
    `DK Total`  = sprintf("%d (%.1f)", Tot_N, Tot_prop)
  ) %>%
  mutate(
    what = case_when(
      what == "Rx index" ~ "RxRiskV",
      what == "Fluid electolyte disorders" ~ "Fluid electrolyte disorders",
      TRUE ~ what
    ),
    level = gsub("Widow", "Widow/widower", level),
    level = tolower(level),
    level = na_if(level, "yes"),
    level = na_if(level, "total")
  )


# Slå samman --------------------------------------------------------------

# Check that no levels differ
setdiff(table1$what, dk2$what)
setdiff(table1$level, dk2$level)

setdiff(dk2$what, table1$what)
setdiff(dk2$level, table1$level)

table1 <-
  left_join(se_table1, dk2) %>%
  mutate(
    level = ifelse(level %in% c("i", "ii", "iii"), toupper(level), level),

  ) %>%
mutate(across(everything(), na_if, "NA (NA)")) %>%
mutate(across(c(`DK PJI`, `DK No PJI`, `DK Total`),
              ~ ifelse(what == "Total", sub("\\(100.0\\)", "", .), .)
))
cache("table1")
