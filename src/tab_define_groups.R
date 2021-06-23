suppressMessages({library(ProjectTemplate); load.project()})

load("cache/categorization.Blad1.RData")

tab_categorization <-
  categorization.Blad1 %>%
  mutate_all(zoo::na.locf) %>%
  group_by(new, from) %>%
  summarise(old = paste(str_to_sentence(old), collapse = ", ")) %>%
  pivot_wider(names_from = from, values_from = old) %>%
  rename(
    `Comorbidities by groups` = new,
    Charlson = CCI,
    Elixhauser = ECI
  ) %>%

  # To test without Rx Risk V
  select(-Rx) %>%
  filter(!(is.na(Charlson) & is.na(Elixhauser))) %>%
  mutate(across(c(Charlson, Elixhauser), ~ gsub("Aids/hiv", "AIDS/HIV", .)))

cache("tab_categorization")
