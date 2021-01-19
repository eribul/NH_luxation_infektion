suppressMessages({library(ProjectTemplate); load.project()})

categorization.Blad1 <- readxl::read_excel("data/categorization.xlsx")

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
  filter(!(is.na(Charlson) & is.na(Elixhauser)))

cache("tab_categorization")
