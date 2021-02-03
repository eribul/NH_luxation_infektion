library(ProjectTemplate)
load.project()

load("cache/coefs_selected.RData")

coefs_selected_tab <-
  coefs_selected$brlasso_tbl_selected %>%
  mutate(
    variable = clean_names(variable)
  )

cache("coefs_selected_tab")
