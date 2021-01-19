
brlasso_selection_n <-
  round(c(main    = .1, reduced = .8) * config$Bmax)
cache("brlasso_selection_n")

# Table to report with no of selections per variable
brlasso_coefs_selected <- function(best_coefs_tmp) {

  brlasso_tbl_selected <-
    best_coefs_tmp %>%
    unnest(coefs) %>%
    count(variable, sort = TRUE)

  # List of variables selected every time
  best_coefs_reduced <-
    brlasso_tbl_selected %>%
    filter(n > brlasso_selection_n["reduced"]) %>% # Urspr föreslogs intersect (variabler som tas varje gång) OBS!!!!!!!!!
    select(variable) %>%
    pluck(1)

  # All variables ever selected
  best_coefs <-
    brlasso_tbl_selected %>%
    filter(n >= brlasso_selection_n["main"]) %>% # Urspr föreslogs intersect (variabler som tas varje gång) OBS!!!!!!!!!
    select(variable) %>%
    pluck(1)

  # Return
  list(
    brlasso_tbl_selected = brlasso_tbl_selected,
    best_coefs_reduced = best_coefs_reduced,
    best_coefs = best_coefs
  )
}

infection_data <-
  infection_data %>%
  mutate(
    coefs_selected = map(best_coefs_tmp, brlasso_coefs_selected)
  )

cache("infection_data")
