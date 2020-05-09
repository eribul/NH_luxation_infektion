
# Table to report with no of selections per variable
brlasso_coefs_selected <- function(best_coefs_tmp) {

  brlasso_tbl_selected <-
    best_coefs_tmp %>%
    unnest(coefs) %>%
    count(variable, sort = TRUE)

  # List of variables selected every time
  best_coefs_reduced <-
    brlasso_tbl_selected %>%
    filter(n >= round(1 / 3 * config$Bmax)) %>% # Urspr föreslogs intersect (variabler som tas varje gång)
    select(variable) %>%
    pluck(1)

  # All variables ever selected
  best_coefs <-
    brlasso_tbl_selected %>%
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
