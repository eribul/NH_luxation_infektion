# Tidiagre steg utgick från både infektion och luxation kombinerat.
# JAg väljer nu att fortsätta med enbart infektion och hanteras luxation separat sernare

infection_data <-
  model_data %>%
  filter(outcome == "infection")



# Perform calculations (time consuming)

best_coefs_fun <- function(df_model) {
  tibble(B = seq_len(config$Bmax)) %>%
  mutate(
    coefs_all = future_map(B, ~ BR_lasso_coefs(df_model), .progress = TRUE),
    breaks    = map_dbl(coefs_all, break_p),
    coefs     = map2(coefs_all, breaks, ~slice(.x, seq_len(.y)))
  )
}


infection_data <-
  infection_data %>%
  mutate(
    best_coefs_tmp = map(df_model, best_coefs_fun)
  )

cache("infection_data")
