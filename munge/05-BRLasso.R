# Perform calculations (time consuming)

if (!exists("infection_data"))
  load("cache/infection_data.RData")


lambda <- glmnet::cv.glmnet(
  select(infection_data, -outcome) %>% as.matrix(),
  infection_data$outcome,
  family = "binomial"
)$lambda.min

best_coefs_tmp <-
  tibble(B = seq_len(config$Bmax)) %>%
  mutate(
    coefs_all = map(B, ~ BR_lasso_coefs(infection_data, lambda), .progress = TRUE),
    breaks    = map_dbl(coefs_all, break_p),
    coefs     = map2(coefs_all, breaks, ~slice(.x, seq_len(.y)))
  )

cache("best_coefs_tmp")



# Find the second change point for piecewise linear regression with three segments
# instead of
cp2 <- function(df) {
  df$row <- df$.row
  model  <- list(impor ~ 1 + row, ~ 0 + row, ~ 0 + row)
  fit    <- suppressMessages(mcp(model, df))
  f      <- suppressWarnings(fixef(fit))
  f[f$name == "cp_2", "mean"]
}

future::plan(multiprocess, workers = 7)

# Replace old break points with new --------------------------------------------
best_coefs_tmp$breaks <-
  best_coefs_tmp$coefs_all %>%
  furrr::future_map_dbl(cp2, .progress = TRUE)

# Include variables based on new break points
best_coefs_tmp$coefs <-
  with(
    best_coefs_tmp,
    map2(coefs_all, breaks, ~slice(.x, seq_len(.y)))
  )



# Different models --------------------------------------------------------

brlasso_selection_n <-
  round(c(main    = .1, reduced = .8) * config$Bmax)
cache("brlasso_selection_n")

# All variables selected at least once
brlasso_tbl_selected <-
  best_coefs_tmp %>%
  unnest(coefs) %>%
  count(variable, sort = TRUE)

# List of variables selected every time
best_coefs_reduced <-
  brlasso_tbl_selected %>%
  # Urspr föreslogs intersect (variabler som tas varje gång) OBS!!!!!!!!!
  filter(n >= brlasso_selection_n["reduced"]) %>%
  select(variable) %>%
  pluck(1)

# All variables selected according to lower threshold
best_coefs <-
  brlasso_tbl_selected %>%
  # Urspr föreslogs intersect (variabler som tas varje gång) OBS!!!!!!!!!
  filter(n >= brlasso_selection_n["main"]) %>%
  select(variable) %>%
  pluck(1)

coefs_selected <-
  list(
    brlasso_tbl_selected = brlasso_tbl_selected,
    best_coefs_reduced = best_coefs_reduced,
    best_coefs = best_coefs
  )
cache("coefs_selected")
