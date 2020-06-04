# Perform calculations (time consuming)

if (!exists("infection_data"))
  load("cache/infection_data.RData")

best_coefs_fun <- function(df_model) {

  lambda <- glmnet::cv.glmnet(
    select(df_model, -outcome) %>% as.matrix(),
    df_model$outcome,
    family = "binomial"
  )$lambda.min

  tibble(B = seq_len(config$Bmax)) %>%
  mutate(
    coefs_all = map(B, ~ BR_lasso_coefs(df_model, lambda), .progress = TRUE),
    breaks    = map_dbl(coefs_all, break_p),
    coefs     = map2(coefs_all, breaks, ~slice(.x, seq_len(.y)))
  )
}

Sys.time()
tmp1 <- best_coefs_fun(infection_data$df_model[[1]]); cache("tmp1"); Sys.time()
tmp2 <- best_coefs_fun(infection_data$df_model[[2]]); cache("tmp2"); Sys.time()

infection_data <-
  infection_data %>%
  mutate(
    best_coefs_tmp = list(tmp1, tmp2)
  )

cache("infection_data")










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
infection_data$best_coefs_tmp[[1]]$breaks <-
  infection_data$best_coefs_tmp[[1]]$coefs_all %>%
  furrr::future_map_dbl(cp2, .progress = TRUE)

infection_data$best_coefs_tmp[[2]]$breaks <-
  infection_data$best_coefs_tmp[[2]]$coefs_all %>%
  furrr::future_map_dbl(cp2, .progress = TRUE)


# Include variables based on new break points
infection_data$best_coefs_tmp[[1]]$coefs <-
  with(
    infection_data$best_coefs_tmp[[1]],
    map2(coefs_all, breaks, ~slice(.x, seq_len(.y)))
  )

infection_data$best_coefs_tmp[[2]]$coefs <-
  with(
    infection_data$best_coefs_tmp[[2]],
    map2(coefs_all, breaks, ~slice(.x, seq_len(.y)))
  )

cache("infection_data")
