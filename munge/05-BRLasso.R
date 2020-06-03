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
