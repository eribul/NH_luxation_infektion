# Apply lasso regression with CV for best lambda and return coeficient estimates
lasso <- function(x, l) {
  i <<- i + 1
  if (i %% 10 == 0) cat(".")

  fit <- glmnet::glmnet(
    select(x, -outcome) %>% as.matrix(),
    x$outcome,
    family = "binomial",
    lambda = l
  )
  as_tibble(
    as.matrix(coef(fit)),#, s = fit$lambda.min)),
    rownames = "variable"
  ) %>%
    rename(coef = s0) %>%
    filter(variable != "(Intercept)")
}
