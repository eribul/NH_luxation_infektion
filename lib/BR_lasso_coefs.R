#' List important variables by importande from bootstrap ranking Lasso
#'
#' NOTE! It is important that all varibles are on a standardized scale for the
#' variable importance to make any sense!
#' @param df data.frame
#' @return tibble with important variables and a measure thereof
I <- 0
BR_lasso_coefs <- function(df, lambda) {
  I <<- I + 1
  i <<- 0
  cat(paste0("\n", I, ": "))

  df %>%
    rsample::bootstraps(config$N_bots) %>%
    mutate(
      data  = map(splits, as_tibble),
      lasso = map(data, lasso, lambda)
    ) %>%
    unnest(lasso) %>%
    group_by(variable) %>%
    summarise(
      impor = abs(mean(coef)),
      .groups = "drop"
    ) %>%
    filter(impor > 0) %>%
    arrange(desc(impor)) %>%
    add_rowindex()
}

#' @param coefs output from BR_lasso_coefs
#' @return break point for important variables
break_p <- function(coefs) {
  with(
    coefs,
    SiZer::piecewise.linear(.row, impor, 1)
  ) %>%
  {.$change.point} %>%
  round()
}
