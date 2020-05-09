suppressMessages({library(ProjectTemplate); load.project()})

# Table to present --------------------------------------------------------

tbl_coefs <-
  infection_data %>%
  unnest("all_models") %>%
  filter(
    Model == "Main model"
  ) %>%
  select(time, tidy) %>%
  unnest(tidy) %>%
  transmute(
    time = factor(time, c("90d", "2y"), c("90 days", "2 years")),
    term = clean_names(term),
    beta = log(estimate),
    `OR 95 % CI` = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    p    = pvalue_format(.01)(p.value)
  ) %>%
  mutate_at(vars(`OR 95 % CI`, p), ~ if_else(term == "(intercept)", "", .))

cache("tbl_coefs")


# List of coefficients for text -------------------------------------------


set_first <- function(coefs, x) {
  if (x %in% coefs) c(x, setdiff(coefs, x))
  else coefs
}

coefs_print_string <- function(name) {
  all_vars <-
    filter(all_models, Model == !!name) %>%
    {.$tidy[[1]]$term[-1]}

  c_vars     <- all_vars[startsWith(all_vars, "c_")]

  basic_vars <-
    setdiff(all_vars, c_vars) %>%
    clean_names(firstupper = FALSE, lvls = FALSE) %>%
    set_first("ASA class") %>%
    set_first("sex") %>%
    set_first("age")

  c_vars <- clean_names(c_vars, FALSE)

  paste(
    glue::glue_collapse(basic_vars, ", "),
    "and the precense of",
    glue::glue_collapse(c_vars, ", ", last = " and ")
  )
}

# coefs_print <- coefs_print_string("BRL (age as main effect)")
# cache("coefs_print")
