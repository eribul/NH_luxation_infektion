suppressMessages({library(ProjectTemplate); load.project()})

# Table to present --------------------------------------------------------

tbl_coefs <-
  model_data %>%
  select(outcome, time, all_models) %>%
  unnest("all_models") %>%
  filter(
    Model == "BRL (age as main effect)"
  ) %>%
  select(outcome, time, Model, tidy) %>%
  unnest(tidy) %>%
  transmute(
    outcome,
    time = factor(time, c("90d", "2y"), c("90 days", "2 years")),
    Model,
    term = clean_names(term),
    term = gsub("_", " ", term),
    term = gsub("Diagnos", "Diagnos: ", term),
    math = sprintf("\\text{%s} \\\\ ", term) %>% {gsub("\\text{(intercept)} \\\\ ", "", ., fixed = TRUE)},
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

# coefs_print_reduced <- coefs_print_string("BRL reduced (age as main effect)")
# coefs_print         <- coefs_print_string("BRL (age as main effect)")
# cache("coefs_print")
# cache("coefs_print_reduced")


# Mathematical formula ----------------------------------------------------
#
# coefs_form <-
#   brlasso_tbl_coefs %>%
#   transmute(
#     coefs = ifelse(
#       math == "",
#       sprintf("%.2f \\\\ ", beta),
#       sprintf("%.2f \\cdot %s", beta, math)
#     )
#   ) %>%
#   summarise(coefs = paste(coefs, collapse = " & + ")) %>%
#   select(coefs) %>%
#   pluck(1) %>%
#   substr(1, nchar(.) - 4) %>%
#   {sprintf("$$\\begin{aligned} \\hat \\beta X = & %s \\end{aligned}.$$", .)}
#
# cache("coefs_form")
