# We will not consider any variable without observations in any combination
# of dead/alive-effected/not affected.
# This does not exclude any cases. It only exclude potential predictor variables

df0 <-
  df %>%
  select(
    -stime, -status,
    -contains("index"),
    -outcome
  )


rare_conditions <- function(df, outcome, minimum = 10) {
  df %>%
  select(starts_with("c_")) %>%
  add_column(outcome = outcome) %>%
  mutate(outcome = factor(outcome, c(TRUE, FALSE), c("event", "no event"))) %>%
  group_by(outcome) %>%
  summarise_if(
    is.logical,
    list(
      .cond    = ~sum(.),
      .no_cond = ~ n() - sum(.))
  ) %>%
  pivot_longer(
    starts_with("c_"),
    names_to = c("variable", "condition"),
    names_sep = "_\\."
  ) %>%
  pivot_wider(names_from = c(outcome, condition)) %>%
  filter_at(vars(-variable), any_vars(. < minimum))
}

# Names of conditions to drop to present in text
comb_lgl_text <- function(comb_lgl) {
  comb_lgl$variable %>%
  clean_names(FALSE) %>%
  glue_collapse(", ", last = " and ")
}

comb_lgl <- list(lgl = rare_conditions(df0, df$outcome))
comb_lgl$lgl_vars <- comb_lgl$lgl$variable
comb_lgl$lgl_text <- comb_lgl_text(comb_lgl$lgl)


# Same for factor variables -----------------------------------------------

comb_fct_tmp <-
  df0 %>%
  select(where(is.factor))

n_lvls <-
  comb_fct_tmp %>%
  summarise(across(, n_distinct)) %>%
  pivot_longer(everything(), values_to = "n_lvls")

# Factor variables with less than 10 observations per outcome
comb_fct <- function(y) {
  comb_fct_tmp %>%
  pivot_longer(everything()) %>%
  nest(data = value) %>%
  left_join(n_lvls, "name") %>%
  mutate(
    data        = map(data, add_column, y),
    data        = map(data, count, y, value) #,
  ) %>%
  unnest(data) %>%
  pivot_wider(names_from = y, values_from = n) %>%
  select(-n_lvls) %>%
  filter_at(vars(`FALSE`, `TRUE`), any_vars(. < 10)) %>%
  transmute(excl = paste0(name, " != '", value, "'")) %>%
  pluck(1)
}


comb_rm <- c(
  fct = comb_fct(df$outcome),
  comb_lgl
)


cache("comb_rm")


# df without those variables ----------------------------------------------

# Prepare data for modelling
bake_data <- function(df0) {
  recipe(outcome ~ ., df0) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_predictors()) %>%
  prep(df0) %>%
  bake(df0)
}

infection_data <-
  df0 %>%
  add_column(outcome = df$outcome) %>%
  select(-one_of(comb_lgl$lgl_vars)) %>%
  mutate_if(is.logical, as.factor) %>%
  bake_data()

cache("infection_data")


# Save list of excluded co-morbidities for presentation
excl_factors <-
  comb_rm$lgl_text %>%
  {gsub("pancreatii", "pancreatic i", .)} %>%
  {gsub("AIDS/HIV hiv", "AIDS/HIV", .)}

cache("excl_factors")
