# We will not consider any variable without observations in any combination
# of dead/alive-effected/not affected.
# This does not exclude any cases. It only exclude potential predictor variables

df0 <-
  df %>%
  select(
    -stime, -status,
    -contains("index"),
    -starts_with("outcome")
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


comb_lgl <-
  tibble(
    outcome = c("dislocation", "infection"),
    time    = c("90d", "2y")
  ) %>%
  expand(outcome, time) %>%
  mutate(
    outcome_var = paste0("outcome_", outcome, "_", time),
    lgl = map(outcome_var, ~ rare_conditions(df0, df[[.]])),
    lgl_vars = map(lgl, ~.$variable),
    lgl_text = map_chr(lgl, comb_lgl_text)
  )


# Same for factor variables -----------------------------------------------

comb_fct_tmp <-
  df0 %>%
  select_if(is.factor)

n_lvls <-
  comb_fct_tmp %>%
  summarise_all(n_distinct) %>%
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
  mutate(
    name = if_else(duplicated(name), "", name)
  ) %>%
  select(-n_lvls) %>%
  mutate(name = zoo::na.locf(na_if(name, ""))) %>%
  filter_at(vars(`FALSE`, `TRUE`), any_vars(. < 10)) %>%
  transmute(excl = paste0(name, " != '", value, "'")) %>%
  pluck(1)
}

comb_rm <-
  comb_lgl %>%
  mutate(
    fct = map(outcome_var, ~ comb_fct(df[[.]]))
  )

cache("comb_rm")


# df without those variables ----------------------------------------------

# Remove lgl variables that are too unusual
rm_unusual_lgls <- function(outcome_var, lgl_vars) {
  df0 %>%
  add_column(outcome = df[[outcome_var]]) %>%
  select(-one_of(lgl_vars)) %>%
  mutate_if(is.logical, as.factor) # needed for recipe
}

# Prepare data for modelling
bake_data <- function(df0) {
  recipe(outcome ~ ., df0_infection) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_predictors()) %>%
  prep(df0_infection) %>%
  bake(df0_infection)
}

model_data <-
  comb_rm %>%
  mutate(
    df0 = map2(outcome_var, lgl_vars, rm_unusual_lgls),
    # Filter out rows whith too unusual outcome!
    # Note that this is different comparede to simply remove the variable!
    # An alternative would be to lump the level with others!
    # But I don't know if that makes sence either!
    fct = map_chr(fct, ~ if (is.null(.)) NA_character_ else .),
    df0 = map2(df0, fct, ~ if (is.na(.y)) .x else filter(.x, eval(parse(text = .y)))),
    df0 = map(df0, droplevels),
    n_df0 = map_int(df0, nrow),
    # Bake data for modelling
    df_model = map(df0, bake_data)
  )

cache("model_data")
