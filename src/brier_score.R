suppressMessages({library(ProjectTemplate); load.project()})

# See @Steyerberg2010 for scaled Brier score
brier_scaled <- function(brier, p_mean) 1 - brier / (p_mean * (1 - p_mean))

brier <- function(all_models) {
  all_models %>%
  mutate(

    # Version corrected for optimism
    brier.corrected = map_dbl(lrm_validate, ~.["B", "index.corrected"]),
    p_mean = map_dbl(obspred, ~mean(.$pred)),

    # Scaled version corrected for optimism
    brierscaled = map2_dbl(brier.corrected, p_mean, brier_scaled)
  ) %>%

  # All scores are approximately equal
  # The score does not work however for very rare events, see:
  # https://journals.ametsoc.org/doi/full/10.1175/2009MWR2945.1
  # We will therefore not report it, just keep it if required by reviewers.
  select(brier.corrected, p_mean, brierscaled)
}

brier_scores <-
  model_data %>%
  mutate(brier = map(all_models, brier)) %>%
  select(outcome, time, brier) %>%
  unnest("brier")

cache("brier_scores")
