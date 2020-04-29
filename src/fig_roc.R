suppressMessages({library(ProjectTemplate); load.project()})


# SHAR --------------------------------------------------------------------

shar_roc <-
  model_data %>%
  select(outcome, time, all_models) %>%
  unnest("all_models") %>%
  #filter(fig) %>%
  unnest(roc_curve) %>%
  select(outcome, time, Model, specificity, sensitivity) %>%
  mutate(
    Model = model_names(Model),
    time = factor(time, c("90d", "2y"), c("90 days", "2 years"))
  )

# Figure ------------------------------------------------------------------

fig_roc <-
  shar_roc %>%
  ggplot(aes(1 - specificity, sensitivity, col = Model)) +
  geom_path(size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = 2) +
  # theme_minimal() +
  theme(
    legend.position = "bottom", # c(1, 0),
    #legend.justification = c(1, 0),
    legend.title = element_blank()
  ) +
  facet_grid(outcome ~ time) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)


ggsave("graphs/roc.png", fig_roc, height = 30, width = 30, units = "cm")
