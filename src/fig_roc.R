suppressMessages({library(ProjectTemplate); load.project()})

load("cache/models.RData")

# SHAR --------------------------------------------------------------------

shar_roc <-
  models %>%
  unnest(roc_curve) %>%
  select(Model, specificity, sensitivity) %>%
  mutate(
    Model = model_names(Model)
  )

# Figure ------------------------------------------------------------------

fig_roc <-
  shar_roc %>%
  ggplot(aes(1 - specificity, sensitivity, col = Model)) +
  geom_path(size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = 2) +
  theme_minimal(15) +
  theme(
    legend.position = "bottom", # c(1, 0),
    #legend.justification = c(1, 0),
    legend.title = element_blank()
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)


ggsave("graphs/roc.png", fig_roc, height = 15, width = 15, units = "cm")
