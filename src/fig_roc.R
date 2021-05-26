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


load("validation/export_DK.RData")
dk <-
  roc_plot_coords %>%
  mutate(Model = "Reduced model (DK)") %>%
  rename(
    specificity = specificities,
    sensitivity = sensitivities
  )

ggdata <-
  bind_rows(
    Sweden  = shar_roc,
    Denmark = filter(shar_roc, Model == "Reduced model"),
    Denmark = dk,
    .id = "country"
  ) %>%
  mutate(country = factor(country, c("Sweden", "Denmark")))

# Figure ------------------------------------------------------------------

ggdata %>%
  ggplot(aes(1 - specificity, sensitivity, col = Model)) +
  geom_path() +
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = 2) +
  theme_minimal(10) +
  theme(
    legend.position = "bottom", # c(1, 0),
    #legend.justification = c(1, 0),
    legend.title = element_blank(),
    legend.text = element_text(size = 4.5)
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ country)


ggsave("graphs/roc.png", height = 10, width = 15, units = "cm", dpi = 900)
