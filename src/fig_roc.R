suppressMessages({library(ProjectTemplate); load.project()})

load("cache/models.RData")

# SHAR --------------------------------------------------------------------

shar_roc <-
  models %>%
  unnest(roc_curve) %>%
  select(Model, specificity, sensitivity, AUC_est) %>%
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
  mutate(
    country = factor(country, c("Sweden", "Denmark")),
    Model = sprintf("%s (%.2f)", Model, AUC_est)
  )



# Sweden ------------------------------------------------------------------

ggdata %>%
  filter(country == "Sweden") %>%
  ggplot(aes(1 - specificity, sensitivity, col = Model)) +
  geom_path() +
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = 2) +
  theme_minimal(12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(col = guide_legend(ncol = 2)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)


ggsave("graphs/roc_se.png", height = 15, width = 15, units = "cm", dpi = 900)
ggsave("graphs/roc_se.pdf", height = 15, width = 15, units = "cm")
