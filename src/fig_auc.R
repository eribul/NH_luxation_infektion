suppressMessages({library(ProjectTemplate); load.project()})

load("cache/models.RData")

digs <- options("digits")
options(digits = 2)


# Prepare data ------------------------------------------------------------

data_auc_ci <-
  models %>%
  select(country, Model, starts_with("AUC")) %>%
  mutate(
    Model  = gsub("Reduced model$", "Reduced model (SE)", Model),
    Model  = reorder(Model, AUC_est),
    ci     = sprintf("95 %% CI: %.2f to %.2f", AUC_lo, AUC_hi),
    text1  = sprintf("(AUC = %.2f, %s)", AUC_est, ci),
    text2  = sprintf("AUC = %.2f (%s)", AUC_est, ci),
    country = factor(country, c("se", "dk"), c("Sweden", "Denmark"))
  )

cache("data_auc_ci")

annotates <-
  data_auc_ci %>%
  pivot_longer(c(AUC_lo, AUC_est, AUC_hi))

# Figure ------------------------------------------------------------------

ggplot(data_auc_ci, aes(Model, AUC_est)) +
  geom_hline(yintercept = 0.7, color = "grey") +
  geom_pointrange(
    aes(ymin = AUC_lo, ymax = AUC_hi),
    size = .33,
    position = position_dodge(width = .25)
  ) +
  geom_text(
    aes(Model, value, label = round(value, 2)),
    data = annotates,
    nudge_x = .3,
  #  nudge_y = .5,
    size = 1.5,
    # position = position_dodge(width = .85)
  ) +
  coord_flip() +
  theme_minimal(10) +
  theme(
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # axis.text.x = element_blank()
  ) +
  ylab("AUC with 95% CI") +
  facet_wrap(~ country, scales = "free_x")

ggsave("graphs/auc_ci.png", width = 15, height = 10, units = "cm")

options(digits = digs$digits)
