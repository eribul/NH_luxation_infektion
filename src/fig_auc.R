suppressMessages({library(ProjectTemplate); load.project()})

digs <- options("digits")
options(digits = 2)


# Prepare data ------------------------------------------------------------

data_auc_ci <-
  model_data %>%
  select(outcome, time, all_models) %>%
  unnest("all_models") %>%
  #filter(fig) %>%
  select(outcome, time, Model, starts_with("AUC")) %>%
  pivot_longer(c(-outcome, -time, -Model)) %>%
  filter(!is.na(value)) %>%
  separate(name, c("AUC","level", "corr")) %>%
  pivot_wider(names_from = level) %>%
  mutate(
    Model = fct_reorder(Model, est),
    time = factor(time, c("90d", "2y"), c("90 days", "2 years"))
  ) %>%
  filter(!is.na(corr))

annotates <-
  data_auc_ci %>%
  pivot_longer(c(lo, est, hi))

# Figure ------------------------------------------------------------------

ggplot(data_auc_ci, aes(Model, est, group = corr, color = corr)) +
  #geom_hline(yintercept = .7, color = "darkgreen", linetype = "dashed", size = 1) +
  geom_pointrange(aes(ymin = lo, ymax = hi), size = 1) + #, position = position_dodge(.5)) +
  geom_text(aes(Model, value, label = round(value, 2)), data = annotates, nudge_x = .3, size = 2) +
  coord_flip() +
  #theme_light() +
  #theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  ylab("AUC with 95% CI") +
  scale_y_continuous(breaks = seq(0, 1, .05)) +
  facet_grid(outcome ~ time)

options(digits = digs$digits)

ggsave("graphs/auc_ci.png", width = 30, height = 20, units = "cm")
