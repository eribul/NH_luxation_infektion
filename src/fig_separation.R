suppressMessages({library(ProjectTemplate); load.project()})

obspred <-
  model_data %>%
  select(outcome, time, all_models) %>%
  unnest("all_models") %>%
  select(outcome, time, Model, fig, obspred, AUC_lo) %>%
  unnest(obspred) %>%
  filter(
    Model == "BRL (age as main effect)"
  ) %>%
  mutate(time = factor(time, c("90d", "2y"), c("90 days", "2 years")))


# Histogram ---------------------------------------------------------------

fig_separation_hist <-
  obspred %>%
  ggplot(aes(pred)) +
  geom_histogram(
    aes(fill = obs),
    alpha = .5, position = "identity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 30),
    axis.ticks = element_line(size = 1)
  ) +
  xlab("") +
  ylab(expression(paste(sqrt(n)))) +
  scale_x_log10(
    #breaks = c(.01, .1, 1, 3, 6, 10) / 100,
    limits = c(.001, .1),
    labels = scales::percent
  ) +
  scale_y_sqrt(
    #breaks = c(0, 100, 500, 1000, 2000, 3000, 4000, 5000), #seq(0, 6000, 1000),
    labels = function(x) format(x, big.mark = ",")
  ) +
  expand_limits(x = 0) +
  facet_grid(outcome ~ time, scales = "free_y")


# Density -----------------------------------------------------------------

fig_separation_density <-
  obspred %>%
  ggplot(aes(pred)) +
  geom_histogram(
    aes(
      y = ..density..,
      fill = obs
    ),
    alpha = .5, position = "identity"
  ) +
  geom_density(aes(col = obs), bw = .1) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 30),
    axis.ticks = element_line(size = 1),
    axis.ticks.y = element_line(color = "white"),
    axis.text.y = element_text(color = "white", size = 17)     # To align with upper panel
  ) +
  # geom_vline(aes(xintercept = .05), color = "darkgreen", linetype = "dashed") +
  # xlab(expression(paste("Predicted probability of death [", log[10], "]"))) +
  ylab("Density") +
  scale_x_log10(
    #breaks = c(.01, .1, 1, 3, 6, 10) / 100,
    limits = c(.001, .1),
    labels = scales::percent
  ) +
  expand_limits(x = 0) +
  facet_grid(outcome ~ time, scales = "free_y")


# Combine plots -----------------------------------------------------------

ggsave(
  "graphs/separation.png",
  gridExtra::grid.arrange(fig_separation_hist, fig_separation_density, nrow = 2),
  height = 15, width = 15, units = "cm"
)
