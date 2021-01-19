suppressMessages({library(ProjectTemplate); load.project()})

load("cache/infection_data.RData")

obspred <-
  infection_data %>%
  select(outcome, time, all_models) %>%
  unnest("all_models") %>%
  select(outcome, time, Model, obspred, AUC_lo) %>%
  unnest(obspred) %>%
  filter(
    Model == "Reduced model"
  ) %>%
  mutate(
    time = factor(time, c("90d", "2y"), c("90 days", "2 years")),
    # pred = factor(pred, c(TRUE, FALSE), c("PJI", "no PJI"))
  )


# Histogram ---------------------------------------------------------------

fig_separation_hist <-
  obspred %>%
  ggplot(aes(pred)) +
  geom_histogram(
    aes(fill = obs),
    alpha = .5, position = "identity",
    bins = 10
  ) +
  theme_minimal(15) +
  theme(
    legend.position = "none",
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 30),
    axis.ticks      = element_line(size = 1)
  ) +
  xlab("") +
  ylab(expression(paste(sqrt(n)))) +
  scale_x_log10(
    limits = c(.01, .1),
    labels = scales::percent
  ) +
  scale_y_sqrt(
    labels = function(x) format(x, big.mark = ",")
  ) +
  expand_limits(x = 0) +
  facet_grid(~ time, scales = "free_y")


# Density -----------------------------------------------------------------

fig_separation_density <-
  obspred %>%
  ggplot(aes(pred)) +
  geom_histogram(
    aes(
      y = ..density..,
      fill = obs
    ),
    alpha = .5, position = "identity",
    bins = 15
  ) +
  geom_density(aes(col = obs), bw = .1) +
  theme_minimal(15) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 30),
    axis.ticks = element_line(size = 1),
    axis.ticks.y = element_line(color = "white"),
    axis.text.y = element_text(color = "white", size = 17)     # To align with upper panel
  ) +
  ylab("Density") +
  scale_x_log10(
    name = "Predicted probability",
    limits = c(.01, .1),
    labels = scales::percent
  ) +
  expand_limits(x = 0) +
  facet_wrap(~ time, scales = "free_y") +
  scale_fill_discrete(labels = c("no PJI", "PJI")) +
  scale_color_discrete(labels = c("no PJI", "PJI"))


# Combine plots -----------------------------------------------------------

ggsave(
  "graphs/separation.png",
  gridExtra::grid.arrange(fig_separation_hist, fig_separation_density, nrow = 2),
  height = 15, width = 15, units = "cm"
)
