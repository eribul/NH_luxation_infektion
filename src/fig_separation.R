suppressMessages({library(ProjectTemplate); load.project()})

load("cache/models.RData")

obspred <-
  models %>%
  select(Model, obspred, AUC_lo) %>%
  unnest(obspred) %>%
  filter(
    Model == "Reduced model"
  )


# Histogram ---------------------------------------------------------------

lims <- c(.002, .2)

fig_separation_hist <-
  obspred %>%
  ggplot(aes(pred)) +
  geom_histogram(
    aes(fill = obs),
    alpha = .5, position = "identity",
    bins = 30
  ) +
  theme_minimal(15) +
  theme(
    legend.position = "none",
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 30),
    axis.ticks      = element_line(size = 1)
  ) +
  xlab("Predicted probability [log]") +
  ylab(expression(paste(sqrt(n)))) +
  scale_x_log10(
    limits = lims,
    labels = scales::percent
  ) +
  scale_y_sqrt(
    labels = function(x) format(x, big.mark = ",")
  ) +
  expand_limits(x = 0)

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
    bins = 30
  ) +
  geom_density(aes(col = obs), bw = .1) +
  theme_minimal(15) +
  theme(
    legend.position = c(.9,.9),
    legend.title    = element_blank(),
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 30),
    axis.ticks      = element_line(size = 1),
    axis.ticks.y    = element_line(color = "white"),
    axis.text.y     = element_text(color = "white", size = 17)     # To align with upper panel
  ) +
  ylab("Density") +
  scale_x_log10(
    name = "Predicted probability [log]",
    limits = lims,
    labels = scales::percent
  ) +
  expand_limits(x = 0) +
  scale_fill_discrete(labels = c("no PJI", "PJI")) +
  scale_color_discrete(labels = c("no PJI", "PJI"))


# Combine plots -----------------------------------------------------------

ggsave(
  "graphs/separation.png",
  gridExtra::grid.arrange(fig_separation_hist, fig_separation_density, nrow = 1),
  height = 10, width = 20, units = "cm"
)
