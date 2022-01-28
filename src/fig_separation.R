suppressMessages({library(ProjectTemplate); load.project()})

load("cache/models.RData")

obspred <-
  models %>%
  select(Model, obspred, AUC_lo) %>%
  unnest(obspred) %>%
  filter(
    Model == "Reduced model"
  )


g <-
  obspred %>%
  ggplot(aes(pred)) +
  geom_density(aes(col = obs), bw = .1) +
  theme_minimal(12) +
  theme(
    legend.position = c(.1,.9),
    legend.title    = element_blank(),
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 30),
    axis.ticks      = element_line(size = 1),
    axis.ticks.y    = element_line(color = "white"),
    axis.text.y     = element_blank()     # To align with upper panel
  ) +
  ylab("Density") +
  scale_x_log10(
    name = "Predicted probability [log(10)]",
    limits = lims,
    labels = scales::percent
  ) +
  expand_limits(x = 0) +
  scale_fill_discrete(labels = c("no PJI", "PJI")) +
  scale_color_discrete(labels = c("no PJI", "PJI"))


ggsave("graphs/separation.png", g, height = 10, width = 13, units = "cm" )
ggsave("graphs/separation.pdf", g, height = 10, width = 13, units = "cm")
