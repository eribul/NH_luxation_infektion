suppressMessages({library(ProjectTemplate); load.project()})

# SHAR (internal calibration) ---------------------------------------------

get_beltdata <- function(cb) {
  tibble(
    x = cb$seqP,
    L = cb$cbBoundByConfLevel[[1]]$L,
    U = cb$cbBoundByConfLevel[[1]]$U
  )
}

shar_calibration <-
  model_data %>%
  select(outcome, time, all_models) %>%
  unnest("all_models") %>%
  filter(
    "BRL (age as main effect)"
  ) %>%
  mutate(cal_belt = map(cal_belt, get_beltdata)) %>%
  select(outcome,time,  Model, cal_belt) %>%
  unnest(cal_belt) %>%
  mutate(time = factor(time, c("90d", "2y"), c("90 days", "2 years")))

# Make figure -------------------------------------------------------------
calplot <- function(xlim = c(0, 0.17), ylim = c(0, 0.25)) {
  ggplot(shar_calibration, aes(x, ymin = L, ymax = U)) +
  geom_ribbon(alpha = .3) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  # theme_minimal() +
  scale_x_continuous(labels = scales::percent_format(1)) +
  scale_y_continuous(labels = scales::percent_format(1)) +
  coord_cartesian(xlim, ylim) +
  xlab("Predicted probability") +
  ylab("Observed proportion") +
  theme(
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.title = element_blank()
  ) +
  facet_grid(outcome ~ time)
}


ggsave("graphs/calibration.png", calplot(), height = 10, width = 20 , units = "cm")
ggsave("graphs/calibration_zoom.png", calplot(c(0, 0.05), c(0, 0.05)), height = 10, width = 20 , units = "cm")
