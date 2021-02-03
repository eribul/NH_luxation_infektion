suppressMessages({library(ProjectTemplate); load.project()})

load("cache/models.RData")

get_beltdata <- function(cb) {
  tibble(
    x = cb$seqP,
    L = cb$cbBoundByConfLevel[[1]]$L,
    U = cb$cbBoundByConfLevel[[1]]$U
  )
}

df_calibration <-
  models %>%
  filter(Model == "Reduced model") %>%
  mutate(
    cal_belt = furrr::future_map(
      obspred, ~ givitiR::givitiCalibrationBelt(
        as.numeric(.$obs == "TRUE"), .$pred, devel = "internal"),
      .progress = TRUE),
    cal_belt.p = map_dbl(cal_belt, "p.value"),
    cal_belt   = map(cal_belt, get_beltdata)
  ) %>%
  select(Model, cal_belt) %>%
  unnest(cal_belt)

# Make figure -------------------------------------------------------------
calplot <- function(xlim = c(0, 0.1), ylim = c(0, 0.1)) {
  ggplot(df_calibration, aes(x, ymin = L, ymax = U)) +
  geom_ribbon(alpha = .3) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  theme_minimal(15) +
  scale_x_continuous(labels = scales::percent_format(1)) +
  scale_y_continuous(labels = scales::percent_format(1)) +
  coord_cartesian(xlim, ylim) +
  xlab("Predicted probability") +
  ylab("Observed proportion") +
  theme(
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.title = element_blank()
  )
}


ggsave("graphs/calibration.png", calplot(), height = 10, width = 10 , units = "cm")
