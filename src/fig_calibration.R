suppressMessages({library(ProjectTemplate); load.project()})

load("cache/models.RData")

get_beltdata <- function(cb) {
  tibble(
    x = cb$seqP,
    L = cb$cbBoundByConfLevel[[1]]$L,
    U = cb$cbBoundByConfLevel[[1]]$U
  )
}

df_calibration_se <-
  models %>%
  filter(Model == "Reduced model", country == "se") %>%
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


# Danish ------------------------------------------------------------------

source("validation/calibration_DK.txt")


# Make figure -------------------------------------------------------------
bind_rows(
  "Internal (SE)" = df_calibration_se,
  "External  (DK)" = get_beltdata(calibration),
  "Re-calibrated (DK)" = get_beltdata(calibration2),
  .id = "country"
) %>%
ggplot(aes(x, ymin = L, ymax = U, fill = country)) +
geom_ribbon(alpha = .5) +
geom_abline(aes(intercept = 0, slope = 1)) +
theme_minimal(15) +
scale_x_continuous(labels = scales::percent_format(1), limits = c(0, .1), breaks = seq(0, .1, .02)) +
scale_y_continuous(labels = scales::percent_format(1), limits = c(0, .1), breaks = seq(0, .1, .02)) +
xlab("Predicted probability") +
ylab("Observed proportion") +
theme(
  legend.position = c(1, 0),
  legend.justification = c(1, 0),
  legend.title = element_blank(),
  panel.grid.minor = element_blank()
)


ggsave("graphs/calibration.png", height = 10, width = 10 , units = "cm")
ggsave("graphs/calibration.pdf", height = 10, width = 10 , units = "cm")


