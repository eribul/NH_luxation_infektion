suppressMessages({library(ProjectTemplate); load.project()})


# This is just to calculate over-all survival as background info

fit <- survfit(Surv(stime, status) ~ 1, df)

surv90d <-
  with(
    summary(fit, 90),
    sprintf("%.2f %% (95 %% CI: %.2f - %.2f)",
    (1 - surv) * 100, (1 - upper) * 100, (1 - lower) * 100)
  )
cache("surv90d")


p <-
  survminer::ggsurvplot(
  fit,
  risk.table = TRUE,
  censor = FALSE,
  ylim = c(.7, 1),
  surv.scale = "percent",
  xscale = "d_y",
  break.time.by = 365.241
)

ggpubr::ggexport(p, filename = "graphs/km.png")
