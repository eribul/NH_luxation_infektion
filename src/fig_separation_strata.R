suppressMessages({library(ProjectTemplate); load.project()})

load("cache/models.RData")
load("cache/df.RData")

digs <- options("digits")
options(digits = 2)

library(ggridges)


plotstratified <- function(stratum) {
  usedat <- tibble(
    outcome = df$outcome,
    pred = predict(models$fit[[1]], df, type = "response"),
    stratum = as.factor(df[[stratum]])
  )
  stratlevel <- levels(usedat$stratum)
  obsprob <- tibble(
    trueprop = tapply(usedat$outcome, usedat$stratum, mean),
    meanpred = tapply(usedat$pred,    usedat$stratum, mean),
    stratum = seq_along(stratlevel)
  )
  ggplot(usedat, aes(y = stratum)) +
    ggridges::geom_density_ridges(aes(x = pred,
                                      fill = paste(stratum, factor(outcome))),
                                  alpha = .3,
                                  scale = .8) +
    geom_segment(
      data = obsprob,
      mapping = aes(
        x = trueprop,
        xend = trueprop,
        y = as.numeric(stratum) - .05,
        yend = as.numeric(stratum) + .5
      ),
      inherit.aes = FALSE,
      color = "red"
    ) +
    geom_segment(
      data = obsprob,
      mapping = aes(
        x = meanpred,
        xend = meanpred,
        y = as.numeric(stratum) - .05,
        yend = as.numeric(stratum) + .5
      ),
      inherit.aes = FALSE,
      color = "black"
    ) +
    scale_fill_cyclical(
      values = c(" violetred", "royalblue"),
      name = "outcome",
      guide = "legend",
      labels = c("No PJI", "PJI")
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, .1)) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    xlab('predicted probability')
}
plotstratified("P_BMI")
  ggsave("graphs/separation_strata_bmi.png", width = 10, height = 15, units = "cm")
plotstratified("P_Sex")
  ggsave("graphs/separation_strata_sex.png", width = 10, height = 10, units = "cm")
plotstratified("P_ASA")
  ggsave("graphs/separation_strata_asa.png", width = 10, height = 15, units = "cm")
plotstratified("P_DiaGrp")
  ggsave("graphs/separation_strata_diagrp.png", width = 20, height = 15, units = "cm")
