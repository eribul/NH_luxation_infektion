suppressMessages({library(ProjectTemplate); load.project()})


fit_export <- fit_brl_reduced$glm

fit_export$data <-
  select(
    fit_export$data,
    one_of(gsub("TRUE|[23]|Man", "",names(coef(fit_export))[-1]))
  ) # %>%
# head()

cache("fit_export")


options(digits = 22, pillar.sigfig = 22)
enframe(coef(fit_export))
