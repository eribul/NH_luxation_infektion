# Borrowed function from
# Author: Nina Zumel
# http://www.win-vector.com/blog/2014/05/trimming-the-fat-from-glm-models-in-r/
# Accessed: 2019-10-01
#
# Trim gml object to smallest possible size to still alow for predictions
# I do not want to include any data that is not needed for the Shiny application.
strip_glm = function(cm) {
  cm$y = c()
  cm$model = c()

  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()


  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()

  cm
}
