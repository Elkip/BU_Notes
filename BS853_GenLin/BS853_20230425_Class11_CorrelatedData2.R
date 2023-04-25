# Poisson QIC for geese{geepack} output
# Ref: Pan (2001)
QIC.pois.geese <- function(model.R, model.indep) {
  library(MASS)
  # Fitted and observed values for quasi likelihood
  mu.R <- model.R$fitted.values
  # alt: X <- model.matrix(model.R)
      #  names(model.R$coefficients) <- NULL
      #  beta.R <- model.R$coefficients
      #  mu.R <- exp(X %*% beta.R)
  y <- model.R$y

  # Quasi Likelihood for Poisson
  quasi.R <- sum((y*log(mu.R)) - mu.R) # poisson()$dev.resids - scale and weights = 1

  # Trace Term (penalty for model complexity)
  AIinverse <- ginv(model.Indep$vbeta.naiv) # Omega-hat(I) via Moore-Penrose generalized inverse of a matrix in MASS package
  # Alt: AIinverse <- solve(model.Indep$vbeta.naiv) # solve via indenity
  Vr <- model.R$vbeta
  trace.R <- sum(diag(AIinverse %*% Vr))
  px <- length(mu.R) # number non-redunant columns in design matrix

  # QIC
  QIC <- (-2)*quasi.R + 2*trace.R
  QICu <- (-2)*quasi.R + 2*px    # Approximation assuming model structured correctly
  output <- c(QIC, QICu, quasi.R, trace.R, px)
  names(output) <- c('QIC', 'QICu', 'Quasi Lik', 'Trace', 'px')
  output
}