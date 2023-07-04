# residuals.R

quantile_residuals <- function(data, mu_func, f_func, Omegah_func, param, h, model_name) {
  # First, you need to get the model predictions
  mu_pred <- mu_func(data[-length(data)], param, h)
  if(model_name == "LL")
    Omega <- Omegah_func(data, param, h)
  else
    Omega <- Omegah_func(param, h)
  if (!is.null(f_func)) {
    # Calculate the function f(x)
    f_inv_pred <- f_func(data[2:length(data)], param, -h/2)
    
    # Calculate the residuals as the empirical cumulative distribution function
    residuals <- pnorm(f_inv_pred, mu_pred, sqrt(Omega))
  } else {
    # Calculate the residuals as the empirical cumulative distribution function
    residuals <- pnorm(data[2:length(data)], mu_pred, sqrt(Omega))
  }
  residuals
  # Transform the residuals back to a standard normal distribution
  qnorm(residuals)
}
