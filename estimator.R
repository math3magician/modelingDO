# estimator.R

estimator <- function(obj, data, h, par_star = c(0.2, -0.1, -0.2, 4), 
                      num_iterations = 500) {
  par_star <- torch_tensor(par_star, requires_grad = TRUE)
  
  optimizer <- optim_rprop(par_star)
  
  calc_loss <- function() {
    optimizer$zero_grad()
    value <- obj(par_star, data, h)
    value$backward()
    value
  }
  for (i in 1:num_iterations) {
    par_old = as.matrix(par_star)
    optimizer$step(calc_loss)
    par_new = as.matrix(par_star)
    if(norm(par_new - par_old) < 10^-5) break
  }
  convergence <- 0
  if(i == num_iterations) convergence <- 1
  list(par_star, i, convergence)
}
