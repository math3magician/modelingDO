# functions_torch.R

# Drift function and its derivative
Fdrift <- function(data, par) {
  a0 <- par[1, drop = F]
  a1 <- par[2, drop = F]
  a3 <- par[3, drop = F]
  x <- torch_tensor(data)
  
  a0 + a1 * x + a3 * x^3
}

DFdrift <- function(data, par) {
  a0 <- par[1, drop = F]
  a1 <- par[2, drop = F]
  a3 <- par[3, drop = F]
  x <- torch_tensor(data)
  
  a1 + 3 * a3 * x^2
}

# Helper functions
r0 <- function(data, par, h) {
  n <- length(data)
  
  M1 <- torch_zeros(c(n, 2, 2))
  M1[, 1, 2] <- 1
  M1[, 2, 2] <- DFdrift(data, par)
  
  exphM1 <- torch_matrix_exp(h * M1)
  exphM1[, 1, 2]
}

r1 <- function(data, par, h) {
  n <- length(data)
  
  M2 <- torch_zeros(c(n, 3, 3))
  M2[, 1, 1] <- -DFdrift(data, par)
  M2[, 1, 2] <- 1
  M2[, 2, 3] <- 1
  
  exphM2 <- torch_matrix_exp(h * M2)
  torch_exp(h * DFdrift(data, par)) * exphM2[, 1, 3]
}

# Additional helper functions
M <- function(data, par, h) {
  a0 <- par[1, drop = F]
  a1 <- par[2, drop = F]
  a3 <- par[3, drop = F]
  sigma2 <- par[4, drop = F]
  x <- torch_tensor(data)
  
  3 * sigma2 * a3 * x
}

OmegahLL <- function(data, par, h) {
  sigma2 <- par[4, drop = F]
  n <- length(data)
  
  M3 <- torch_zeros(c(n, 2, 2))
  M3[, 1, 1] <- DFdrift(data, par)
  M3[, 1, 2] <- sigma2
  M3[, 2, 2] <- - M3[, 1, 1]
  
  exphM3 <- torch_matrix_exp(h * M3)
  F1 <- exphM3[, 1, 1]
  G1 <- exphM3[, 1, 2]
  G1*F1
}


mu_LL <- function(data, par, h){
  n <- length(data)
  
  data + r0(data, par, h) * Fdrift(data, par) + 
    (h * r0(data, par, h) - r1(data, par, h)) * M(data, par, h)
  
}

objectiveLL <- function(par, data, h) {
  N <- length(data)
  
  data_new <- data[2:N]
  data_old <- data[1:(N - 1)]
  
  x <- data_new - mu_LL(data_old, par, h)
  
  Omega <- OmegahLL(data_old, par, h)
  
  (x^2 / Omega + Omega$log())$sum()
}

mu_EM <- function(data, par, h) {
  data + h * Fdrift(data, par)
}

objectiveEM <- function(par, data, h) {
  N <- length(data)
  sigma2 <- par[4, drop = F]
  
  data_new <- data[2:N]
  data_old <- data[1:(N - 1)]
  
  x <- data_new - mu_EM(data_old, par, h)
  
  1/h * (x^2/sigma2)$sum() + 
    (N - 1) * (h * sigma2)$log() 
}

f <- function(data, par, h){
  a3 <- par[3, drop = F]
  x <- torch_tensor(data)
  n <- length(x)
  
  x/sqrt(1 - 2 * a3 * h * x^2)
}

Df <- function(data, par, h){
  a3 <- par[3, drop = F]
  x <- torch_tensor(data)
  n <- length(x)
  
  1/(1 - 2 * a3 * h * x^2)^(3/2)
}

mu_S <- function(data, par, h) {
  a0 <- par[1, drop = F]
  a1 <- par[2, drop = F]
  a3 <- par[3, drop = F]
  x <- torch_tensor(data)
  n <- length(x)
  
  exp(a1 * h) * f(x, par, h/2) - a0/a1 * (1 - exp(a1 * h))
}

OmegahS <- function(par, h){
  a1 <- par[2, drop = F]
  sigma2 <- par[4, drop = F]
  sigma2 * (exp(2 * a1 * h) - 1) / (2 * a1)
}

objectiveS <- function(par, data, h) {
  N <- length(data)
  a0 <- par[1, drop = F]
  a1 <- par[2, drop = F]
  a3 <- par[3, drop = F]
  sigma2 <- par[4, drop = F]
  
  data_new <- data[2:N]
  data_old <- data[1:(N - 1)]
  
  finv <- f(data_new, par, -h/2)
  x <- finv - mu_S(data_old, par, h)
  
  Omega <- OmegahS(par, h)
  
  (x^2 / Omega + Omega$log())$sum() -
    3 * log(1 - a3 * h * data_new^2)$sum()
}
