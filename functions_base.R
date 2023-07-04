# functions_base.R

# Source the torch functions
source("functions_torch.R")

# Base version of f function
f_base <- function(data, par, h){
  as_array(f(data, par, h))
}

# Base version of mu functions

mu_EM_base <- function(data, par, h) {
  as_array(mu_EM(data, par, h))
}

mu_S_base <- function(data, par, h) {
  as_array(mu_S(data, par, h))
}

mu_LL_base <- function(data, par, h){
  as_array(mu_LL(data, par, h))
}

# Base version of Omega functions
OmegahLL_base <- function(data, par, h){
  as.array(OmegahLL(data, par, h))
}