newton_raphson_dual_parameter_iterate <- function(point,function_to_zero,first_partial_derivative,second_partial_derivative,sample = c(0)){
  function_evaluation <- function_to_zero(point[1],point[2],sample)
  print(function_evaluation)
  dx_evaluation <- first_partial_derivative(point[1],point[2],sample)
  dy_evaluation <- second_partial_derivative(point[1],point[2],sample)
  descent_vector <- function_evaluation / c(dx_evaluation,dy_evaluation)
  return(unlist(point) - descent_vector)
}

log_likelihood <- function(alpha,lambda,X){
  n <- length(X)
  return(alpha * n * log(lambda) - n * log(gamma(alpha)) + (alpha - 1) * sum(log(X)) - lambda * sum(X))
}

log_likelihood_pd_alpha <- function(alpha,lambda,X){
  n <- length(X)
  return(n * log(lambda) - n*digamma(alpha) + sum(log(X)))
}

log_likelihood_pd_lambda <- function(alpha,lambda,X){
  n <- length(X)
  return(n * (alpha/lambda) - sum(X))
}

log_likelihood_2pd_lambda <- function(alpha,lambda,X){
  n <- length(X)
  return(-alpha * n * (lambda ^ - 2))
}

log_likelihood_2pd_alpha <- function(alpha,lambda,X){
  n <- length(X)
  return(n*trigamma(alpha))
}

log_likelihood_mixed <- function(alpha,lambda,X){
  n <- length(X)
  a <- alpha
  return(n / lambda)
}

derivative_square_sum <- function(alpha,lambda,X){
  return((log_likelihood_pd_lambda(alpha,lambda,X)) + (log_likelihood_pd_alpha(alpha,lambda,X)))
}

derivative_square_sum_pd_alpha <- function(alpha,lambda,X){
  return((log_likelihood_2pd_alpha(alpha,lambda,X)+log_likelihood_mixed(alpha,lambda,X)))
}

derivative_square_sum_pd_lambda <- function(alpha,lambda,X){
  return((log_likelihood_2pd_lambda(alpha,lambda,X)+log_likelihood_mixed(alpha,lambda,X)))
}
alpha = 1/2
lambda = 6

X = rgamma(1000,shape = alpha, rate = lambda)

mu_1_estimated = mean(X)
mu_2_estimated = mean(X^2)

initial_point <- c(mu_1_estimated^2 / (mu_2_estimated - mu_1_estimated^2), mu_1_estimated / (mu_2_estimated - mu_1_estimated^2))

point <- initial_point

for (i in 1:50){
  point <- newton_raphson_dual_parameter_iterate(point,derivative_square_sum,derivative_square_sum_pd_alpha,derivative_square_sum_pd_lambda,X)
}

