library(stochtree_hugo)
n <- 500
p <- 5
X <- matrix(runif(n*p), ncol = p)
mu_x <- (
  ((0 <= X[,1]) & (0.25 > X[,1])) * (-7.5) + 
    ((0.25 <= X[,1]) & (0.5 > X[,1])) * (-2.5) + 
    ((0.5 <= X[,1]) & (0.75 > X[,1])) * (2.5) + 
    ((0.75 <= X[,1]) & (1 > X[,1])) * (7.5)
)
pi_x <- (
  ((0 <= X[,1]) & (0.25 > X[,1])) * (0.2) + 
    ((0.25 <= X[,1]) & (0.5 > X[,1])) * (0.4) + 
    ((0.5 <= X[,1]) & (0.75 > X[,1])) * (0.6) + 
    ((0.75 <= X[,1]) & (1 > X[,1])) * (0.8)
)
tau_x <- (
  ((0 <= X[,2]) & (0.25 > X[,2])) * (0.5) + 
    ((0.25 <= X[,2]) & (0.5 > X[,2])) * (1.0) + 
    ((0.5 <= X[,2]) & (0.75 > X[,2])) * (1.5) + 
    ((0.75 <= X[,2]) & (1 > X[,2])) * (2.0)
)
Z <- rbinom(n, 1, pi_x)
noise_sd <- 1
y <- mu_x + tau_x*Z + rnorm(n, 0, noise_sd)
test_set_pct <- 0.2
n_test <- round(test_set_pct*n)
n_train <- n - n_test
test_inds <- sort(sample(1:n, n_test, replace = FALSE))
train_inds <- (1:n)[!((1:n) %in% test_inds)]
X_test <- X[test_inds,]
X_train <- X[train_inds,]
pi_test <- pi_x[test_inds]
pi_train <- pi_x[train_inds]
Z_test <- Z[test_inds]
Z_train <- Z[train_inds]
y_test <- y[test_inds]
y_train <- y[train_inds]
mu_test <- mu_x[test_inds]
mu_train <- mu_x[train_inds]
tau_test <- tau_x[test_inds]
tau_train <- tau_x[train_inds]
bcf_model <- bcf(X_train = X_train, Z_train = Z_train, y_train = y_train, 
                 propensity_train = pi_train, X_test = X_test, Z_test = Z_test, 
                 propensity_test = pi_test, num_gfr = 10, 
                 num_burnin = 0, num_mcmc = 10)