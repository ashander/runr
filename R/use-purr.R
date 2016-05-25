growth_t <- function(n0, r, K, b, T) {
  n <- n0
  
  for (t in 1:T) {
    n <- n + r - exp(n) / K - b - rnorm(1, 0, 0.1)
  }
  n
}

data <- expand.grid(
  b = seq(0.01, 0.5, length.out = 5),
  K = exp(seq(0.1, 5, length.out = 5)),
  r = seq(0.5, 3.5, length.out = 5),
  rep = 1:20
)

data_small <- expand.grid(
  b = seq(0.01, 0.5, length.out = 5),
  K = exp(seq(0.1, 5, length.out = 5)),
  r = seq(0.5, 3.5, length.out = 5)
)
data_small$rep <- 20

growth <- function(n, r, K, b) {
  # some dynamical simulation
  # this is an obviously-inefficient way to do this ;)
  n  + r - exp(n) / K - b - rnorm(1, 0, 0.1)
}
growth_runner <- function(r, K, b, rep, T, n0, ...) {
  # a wrapper to run the simulation with some fixed values
  data.frame(n_final = replicate(rep, {for(t in 1:T) {
    n0 <- growth(n0, r, K, b)
  };
    n0})
  )
}
