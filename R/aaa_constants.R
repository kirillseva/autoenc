ACTIVATION_FUNCTIONS <- list(
  logistic = list(
    activation   = function(x) { 1 / (1 + exp(-x)) },
    d_activation = function(x) {
      fn <- function(z) { 1 / (1 + exp(-z)) }
      fn(x) * (1 - fn(x))
    }
  ),
  tanh = list(
    activation   = function(x) { tanh(x) },
    d_activation = function(x) { 2 / (1 + cosh(2 * x)) }
  )
)

OPTIM_METHODS <- c("BFGS", "L-BFGS-B", "CG")

# call autoenc:::constants in order to get a quick peek at suitable defaults.
constants <- list(
  optim_methods        = OPTIM_METHODS,
  activation_functions = ACTIVATION_FUNCTIONS
)
