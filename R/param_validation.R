#' Validate and preprocess inputs to autoenc call
#'
#' Converts the params from autoenc inputs into an
#' intermediate representation that will be used by modeling functions.
validate_autoenc_params <- function(...) {
  inputs <- list(...)
  list(
    X                = validate_train_matrix(inputs$X),
    X.test           = validate_test_matrix(inputs$X.test),
    num_hidden       = validate_num_hidden(inputs$num_hidden),
    lambda           = validate_lambda(inputs$lambda),
    beta             = validate_beta(inputs$beta),
    rho              = validate_rho(inputs$rho),
    epsilon          = validate_epsilon(inputs$epsilon),
    tolerance        = validate_tolerance(inputs$tolerance),
    max_iterations   = validate_max_iterations(inputs$max_iterations),
    rescale          = validate_rescale(inputs$rescale),
    rescaling_offset = validate_rescaling_offset(inputs$rescaling_offset, inputs$rescale),
    optim_method     = validate_optim_method(inputs$optim_method),
    activation       = validate_activation(inputs$activation, inputs$d_activation)[[1]],
    d_activation     = validate_activation(inputs$activation, inputs$d_activation)[[2]]
  )
}

# validate_{lambda..tolerance}
NUMERIC_VARS <- c('lambda', 'beta', 'rho', 'epsilon', 'tolerance')
for (var in NUMERIC_VARS) {
  assign(paste0('validate_', var), function(x) {
    validate_positive_numeric(x, var)
  }, envir = environment())
}

validate_max_iterations <- function(x) {
  x <- validate_positive_numeric(x, 'max_iterations')
  stop_if_not(identical(x %% 1, 0), 'max_iterations must be a positive integer.')
  x
}

validate_num_hidden <- function(x) {
  if (is.numeric(x) && all(x %% 1 == 0)) {
    x
  } else {
    stop('num_hidden must be an integer vector, specifying number of neurons in each hidden layer.')
  }
}

validate_rescale <- function(x) if (isTRUE(x)) { TRUE } else { FALSE }

validate_rescaling_offset <- function(offset, rescale) {
  if (isTRUE(rescale)) { validate_positive_numeric(offset, 'rescaling_offset') }
}

validate_activation <- function(activation, d_activation) {
  UseMethod('validate_activation')
}
validate_activation.character <- function(activation, ...) {
  activation <- ACTIVATION_FUNCTIONS[[activation]]
  if (is.null(activation)) { stop('No such activation function found. Please select from ',
    paste0(names(ACTIVATION_FUNCTIONS), collapse = ', '), ' or supply your own <activation> and <d_activation>') }
  activation
}
validate_activation.function <- function(activation, d_activation) {
  list(
    activation   = validate_simple_function(activation, 'activation'),
    d_activation = validate_simple_function(d_activation, 'd_activation')
  )
}

validate_activation.default <- function(...) {
  stop('Invalid activation specified. Must be either a function or one of predefined activations, like ',
    paste0(names(ACTIVATION_FUNCTIONS), collapse = ', '))
}

validate_optim_method <- function(optim_method) {
  match_arg(optim_method, OPTIM_METHODS)
}

validate_train_matrix <- function(x) {
  UseMethod('validate_train_matrix')
}
validate_train_matrix.data.frame <- function(x) {
  numerical <- vapply(x, is.numeric, logical(1))
  if (all(numerical)) {
    as.matrix(x)
  } else {
    stop('Provided data.frame must contain only numeric columns.')
  }
}
validate_train_matrix.matrix <- function(x) {
  if (!is.numeric(x)) stop('Provided datasets must be numeric matrixes.')
  x
}
validate_train_matrix.default <- function(...) {
  stop('Provided datasets for test and training must be either numeric data frames or matrixes.')
}

validate_test_matrix <- function(x) {
  if (is.null(x)) { NULL } else { validate_train_matrix(x) }
}
