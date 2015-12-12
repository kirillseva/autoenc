#' Validate and preprocess inputs to autoenc call
#'
#' Converts the params from autoenc inputs into an
#' intermediate representation that will be used by modeling functions.
validate_autoenc_params <- function(...) {
  inputs <- list(...)
  list(
    X                = parse_train_matrix(inputs$X),
    X.test           = parse_train_matrix(inputs$X.test),
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
    d_activation     = validate_activation(inputs$activation, inputs$d_activation)[[2]],
  )
}

NUMERIC_VARS <- c('lambda', 'beta', 'rho', 'epsilon', 'tolerance')
for (var in NUMERIC_VARS) {
  assign(paste0('validate_', var), function(x) {
    validate_positive_numeric(x, var)
  }, envir = environment())
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

validate_activation.default <- funciton(...) {
  stop('Invalid activation specified. Must be either a function or one of predefined activations, like ',
    paste0(names(ACTIVATION_FUNCTIONS), collapse = ', '))
}
