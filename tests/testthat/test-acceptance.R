context('acceptance')

describe('parameter validation', {
  inputs <- list(
    X                = data.frame(a = 1:10, b = rep(0.4, 10)),
    X.test           = NULL,
    num_hidden       = 100,
    activation       = 'tanh',
    lambda           = 0.4,
    beta             = 0.1,
    rho              = 0.2,
    epsilon          = 0.5,
    tolerance        = 0.001,
    d_activation     = NULL,
    optim_method     = 'BFGS',
    max_iterations   = 2000,
    rescale          = TRUE,
    rescaling_offset = 0.01
  )
  outputs              <- inputs
  outputs$X            <- as.matrix(data.frame(a = 1:10, b = rep(0.4, 10)))
  outputs$activation   <- ACTIVATION_FUNCTIONS[['tanh']]$activation
  outputs$d_activation <- ACTIVATION_FUNCTIONS[['tanh']]$d_activation
  result               <- do.call(validate_autoenc_params, inputs)
  expect_equal(length(result), length(outputs))
  lapply(names(outputs), function(nm) {
    expect_equal(result[[nm]], outputs[[nm]])
  })
})
