#' Train a sparse autoencoder.
#'
#' Implementation based on http://web.stanford.edu/class/archive/cs/cs294a/cs294a.1104/sparseAutoencoder.pdf
#'
#' @param X matrix. Training dataset.
#' @param X.test matrix (optional). Testing dataset for evaluating the network.
#' @param num_hidden integer. Specifies number of neurons in each hidden layer.
#' @param activation character or function. If it's a character it has to be one of the
#'   predefined activation functions. If it's a function, \code{d_activation}, activation prime,
#'   function must be supplied as well. Those functions are needed for backpropagation.
#' @param d_activation function (optional). This parameter will be omitted if
#'   activation is a character, otherwise this function will be used in backpropagation
#'   as activation prime, in which case this parameter becomes mandatory.
#' @param optim_method character (optional). Optimization method to be used. Please check
#'   \code{autoenc:::constants$optim_methods} for available options. Default is \code{"BFGS"}.
#' @param max_iterations integer (optional). Maximum number of iterations for
#'   optimizer. Default is \code{2000}.
#' @param rescale logical or numeric. Autoencoders yield better results when used on
#'   normalized matrices. Normalization should be performed according to the activation
#'   function being used. Default is \code{TRUE}, which will select the appropritate ranges
#'   for activation functions included in the package. If you are providing custom
#'   activation function, you must specify an appropriate range, like \code{c(-1, 1)}.
#'   If it is \code{FALSE} no rescaling will be performed.
#' @param rescaling_offest numeric. A small value used in rescaling to
#'   \code{c(rescale[1] + offset, rescale[2] - offset)} interval. Default is \code{0.001}.
#' @param lambda numeric. Weight decay parameter.
#' @param beta numeric. Learning rate parameter for algorithm trying to (approximately)
#'   satisfy the sparsity constraint.
#' @param tolerance numeric (optional). Tolarance to be used for comparing floting point numbers.
#'   Default is \code{.Machine$double.eps}
#' @param rho numeric. Sparsity parameter, which specifies our desired level of sparsity.
#' @param epsilon numeric. A (small) parameter for initialization of weight matrices
#'   as small gaussian random numbers sampled from \code{N(0, epsilon^2)}
#'
#' @return Object of class autoenc.
#' @export
sparse_autoenc <-
  function(X, num_hidden, activation, lambda, beta, rho, epsilon,
    tolerance        = sqrt(.Machine$double.eps),
    X.test           = NULL,
    d_activation     = NULL,
    optim_method     = "BFGS",
    max_iterations   = 2000,
    rescale          = TRUE,
    rescaling_offset = 0.001) {

  params <- do.call(validate_autoenc_params, as.list(environment()))
}
