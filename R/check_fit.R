#' @title Check The Fit Object
#'
#' @description Checks whether the fit object is supported.
#'
#' @details It receives the output of structural equation
#' model analysis and check whether it is supported by
#' [fit_to_diag()].
#'
#' @return
#' It always return `NULL`. If the fit object is not supported,
#' it will raise an error.
#'
#' @param fit The fit object to be checked.
#'
#' @examples
#' \donttest{
#' # TODO: Prepare some examples.
#' }
#'
#' @noRd
#'

check_fit_lavaan <- function(fit) {
    if (length(lavaan::lavNames(fit, "lv")) > 0) {
        stop("Does not support a model with latent variables.")
      }
    if (lavaan::lavInspect(fit, what = "ngroups") > 1) {
        stop("Does not support a model with more than one group.")
      }
    if (!identical(lavaan::lavInspect(fit, what = "ordered"), character(0))) {
        stop("Does not support a model with ordered variables.")
      }
    return(NULL)
  }
