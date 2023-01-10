#' @title Casewise Residuals
#'
#' @description It computes casewise residuals
#' to be used by the plot functions.
#'
#' @details It receives the output of structural equation
#' model analysis (e.g, the output of [lavaan::sem()]) and
#' and computes casewise residuals.
#'
#' It currently only supports the output of [lavaan::sem()]
#' and a single-sample model with observable variables only.
#'
#' @return
#' A `semdplot_residuals`-class object.
#'
#' TODO: Write methods for this class.
#'
#' @param fit The output of a structural equation model
#' analysis. Only support the output of [lavaan::sem()]
#' for now.
#'
#' @param add_path Logical. If a path is not in a model
#' (fixed to zero), whether this path will be added to
#' the model #' to compute the residuals. Default is
#' `FALSE`. Use this only if necessary because the model
#' will be fitted once for each such path.
#'
#' @examples
#' \donttest{
#' # TODO: Prepare some examples.
#' }
#'
#' @export
#'

casewise_residuals <- function(fit,
                               add_path = FALSE) {
    # It may support the output of other SEM functions in the future.
    if (inherits(fit, "lavaan")) {
        out <- casewise_residuals_lavaan(fit = fit,
                                  add_path = add_path)
      } else {
        stop("'fit' is not a supported object.")
      }
    return(out)
  }

casewise_residuals_lavaan <- function(fit,
                                      add_path = FALSE) {
    # If the check fails, an error will be raised.
    check_fit_lavaan(fit)
    fit_info <- c_resid_get_info_lavaan(fit)
    p <- ncol(fit_info$est_beta)
    q <- nrow(fit_info$est_beta)
    to_add <- lapply(seq_len(q), function(ii) {
        lapply(seq_len(p), function(jj) {
            nature_ij(est_beta = fit_info$est_beta,
                      i = ii,
                      j = jj,
                      free_beta = fit_info$free_beta)
          })
      })
    out <- lapply(seq_len(q), function(ii) {
            lapply(seq_len(p), function(jj) {
                if (to_add[[ii]][[jj]] == "no") {
                    out3 <- partial_ij(est_beta = fit_info$est_beta,
                                       i = ii,
                                       j = jj,
                                       fit_data = fit_info$fit_data,
                                       implied_cov = fit_info$implied_cov,
                                       free_beta = fit_info$free_beta,
                                       std_beta = fit_info$std_beta)
                    if (is.list(out3)) out3$type <- "partial"
                    return(out3)
                  }
                if (to_add[[ii]][[jj]] == "same") {
                    out3 <- resid_ij(est_beta = fit_info$est_beta,
                                     i = ii,
                                     j = jj,
                                     fit_data = fit_info$fit_data,
                                     implied_cov = fit_info$implied_cov,
                                     free_beta = fit_info$free_beta,
                                     std_beta = fit_info$std_beta)
                    if (is.list(out3)) out3$type <- "residual"
                    return(out3)
                  }
                if ((to_add[[ii]][[jj]] == "yes") && add_path) {
                    xname <- colnames(fit_info$est_beta)[[jj]]
                    yname <- rownames(fit_info$est_beta)[[ii]]
                    fit_ij <- add_and_fit_lavaan(xname = xname,
                                                 yname = yname,
                                                 fit = fit)
                    fit_info_ij <- c_resid_get_info_lavaan(fit_ij)
                    out3 <- partial_ij(est_beta = fit_info_ij$est_beta,
                                       i = ii,
                                       j = jj,
                                       fit_data = fit_info_ij$fit_data,
                                       implied_cov = fit_info_ij$implied_cov,
                                       free_beta = fit_info_ij$free_beta,
                                       std_beta = fit_info_ij$std_beta)
                    if (is.list(out3)) out3$type <- "added"
                    return(out3)
                  }
                return(NA)
              })
          })
    class(out) <- c("semdplot_residuals", class(out))
    out
  }


#' @title Gather Necessary Information For 'casewise_residuals()'
#'
#' @noRd

c_resid_get_info_lavaan <- function(fit) {
    out <- list()
    out$x <- lavaan::lavNames(fit, "eqs.x")
    out$y <- lavaan::lavNames(fit, "eqs.y")
    out$y_pure <- lavaan::lavNames(fit, "ov.y")
    est_beta <- lavaan::lavInspect(fit, what = "est")$beta
    out$est_beta <- est_beta[out$y, ]
    std_beta <- lavaan::lavInspect(fit, what = "std")$beta
    out$std_beta <- std_beta[out$y, ]
    free_beta <- lavaan::lavInspect(fit, what = "free")$beta
    out$free_beta <- free_beta[out$y, ]
    out$fit_data <- lavaan::lavInspect(fit, "data")
    fit_implied <- lavaan::lavInspect(fit, "implied")
    out$implied_mean <- fit_implied$mean
    out$implied_cov <- fit_implied$cov
    out
  }