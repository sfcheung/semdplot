#' @title Diagnostic Information
#'
#' @description It computes diagnostic information such
#' residuals to be used by the plot functions.
#'
#' @details It receives the output of structural equation
#' model analysis (e.g, the output of [lavaan::sem()]) and
#' and computes diagnostic information to be used for the
#' diagnostics plots.
#'
#' It currently only supports the output of [lavaan::sem()]
#' and a single-sample model with observable variables only.
#'
#' @return
#' A `semdplot`-class object.
#'
#' TODO: Write methods for this class.
#'
#' @param fit Argument description.
#' @param add_path Logical. If a path is not in a model
#' (fixed to zero), will this path be added to the model
#' to generate the diagnostic information. Default is
#' `FALSE`.
#'
#' @examples
#' \donttest{
#' # TODO: Prepare some examples.
#' }
#'
#' @export
#'

fit_to_diag <- function(fit,
                        add_path = FALSE) {
    # It may support the output of other SEM functions in the future.
    if (inherits(fit, "lavaan")) {
        out <- fit_to_diag_lavaan(fit = fit,
                                  add_path = add_path)
      } else {
        stop("'fit' is not a supported object.")
      }
    return(out)
  }

fit_to_diag_lavaan <- function(fit,
                               add_path = FALSE) {
    # If the check fails, an error will be raised.
    check_fit_lavaan(fit)
    x <- lavaan::lavNames(fit, "eqs.x")
    y <- lavaan::lavNames(fit, "eqs.y")
    y_pure <- lavaan::lavNames(fit, "ov.y")
    est_beta <- lavaan::lavInspect(fit, what = "est")$beta
    est_beta <- est_beta[y, !colnames(est_beta) %in% y_pure]
    std_beta <- lavaan::lavInspect(fit, what = "std")$beta
    std_beta <- std_beta[y, !colnames(std_beta) %in% y_pure]
    free_beta <- lavaan::lavInspect(fit, what = "free")$beta
    free_beta <- free_beta[y, !colnames(free_beta) %in% y_pure]
    fit_data <- lavaan::lavInspect(fit, "data")
    fit_implied <- lavaan::lavInspect(fit, "implied")
    implied_mean <- fit_implied$mean
    implied_cov <- fit_implied$cov
    p <- ncol(est_beta)
    q <- nrow(est_beta)
    to_add <- lapply(seq_len(q), function(ii) {
        to_add2 <- lapply(seq_len(p), function(jj) {
            nature_ij(est_beta = est_beta,
                      i = ii,
                      j = jj,
                      fit = fit,
                      free_beta = free_beta)
          })
      })
    out <- lapply(seq_len(q), function(ii) {
            out2 <- lapply(seq_len(p), function(jj) {
                if (to_add[[ii]][[jj]] == "no") {
                    out3 <- partial_ij(est_beta = est_beta,
                                       i = ii,
                                       j = jj,
                                       fit_data = fit_data,
                                       implied_cov = implied_cov,
                                       free_beta = free_beta,
                                       std_beta = std_beta)
                    if (is.list(out3)) out3$type <- "partial"
                    return(out3)
                  }
                if (to_add[[ii]][[jj]] == "same") {
                    out3 <- resid_ij(est_beta = est_beta,
                                     i = ii,
                                     j = jj,
                                     fit_data = fit_data,
                                     implied_cov = implied_cov,
                                     free_beta = free_beta,
                                     std_beta = std_beta)
                    if (is.list(out3)) out3$type <- "residual"
                    return(out3)
                  }
                if ((to_add[[ii]][[jj]] == "yes") && add_path) {
                    xname <- colnames(est_beta)[[jj]]
                    yname <- rownames(est_beta)[[ii]]
                    fit_ij <- add_and_fit_lavaan(xname = xname,
                                                 yname = yname,
                                                 fit = fit)
                    x_ij <- lavaan::lavNames(fit_ij, "eqs.x")
                    y_ij <- lavaan::lavNames(fit_ij, "eqs.y")
                    y_pure_ij <- lavaan::lavNames(fit_ij, "ov.y")
                    est_beta_ij <- lavaan::lavInspect(fit_ij,
                                                      what = "est")$beta
                    est_beta_ij <- est_beta_ij[y_ij,
                                      !colnames(est_beta_ij) %in% y_pure_ij]
                    std_beta_ij <- lavaan::lavInspect(fit_ij,
                                                      what = "std")$beta
                    std_beta_ij <- std_beta_ij[y_ij,
                                      !colnames(std_beta_ij) %in% y_pure_ij]
                    free_beta_ij <- lavaan::lavInspect(fit_ij,
                                                       what = "free")$beta
                    free_beta_ij <- free_beta_ij[y_ij,
                                      !colnames(free_beta_ij) %in% y_pure_ij]
                    fit_implied_ij <- lavaan::lavInspect(fit_ij,
                                                         what = "implied")
                    implied_mean_ij <- fit_implied_ij$mean
                    implied_cov_ij <- fit_implied_ij$cov
                    out3 <- partial_ij(est_beta = est_beta_ij,
                                       i = ii,
                                       j = jj,
                                       fit_data = fit_data,
                                       implied_cov = implied_cov_ij,
                                       free_beta = free_beta_ij,
                                       std_beta = std_beta_ij)
                    if (is.list(out3)) out3$type <- "added"
                    return(out3)
                  }
                return(NA)
              })
          })
    class(out) <- c("semdplot", class(out))
    out
  }
