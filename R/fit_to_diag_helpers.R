#' @title Refitting a Model by Adding a Path
#'
#' @description It adds a path, if feasible, and refits
#' a model.
#'
#' @details It will disable `se` and `test` because only the
#' path estimates are needed.
#'
#' @return
#' It always the output of the refitted model.
#'
#' @param yname Character. The name of the `y`-variable.
#'
#' @param xname Character. The name of the `x`-variable.
#'
#' @param fit The original SEM output, such as the output
#' of [lavaan::sem()].
#'
#' @examples
#' \donttest{
#' # TODO: Prepare some examples.
#' }
#'
#' @noRd
#'
add_and_fit_lavaan <- function(yname,
                               xname,
                               fit) {
    slot_opt <- fit@Options
    slot_pat <- data.frame(fit@ParTable)
    data_full <- lavaan::lavInspect(fit, "data")
    #slot_pat$est <- NULL
    #slot_pat$start <- NULL
    pat_to_add <- lavaan::lavaanify(paste(yname, "~", xname))
    slot_pat_new <- lavaan::lav_partable_merge(pt1 = slot_pat,
                                               pt2 = pat_to_add,
                                               remove.duplicated = TRUE,
                                               warn = FALSE)
    slot_opt$se <- "none"
    slot_opt$test <- "none"
    fit_new <- lavaan::lavaan(data = data_full,
                              model = slot_pat_new,
                              slotOptions = slot_opt)
    # TODO: Handle cases in which the model fit failed.
    fit_new
  }

#' @title Nature of a Path
#'
#' @description Checks the nature a cell in the `beta`
#' model matrix.
#'
#' @details It is used to determine what to do with a
#' cell in the `beta` model matrix.
#'
#' This function is not limited to [lavaan::sem()] output.
#'
#' TODO: Describes the rules.
#'
#' @return
#' It returns a character string of one of these values:
#' `"same"`, `"x_not_x"`, `"yes"`, and `"no"`.
#'
#' @param est_beta The `beta` matrix storing the estimates
#' of the regression coefficients.
#'
#' @param i Numeric. The row number.
#'
#' @param j Numeric. The column number.
#'
#' @param fit The original SEM output, such as the output
#' of [lavaan::sem()].
#'
#' @param free_beta The `beta` matrix storing indicating
#' whether a path is free.
#'
#' @examples
#' \donttest{
#' # TODO: Prepare some examples.
#' }
#'
#' @noRd
#'
nature_ij <- function(est_beta,
                      i, j,
                      fit,
                      free_beta) {
    cnames <- colnames(est_beta)
    rnames <- rownames(est_beta)
    yname <- rnames[i]
    xname <- cnames[j]
    if (identical(xname, yname)) {
        return("same")
      }
    if (!(xname %in% cnames)) {
        return("x_not_x")
      }
    beta_y_all <- est_beta[i, , drop = FALSE]
    free_y_all <- free_beta[i, , drop = FALSE]
    if (!((free_y_all[, xname] == 0) && (beta_y_all[, xname] == 0))) {
        return("no")
      }
    if (xname %in% lavaan::lavNames(fit, "ov.y")) {
        return("no")
      }
    if ((xname %in% rnames) && (yname %in% cnames)) {
        beta_x_all <- est_beta[xname, , drop = FALSE]
        free_x_all <- free_beta[xname, , drop = FALSE]
        if ((free_x_all[, yname] != 0) || (beta_x_all[, yname] != 0)) {
            return("no")
          }
      }
    return("yes")
  }

#' @title Partial Plot
#'
#' @description It generates the partial plot for an
#' `x`-variable and a `y`-variable.
#'
#' @details It uses a method similar to the partial
#' plot in linear regression.
#'
#' This function is not limited to [lavaan::sem()] output.
#'
#' TODO: Describes the method.
#'
#' @return
#' It returns a `ggplot2` plot.
#'
#' @param est_beta The `beta` matrix storing the estimates
#' of the regression coefficients.
#'
#' @param i Numeric. The row number.
#'
#' @param j Numeric. The column number.
#'
#' @param fit_data A matrix of the data. The output of
#' [lavaan::lavInspect()] with `what` set to `"data"`.
#'
#' @param free_beta The `beta` matrix storing indicating
#' whether a path is free.
#'
#' @param implied_cov The implied covariance matrix of
#' the variables.
#'
#' @param implied_mean The implied mean vector of
#' the variables. Not used for now.
#'
#' @param std_beta The `beta` matrix storing the
#' standardized estimates of the regression coefficients.
#'
#' @examples
#' \donttest{
#' # TODO: Prepare some examples.
#' }
#'
#' @noRd
#'
partial_ij <- function(est_beta,
                       i, j,
                       fit_data,
                       free_beta,
                       implied_cov,
                       implied_mean,
                       std_beta) {
    cnames <- colnames(est_beta)
    rnames <- rownames(est_beta)
    yname <- rnames[i]
    xname <- cnames[j]
    if (identical(xname, yname)) {
        return(NA)
      }
    if (!(xname %in% cnames)) {
        return(NA)
      }
    beta_y_all <- est_beta[i, , drop = FALSE]
    free_y_all <- free_beta[i, , drop = FALSE]
    if ((free_y_all[, xname] == 0) && (beta_y_all[, xname] == 0)) {
        return(NA)
      }
    y_xx_all_idx <- (free_y_all != 0) | (beta_y_all != 0)
    y_xx_all_names <- cnames[y_xx_all_idx]
    x_x <- setdiff(y_xx_all_names, c(yname, xname))
    y_x <- setdiff(y_xx_all_names, c(yname, xname))
    x_obs <- fit_data[, xname]
    y_obs <- fit_data[, yname]
    est_x <- est_beta[yname, xname]
    std_x <- std_beta[yname, xname]
    if (length(x_x) > 0) {
        x_x_b <- solve(implied_cov[x_x, x_x]) %*% implied_cov[x_x, xname, drop = FALSE]
        x_hat <- as.vector(fit_data[, x_x, drop = FALSE] %*% x_x_b)
        x_resid <- x_obs - x_hat
      } else {
        x_x_b <- matrix(1, 1, 1)
        x_resid <- x_obs
        x_hat <- rep(0, length(x_resid))
      }
    if (length(y_x) > 0) {
        y_x_b <- solve(implied_cov[y_x, y_x]) %*% implied_cov[y_x, yname, drop = FALSE]
        y_hat <- as.vector(fit_data[, y_x, drop = FALSE] %*% y_x_b)
        y_resid <- y_obs - y_hat
      } else {
        y_x_b <- matrix(1, 1, 1)
        y_resid <- y_obs
        y_hat <- rep(0, length(y_resid))
      }
    list(x_resid = x_resid,
         y_resid = y_resid,
         x_hat = x_hat,
         y_hat = y_hat,
         x_b = x_x_b,
         y_b = y_x_b,
         xname = xname,
         yname = yname,
         est_x = est_x,
         std_x = std_x)
  }

#' @title Residual Plot
#'
#' @description It generates the plot for the residuals
#' of a `y`-variable.
#'
#' @details It plots a histogram, with an density plot
#' imposed.
#'
#' This function is not limited to [lavaan::sem()] output.
#'
#' TODO: Describes the method.
#'
#' @return
#' It returns a `ggplot2` plot.
#'
#' @param est_beta The `beta` matrix storing the estimates
#' of the regression coefficients.
#'
#' @param i Numeric. The row number.
#'
#' @param j Numeric. The column number.
#'
#' @param fit_data A matrix of the data. The output of
#' [lavaan::lavInspect()] with `what` set to `"data"`.
#'
#' @param free_beta The `beta` matrix storing indicating
#' whether a path is free.
#'
#' @param implied_cov The implied covariance matrix of
#' the variables.
#'
#' @param implied_mean The implied mean vector of
#' the variables. Not used for now.
#'
#' @param std_beta The `beta` matrix storing the
#' standardized estimates of the regression coefficients.
#'
#' @examples
#' \donttest{
#' # TODO: Prepare some examples.
#' }
#'
#' @noRd
#'
resid_ij <- function(est_beta,
                     i, j,
                     fit_data,
                     free_beta,
                     implied_cov,
                     implied_mean,
                     std_beta) {
    cnames <- colnames(est_beta)
    rnames <- rownames(est_beta)
    yname <- rnames[i]
    xname <- cnames[j]
    if (!identical(xname, yname)) {
        stop("x and y must be the same to compute residuals.")
      }
    if (!(xname %in% cnames) || !(yname %in% rnames)) {
        stop("x and/or y not in the regression table.")
      }
    y_x <- setdiff(cnames, c(yname, xname))
    beta_y_all <- t(est_beta[i, -j, drop = FALSE])
    y_obs <- fit_data[, yname]
    y_hat <- as.vector(fit_data[, y_x, drop = FALSE] %*% beta_y_all)
    y_resid <- y_obs - y_hat
    n <- length(y_resid)
    list(x_resid = rep(NA, n),
         y_resid = y_resid,
         x_hat = rep(NA, n),
         y_hat = y_hat,
         x_b = rep(NA, length(as.vector(beta_y_all))),
         y_b = beta_y_all,
         xname = xname,
         yname = yname,
         est_x = NA,
         std_x = NA)
  }

