#' @title Sample Data: A Path Model
#'
#' @description A 10-variable dataset with 200 cases.
#'
#' @format A data frame with 200 rows
#' and 10 variables:
#' \describe{
#'   \item{m11}{Mediator. Numeric.}
#'   \item{m12}{Mediator. Numeric.}
#'   \item{m21}{Mediator. Numeric.}
#'   \item{m22}{Mediator. Numeric.}
#'   \item{y1}{Outcome variable. Numeric.}
#'   \item{y2}{Outcome variable. Numeric.}
#'   \item{x1}{Predictor. Numeric.}
#'   \item{x2}{Predictor. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_pa)
#' mod <-
#' "
#' m11 ~ a11*x1 + a21*x2 + c1 + c2
#' m12 ~ b12*m11 + c1 + c2
#' m21 ~ a12*x1 + a22*x2 + c1 + c2
#' m22 ~ b22*m21 + c1 + c2
#' y1 ~ b12y1*m12 + c1 + c2
#' y2 ~ b22y2*m22 + c1 + c2
#' ind_1 := a11*b12*b12y1
#' ind_2 := a21*b22*b22y2
#' "
#' fit <- sem(mod, data_pa)
#' summary(fit, fit.measures = TRUE)
#'
"data_pa"
