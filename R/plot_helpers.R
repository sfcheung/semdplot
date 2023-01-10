# Convert the list-of-lists to a data frame.

#' @noRd

partial_to_long <- function(partial_list, what = c("resid", "est", "std")) {
    what <- match.arg(what)
    partial0 <- unlist(partial_list, recursive = FALSE)
    out0 <- lapply(partial0, partial_to_long_i,
                   what = what)
    out <- do.call(rbind, out0)
    out
  }

#' @noRd

partial_to_long_i <- function(partial_i, what) {
    if (isTRUE(is.na(partial_i))) {
        return(NULL)
      }
    xname <- partial_i$xname
    yname <- partial_i$yname
    if (what == "resid") {
        x_resid <- partial_i$x_resid
        y_resid <- partial_i$y_resid
        x_zresid <- as.vector(scale(x_resid))
        y_zresid <- as.vector(scale(y_resid))
        out <- data.frame(xname = xname,
                          yname = yname,
                          x_resid = x_resid,
                          y_resid = y_resid,
                          x_zresid = x_zresid,
                          y_zresid = y_zresid)
        return(out)
      }
    if ((what == "est") || (what == "std")) {
        out <- data.frame(xname = xname,
                          yname = yname,
                          b = switch(what,
                                     est = partial_i$est_x,
                                     std = partial_i$std_x),
                          type = partial_i$type)
        return(out)
      }
    stop("Invalid request.")
  }

#' @title Residual-Only
#'
#' @description It extracts the residuals from
#' the output of [casewise_residuals()].
#'
#' @details
#'
#' It currently supports an output of any function
#' supported by [casewise_residuals()].
#'
#' TODO: Allow users to customize the plot.
#'
#' @return
#' A data frame
#'
#' @param diag_info Argument description.
#'
#' @examples
#' \donttest{
#' # TODO: Prepare some examples.
#' }
#'
#' @export
#'

y_residuals <- function(diag_info) {
    partial0 <- sapply(unlist(diag_info, recursive = FALSE),
                       function(x) {
                           if (is.list(x)) {
                               if (x$type == "residual") {
                                   return(x)
                                 } else {
                                   return(NULL)
                                 }
                             } else {
                               return(NULL)
                             }
                         })
    partial0 <- partial0[!sapply(partial0, is.null)]
    partial1 <- lapply(partial0, function(x) {
                    out <- data.frame(x$y_resid)
                    colnames(out) <- x$yname
                    out
                  })
    out <- as.data.frame(partial1)
    out
  }