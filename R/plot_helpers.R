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
