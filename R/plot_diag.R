#' @title Diagnostic Plot Matrix
#'
#' @description It generate the diagnostic plots based
#' on the output of [fit_to_diag()].
#'
#' @details TODO: Should be converted to a method.
#'
#' It currently output of any function supported by
#' [fit_to_diag()].
#'
#' TODO: Allow users to customize the plot.
#'
#' @return
#' A `ggplot2` plot, or an array of `ggplot2` plots.
#'
#' @param diag_info Argument description.
#'
#' @param x Character. The `x`-variable.
#'
#' @param y Character. The `y`-variable.
#'
#' @examples
#' \donttest{
#' # TODO: Prepare some examples.
#' }
#'
#' @export
#'

plot_diag <- function(diag_info,
                      x = NULL,
                      y = NULL) {
    res0 <- partial_to_long(diag_info, what = "resid")
    std0 <- partial_to_long(diag_info, what = "std")
    xnames <- unique(std0$xname)
    ynames <- unique(std0$yname)
    if (!is.null(x)) {
        xnames <- intersect(xnames, x)
        xnames <- stats::na.omit(xnames[match(x, xnames)])
      }
    if (!is.null(y)) {
        ynames <- intersect(ynames, y)
        ynames <- stats::na.omit(ynames[match(y, ynames)])
      }
    if (length(xnames) == 0) stop("All x variable(s) not in the model.")
    if (length(ynames) == 0) stop("All y variable(s) not in the model.")
    p <- length(xnames)
    q <- length(ynames)
    ps <- lapply(ynames, function(yi) {
            lapply(xnames, function(xi) {
                res0i <- res0[(res0$xname == xi) &
                              (res0$yname == yi), ]
                std0i <- std0[(std0$xname == xi) &
                              (std0$yname == yi), ]
                if (nrow(res0i) == 0) {
                    return(grid::textGrob("N/A"))
                  }
                plot_diag_i(x = xi,
                            y = yi,
                            res = res0i,
                            std = std0i)
              })
          })
    out <- patchwork::wrap_plots(unlist(ps, recursive = FALSE),
                                 ncol = p,
                                 nrow = q)
    out
  }

#' @noRd
plot_diag_i <- function(res,
                        std,
                        x,
                        y) {
   if (std$type == "residual") {
        out <- plot_diag_i_resid(res = res,
                                 std = std,
                                 x = x,
                                 y = y)
        return(out)
        # return(grid::textGrob("Residual"))
      }
    if (std$type == "partial") {
        out <- plot_diag_i_partial(res = res,
                                   std = std,
                                   x = x,
                                   y = y,
                                   point_color = "black",
                                   line_color = "blue",
                                   loess_color = "black",
                                   bg_fill = "gray90")
        return(out)
        # return(grid::textGrob("Partial"))
      }
    if (std$type == "added") {
        out <- plot_diag_i_partial(res = res,
                                   std = std,
                                   x = x,
                                   y = y,
                                   point_color = "black",
                                   line_color = "red",
                                   loess_color = "black",
                                   bg_fill = "gray80")
        return(out)
        # return(grid::textGrob("Added"))
      }
    return(grid::textGrob("Unknown"))
  }

#' @noRd
plot_diag_i_partial <- function(res,
                                std,
                                x,
                                y,
                                line_color = "blue",
                                loess_color = "black",
                                point_color = "black",
                                bg_fill = "lightgrey") {
    tmp <- range(res$y_zresid)
    b_pos_y <- tmp[1] + .95 * (tmp[2] - tmp[1])
    p <- ggplot2::ggplot(data = res,
                         ggplot2::aes(x = x_zresid,
                                      y = y_zresid)) +
          ggplot2::xlab(x) +
          ggplot2::ylab(y) +
          ggplot2::geom_point(alpha = .5,
                              size = .5,
                              color = point_color) +
          ggplot2::geom_smooth(method = "loess",
                               formula = y ~ x,
                               linewidth = 1,
                               color = loess_color,
                               alpha = .75,
                               se = FALSE) +
          ggplot2::geom_abline(slope = std$b,
                               intercept = 0,
                               color = line_color,
                               linewidth = 1) +
          ggplot2::annotate("text",
                            x = mean(range(res$x_zresid)),
                            y = b_pos_y,
                            label = formatC(std$b,
                                            digits = 2,
                                            format = "f"),
                            size = 5) +
          ggplot2::theme(panel.background =
                          ggplot2::element_rect(fill = bg_fill))
    p
  }

#' @noRd
plot_diag_i_resid <- function(res,
                              std,
                              x,
                              y) {
    p <- ggplot2::ggplot(data = res,
                ggplot2::aes(x = y_zresid)) +
          ggplot2::geom_histogram(
                        ggplot2::aes(y = ggplot2::after_stat(density)),
                        color = "grey",
                        fill = "blue",
                        alpha = .5,
                        bins = 10) +
          ggplot2::geom_density(alpha = .5,
                                fill = "grey") +
          ggplot2::xlab(y)
    p
  }
