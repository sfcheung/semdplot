#' @title Residual Plot Matrix
#'
#' @description The plot method of a
#' `semdplot_residuals`-class object.
#' It generates the residual plots based
#' on the output of [casewise_residuals()].
#'
#' @details It currently supports the output of
#' [casewise_residuals()].
#'
#' @return
#' A `ggplot2` plot, or a list of `ggplot2` plots.
#'
#' @param x A `semdplot_residuals`-class object,
#' such as the output of [casewise_residuals()].
#' Named `x` as required for the `plot` method.
#'
#' @param what String. What to plot. Can be `"partial"`
#' or `"residual"`. if `"partial"`, the default,
#' a matrix of partial plots and/or plots of
#' residuals will be plotted. If `"residual"`,
#' then only the plots of residuals of
#' y-variable will be plotted.
#'
#' @param x_names Character vector. The `x`-variables. If
#' the vector is named, the names will be used instead
#' of the variable names in the plots.
#'
#' @param y_names Character vector. The `y`-variables. If
#' the vector is named, the names will be used instead
#' of the variable names in the plots.
#'
#' @param same_x_range Whether all plots will have the
#' same range for the x-axis. Default is `TRUE`.
#'
#' @param same_y_range Whether all plots will have the
#' same range for the y-axis, except for the plots of
#' residuals. Default is `TRUE`.
#'
#' @param partial_point_aes A list of argument for
#' [ggplot2::geom_point()]. Used to modify the default
#' aesthetics of the points in the partial plot.
#'
#' @param partial_loess_aes A list of argument for
#' [ggplot2::geom_line()] for the loess line.
#' Used to modify the default
#' aesthetics of the line in the partial plot.
#'
#' @param partial_reg_aes A list of argument for
#' [ggplot2::geom_line()] for the regression line.
#' Used to modify the default
#' aesthetics of the line in the partial plot.
#'
#' @param partial_b_aes A list of argument for
#' [ggplot2::annotate()] for the standardized
#' regression coefficient.
#' Used to modify the default
#' aesthetics of the text in the partial plot.
#'
#' @param partial_theme_aes A list of argument for
#' [ggplot2::theme()] for the partial plots.
#' Used to modify the default
#' theme of the partial plot.
#'
#' @param added_point_aes A list of argument for
#' [ggplot2::geom_point()]. Used to modify the default
#' aesthetics of the points in the added-variable plot.
#'
#' @param added_loess_aes A list of argument for
#' [ggplot2::geom_line()] for the loess line.
#' Used to modify the default
#' aesthetics of the line in the added-variable plot.
#'
#' @param added_reg_aes A list of argument for
#' [ggplot2::geom_line()] for the regression line.
#' Used to modify the default
#' aesthetics of the line in the added-variable plot.
#'
#' @param added_b_aes A list of argument for
#' [ggplot2::annotate()] for the standardized
#' regression coefficient.
#' Used to modify the default
#' aesthetics of the text in the added-variable plot.
#'
#' @param added_theme_aes A list of argument for
#' [ggplot2::theme()] for the added-variable plots.
#' Used to modify the default
#' theme of the added-variable plot.
#'
#' @param resid_hist_aes A list of arguments for
#' [ggplot2::geom_histogram()] for the plot of residuals.
#' Use to modify the default aesthetics of the histogram.
#'
#' @param resid_density_aes A list of arguments for
#' [ggplot2::geom_density()] for the plot of residuals.
#' Use to modify the default aesthetics of the density plot.
#'
#' @param resid_theme_aes A list of argument for
#' [ggplot2::theme()] for the plots of residuals.
#' Used to modify the default
#' theme of the plot of residual.
#'
#' @param text_nopath String. Text to display in empty
#' cells. Default is`"--".
#'
#' @param output_type The type of output. If `"plot"`,
#' the default, the plots will be processed by
#' [patchwork::wrap_plots()]. If `"list"`, the list
#' of plots will be returned.
#'
#' @param ... Additional arguments. Not used.
#'
#' @examples
#' \donttest{
#' # TODO: Prepare some examples.
#' }
#'
#' @export
#'
#' @importFrom rlang .data

plot.semdplot_residuals <- function(x,
                                    what = c("partial", "residual"),
                                    x_names = NULL,
                                    y_names = NULL,
                                    same_x_range = TRUE,
                                    same_y_range = TRUE,
                                    partial_point_aes = list(),
                                    partial_loess_aes = list(),
                                    partial_reg_aes = list(),
                                    partial_b_aes = list(),
                                    partial_theme_aes = list(),
                                    added_point_aes = list(),
                                    added_loess_aes = list(),
                                    added_reg_aes = list(),
                                    added_b_aes = list(),
                                    added_theme_aes = list(),
                                    resid_hist_aes = list(),
                                    resid_density_aes = list(),
                                    resid_theme_aes = list(),
                                    text_nopath = "--",
                                    output_type = c("plot", "list"),
                                    ...
                                  ) {
    what <- match.arg(what)
    output_type <- match.arg(output_type)
    if (what == "residual") {
        out <- plot_resid(c_resid = x,
                          y = y_names,
                          progress = FALSE)
        return(out)
      }
    res0 <- partial_to_long(partial_list = x,
                            what = "resid")
    std0 <- partial_to_long(partial_list = x,
                            what = "std")
    xnames <- unique(std0$xname)
    ynames <- unique(std0$yname)
    if (!is.null(x_names)) {
        xnames <- intersect(xnames, x_names)
        xnames <- stats::na.omit(xnames[match(x_names, xnames)])
      }
    if (!is.null(y_names)) {
        ynames <- intersect(ynames, y_names)
        ynames <- stats::na.omit(ynames[match(y_names, ynames)])
      }
    if (length(xnames) == 0) stop("All x variable(s) not in the model.")
    if (length(ynames) == 0) stop("All y variable(s) not in the model.")
    if (!is.null(names(x_names))) {
        xlabels <- names(x_names)[match(xnames, x_names)]
        names(xlabels) <- xnames
      } else {
        xlabels <- NULL
      }
    if (!is.null(names(y_names))) {
        ylabels <- names(y_names)[match(ynames, y_names)]
        names(ylabels) <- ynames
      } else {
        ylabels <- NULL
      }
    p <- length(xnames)
    q <- length(ynames)

    # Update the aesthetics arguments
    pq_range <- range(p, q)
    x_title_size <-  min(1, 0.75 * 6 / max(pq_range))
    y_title_size <-  min(1, 0.75 * 6 / max(pq_range))
    x_text_size <-  min(1, 0.75 * 6 / max(pq_range))
    y_text_size <-  min(1, 0.75 * 6 / max(pq_range))
    b_size <- min(18, 3 * 6 / max(pq_range))
    point_size <- min(4, .5 * 6 / max(pq_range))
    partial_point_aes_def <- list(na.rm = TRUE,
                                  size = ggplot2::rel(point_size))
    partial_point_aes <- utils::modifyList(partial_point_aes_def,
                                           partial_point_aes)
    added_point_aes <- utils::modifyList(partial_point_aes_def,
                                         added_point_aes)
    partial_loess_aes_def <- list(na.rm = TRUE,
                                  color = "darkgreen")
    partial_loess_aes <- utils::modifyList(partial_loess_aes_def,
                                           partial_loess_aes)
    partial_reg_aes_def <- list(na.rm = TRUE,
                                color = "blue")
    partial_reg_aes <- utils::modifyList(partial_reg_aes_def,
                                         partial_reg_aes)
    partial_b_aes_def <- list(geom = "text",
                              color = "blue",
                              size = ggplot2::rel(b_size))
    partial_b_aes <- utils::modifyList(partial_b_aes_def,
                                       partial_b_aes)
    partial_theme_aes_def <- list(axis.title.x =
                                    ggplot2::element_text(size =
                                      ggplot2::rel(x_title_size)),
                                  axis.title.y =
                                    ggplot2::element_text(size =
                                      ggplot2::rel(x_text_size)),
                                  axis.text.x =
                                    ggplot2::element_text(size =
                                      ggplot2::rel(y_title_size)),
                                  axis.text.y =
                                    ggplot2::element_text(size =
                                      ggplot2::rel(y_text_size)),
                                  panel.background =
                                    ggplot2::element_rect(fill = "grey90"),
                                  plot.background =
                                    ggplot2::element_rect(color = "grey90",
                                                          fill = NA,
                                                          size = ggplot2::rel(.1)))
    partial_theme_aes <- utils::modifyList(partial_theme_aes_def,
                                           partial_theme_aes)
    added_loess_aes <- utils::modifyList(partial_loess_aes_def,
                                         added_loess_aes)
    added_reg_aes_def <- utils::modifyList(partial_reg_aes_def,
                                           list(color = "red"))
    added_reg_aes <- utils::modifyList(added_reg_aes_def,
                                       added_reg_aes)
    added_b_aes_def <- utils::modifyList(partial_b_aes_def,
                                         list(color = "red"))
    added_b_aes <- utils::modifyList(added_b_aes_def,
                                    added_b_aes)
    added_theme_aes_def <- utils::modifyList(partial_theme_aes_def,
                                             list(panel.background =
                                                    ggplot2::element_rect(
                                                      fill = "grey80")))
    added_theme_aes <- utils::modifyList(added_theme_aes_def,
                                         added_theme_aes)
    resid_hist_aes_def <- list(na.rm = TRUE)
    resid_hist_aes <- utils::modifyList(resid_hist_aes_def,
                                        resid_hist_aes)
    resid_density_aes_def <- list(fill = "green",
                                  na.rm = TRUE)
    resid_density_aes <- utils::modifyList(resid_density_aes_def,
                                           resid_density_aes)
    resid_theme_aes <- utils::modifyList(partial_theme_aes_def,
                                         resid_theme_aes)

    res0 <- res0[(res0$xname %in% x_names) |
                 (res0$yname %in% y_names), ]
    all_ranges <- get_xy_range(res0)
    if (same_x_range) {
        xlim_i <- all_ranges$zx_range
      } else {
        xlim_i <- NULL
      }
    if (same_y_range) {
        ylim_i <- all_ranges$zy_range
      } else {
        ylim_i <- NULL
      }
    ps <- lapply(ynames, function(yi) {
            lapply(xnames, function(xi) {
                res0i <- res0[(res0$xname == xi) &
                              (res0$yname == yi), ]
                std0i <- std0[(std0$xname == xi) &
                              (std0$yname == yi), ]
                # if ((xi == "x1") && (yi == "m12")) browser()
                if (nrow(res0i) == 0) {
                    return(grid::textGrob(text_nopath))
                  }
                plot_cres_i(x = xi,
                            y = yi,
                            res = res0i,
                            std = std0i,
                            partial_xlim = xlim_i,
                            partial_ylim = ylim_i,
                            added_xlim = xlim_i,
                            added_ylim = ylim_i,
                            resid_xlim = xlim_i,
                            partial_point_aes = partial_point_aes,
                            partial_loess_aes = partial_loess_aes,
                            partial_reg_aes = partial_reg_aes,
                            partial_b_aes = partial_b_aes,
                            partial_theme_aes = partial_theme_aes,
                            added_point_aes = added_point_aes,
                            added_loess_aes = added_loess_aes,
                            added_reg_aes = added_reg_aes,
                            added_b_aes = added_b_aes,
                            added_theme_aes = added_theme_aes,
                            resid_hist_aes = resid_hist_aes,
                            resid_density_aes = resid_density_aes,
                            resid_theme_aes = resid_theme_aes,
                            xlabel = xlabels[xi],
                            ylabel = ylabels[yi]
                           )
              })
          })
    if (output_type == "plot") {
        out <- patchwork::wrap_plots(unlist(ps, recursive = FALSE),
                                    ncol = p,
                                    nrow = q)
        return(out)
      }
    if (output_type == "list") {
        out <- unlist(ps, recursive = FALSE)
        return(out)
      }
  }

#' @title Residual-Only Plot Matrix
#'
#' @description It generate the plots of residuals based
#' on the output of [casewise_residuals()].
#'
#' @details TODO: Should be converted to a method.
#'
#' It currently supports an output of any function
#' supported by [casewise_residuals()].
#'
#' TODO: Allow users to customize the plot.
#'
#' @return
#' A `gg`-class object generated by [GGally::ggpairs()].
#' Note that [print()] is used to "plot" the plot.
#'
#' @param c_resid Argument description.
#'
#' @param y Character. The `y`-variable.
#'
#' @param progress To be passed to [GGally::ggpairs()]
#'
#' @param ... Arguments to be passed to [GGally::ggpairs()].
#'
#' @examples
#' \donttest{
#' # TODO: Prepare some examples.
#' }
#'
#' @export
#'

plot_resid <- function(c_resid,
                       y = NULL,
                       progress = FALSE,
                       ...
                     ) {
    res0 <- y_residuals(c_resid)
    ynames <- colnames(res0)
    if (!is.null(y)) {
        ynames <- intersect(ynames, y)
        ynames <- stats::na.omit(ynames[match(y, ynames)])
      }
    if (length(ynames) == 0) stop("All y variable(s) not in the model.")
    q <- length(ynames)
    out <- GGally::ggpairs(data = res0[, ynames],
                           progress = progress,
                           ...)
    out
  }


#' @noRd
get_xy_range <- function(res) {
    tmp <- res[res$xname != res$yname, ]
    x_range <- range(tmp$x_resid, na.rm = TRUE)
    y_range <- range(tmp$y_resid, na.rm = TRUE)
    zx_range <- range(tmp$x_zresid, na.rm = TRUE)
    zy_range <- range(tmp$y_zresid, na.rm = TRUE)
    list(x_range = x_range,
         y_range = y_range,
         zx_range = zx_range,
         zy_range = zy_range)
  }

#' @noRd
plot_cres_i <- function(res,
                        std,
                        x,
                        y,
                        partial_xlim = NULL,
                        partial_ylim = NULL,
                        added_xlim = NULL,
                        added_ylim = NULL,
                        resid_xlim = NULL,
                        partial_point_aes = list(),
                        partial_loess_aes = list(),
                        partial_reg_aes = list(),
                        partial_b_aes = list(),
                        partial_theme_aes = list(),
                        added_point_aes = list(),
                        added_loess_aes = list(),
                        added_reg_aes = list(),
                        added_b_aes = list(),
                        added_theme_aes = list(),
                        resid_hist_aes = list(),
                        resid_density_aes = list(),
                        resid_theme_aes = list(),
                        xlabel = NULL,
                        ylabel = NULL
                       ) {
   if (std$type == "residual") {
        out <- plot_cres_i_resid(res = res,
                                 std = std,
                                 x = x,
                                 y = y,
                                 resid_xlim = resid_xlim,
                                 resid_hist_aes = resid_hist_aes,
                                 resid_density_aes = resid_density_aes,
                                 resid_theme_aes = resid_theme_aes,
                                 ylabel = ylabel
                                )
        return(out)
        # return(grid::textGrob("Residual"))
      }
    if (std$type == "partial") {
        out <- plot_cres_i_partial(res = res,
                                   std = std,
                                   x = x,
                                   y = y,
                                   partial_xlim = partial_xlim,
                                   partial_ylim = partial_ylim,
                                   partial_point_aes = partial_point_aes,
                                   partial_loess_aes = partial_loess_aes,
                                   partial_reg_aes = partial_reg_aes,
                                   partial_b_aes = partial_b_aes,
                                   partial_theme_aes = partial_theme_aes,
                                   xlabel = xlabel,
                                   ylabel = ylabel
                                  )
        return(out)
        # return(grid::textGrob("Partial"))
      }
    if (std$type == "added") {
        out <- plot_cres_i_partial(res = res,
                                   std = std,
                                   x = x,
                                   y = y,
                                   partial_xlim = added_xlim,
                                   partial_ylim = added_ylim,
                                   partial_point_aes = added_point_aes,
                                   partial_loess_aes = added_loess_aes,
                                   partial_reg_aes = added_reg_aes,
                                   partial_b_aes = added_b_aes,
                                   partial_theme_aes = added_theme_aes,
                                   xlabel = xlabel,
                                   ylabel = ylabel
                                  )
        return(out)
        # return(grid::textGrob("Added"))
      }
    return(grid::textGrob("Unknown"))
  }

#' @noRd
plot_cres_i_partial <- function(res,
                                std,
                                x,
                                y,
                                partial_xlim = NULL,
                                partial_ylim = NULL,
                                partial_point_aes = list(),
                                partial_loess_aes = list(),
                                partial_reg_aes = list(),
                                partial_b_aes = list(),
                                partial_theme_aes = list(),
                                xlabel = NULL,
                                ylabel = NULL
                               ) {
    if (!is.null(partial_ylim)) {
        b_pos_y <- partial_ylim[1] + .95 * (partial_ylim[2] - partial_ylim[1])
      } else {
        tmp <- range(res$y_zresid, na.rm = TRUE)
        b_pos_y <- tmp[1] + .95 * (tmp[2] - tmp[1])
      }
    if (!is.null(partial_xlim)) {
        b_pos_x <- mean(partial_xlim)
      } else {
        b_pos_x <- mean(res$x_zresid, na.rm = TRUE)
      }
    p <- ggplot2::ggplot(data = res,
                         ggplot2::aes(x = .data$x_zresid,
                                      y = .data$y_zresid)) +
          ggplot2::xlab(x) +
          ggplot2::ylab(y)
    tmp <- utils::modifyList(list(alpha = .5,
                                  size = .5,
                                  color = "black"),
                             partial_point_aes)
    p <- p + do.call(ggplot2::geom_point, tmp)
    tmp <- utils::modifyList(list(method = "loess",
                                  formula = y ~ x,
                                  linewidth = 1,
                                  color = "black",
                                  alpha = .75,
                                  se = FALSE),
                             partial_loess_aes)
    p <- p + do.call(ggplot2::geom_smooth, tmp)
    tmp <- utils::modifyList(list(slope = std$b,
                                  intercept = 0,
                                  color = "blue",
                                  linewidth = 1),
                             partial_reg_aes)
    p <- p + do.call(ggplot2::geom_abline, tmp)
    tmp <- utils::modifyList(list(geom = "text",
                                  x = b_pos_x,
                                  y = b_pos_y,
                                  label = formatC(std$b,
                                                  digits = 2,
                                                  format = "f"),
                                  size = 5),
                             partial_b_aes)
    p <- p + do.call(ggplot2::annotate, tmp)
    tmp <- utils::modifyList(list(panel.background =
                                    ggplot2::element_rect(fill = "grey90")),
                             partial_theme_aes)
    p <- p + do.call(ggplot2::theme, tmp)
    if (!is.null(partial_xlim)) p <- p + ggplot2::xlim(partial_xlim)
    if (!is.null(partial_ylim)) p <- p + ggplot2::ylim(partial_ylim)
    if (!is.null(xlabel)) p <- p + ggplot2::xlab(xlabel)
    if (!is.null(ylabel)) p <- p + ggplot2::ylab(ylabel)
    p
  }

#' @noRd
plot_cres_i_resid <- function(res,
                              std,
                              x,
                              y,
                              resid_xlim = NULL,
                              resid_hist_aes = list(),
                              resid_density_aes = list(),
                              resid_theme_aes = list(),
                              ylabel = NULL
                             ) {
    p <- ggplot2::ggplot(data = res,
                ggplot2::aes(x = .data$y_zresid))
    tmp <- utils::modifyList(list(ggplot2::aes(y =
                                    ggplot2::after_stat(density)),
                                  color = "grey",
                                  fill = "blue",
                                  alpha = .5,
                                  bins = 10),
                             resid_hist_aes)
    p <- p + do.call(ggplot2::geom_histogram, tmp)
    tmp <- utils::modifyList(list(alpha = .5,
                                  fill = "grey"),
                             resid_density_aes)
    p <- p + do.call(ggplot2::geom_density, tmp)
    p <- p + ggplot2::xlab(y)
    tmp <- utils::modifyList(list(),
                             resid_theme_aes)
    p <- p + do.call(ggplot2::theme, tmp)
    if (!is.null(resid_xlim)) p <- p + ggplot2::xlim(resid_xlim)
    p
  }
