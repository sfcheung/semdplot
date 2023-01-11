skip("WIP")
library(testthat)
library(semdplot)
suppressMessages(library(lavaan))

dat <- data_pa
mod <-
"
m11 ~ a11*x1 + a21*x2 + c1 + c2
m12 ~ b12*m11 + c1 + c2
m21 ~ a12*x1 + a22*x2 + c1 + c2
m22 ~ b22*m21 + c1 + c2
y1 ~ b12y1*m12 + c1 + c2
y2 ~ b22y2*m22 + c1 + c2
ind_1 := a11*b12*b12y1
ind_2 := a21*b22*b22y2
"
fit <- sem(mod, dat)
fit_res <- casewise_residuals(fit)

p <- plot(fit_res,
          x_names = c("x1", "m22", "y1", "y2"),
          y_names = c("m11", "y1"))
plot(p)

p <- plot(fit_res,
          what = "resid",
          y_names = c("m11", "y1"))
print(p)

# WIP Experiments

fit_res_y <- y_residuals(fit_res)
head(fit_res_y)

p <- ggplot2::ggplot(data = fit_res_y,
                     mapping = ggplot2::aes(x = m11, y = y1)) +
      ggplot2::geom_point() +
      ggplot2::stat_ellipse(type = "norm",
                            linetype = "dashed")
plot(p)
