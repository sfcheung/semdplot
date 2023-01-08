# Create several datasets for testing purpose.
#

library(lavaan)

# A path analysis model

mod_p <-
"
m11 ~ 0.5*x1 + 0.6*x2 + .2*c1 + .3*c2
m12 ~ 0.4*m11 + (-0.3)*x2 + .1*c1 + .1*c2
m21 ~ 0.2*x2 + (-0.3)*x2
m22 ~ 0.3*m21 + 0.4*x1
y1 ~ 0.5*m12 + 0.2*m22 + .1*c2
y2 ~ 0.2*m12 + 0.5*m22 + .1*c1
"
mod <-
"
m11 ~ x1 + x2 + c1 + c2
m12 ~ m11 + c1 + c2
m21 ~ x1 + x2 + c1 + c2
m22 ~ m21 + c1 + c2
y1 ~ m12 + c1 + c2
y2 ~ m22 + c1 + c2
"

# generate data
set.seed(870986)
dat <- lavaan::simulateData(mod_p, sample.nobs = 200L)
fit <- lavaan::sem(mod, dat)
summary(fit)
dat_pa <- dat
usethis::use_data(dat_pa, overwrite = TRUE)
