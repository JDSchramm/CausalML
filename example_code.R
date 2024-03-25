# example code for PLR

rm(list = ls())

library(DoubleML)
library(mlr3)
library(mlr3learners)
library(ranger)
library(data.table)
lgr::get_logger("mlr3")$set_threshold("warn")

learner = lrn("regr.ranger", num.trees = 100, mtry = 20, min.node.size = 2, max.depth = 5)
ml_l = learner$clone()
ml_m = learner$clone()
set.seed(1111)
data = make_plr_CCDDHNR2018(alpha=0.5, n_obs=500, dim_x=20, return_type='data.table')
obj_dml_data = DoubleMLData$new(data, y_col="y", d_cols="d")
dml_plr_obj = DoubleMLPLR$new(obj_dml_data, ml_l, ml_m)
dml_plr_obj$fit()
print(dml_plr_obj)

summary(data)


# Example code for using grf to estimate heterogenous treatment effects

n <- 2000
p <- 10
X <- matrix(rnorm(n * p), n, p)
X.test <- matrix(0, 101, p)
X.test[, 1] <- seq(-2, 2, length.out = 101)

W <- rbinom(n, 1, 0.4 + 0.2 * (X[, 1] > 0))
Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)
tau.forest <- grf::causal_forest(X, Y, W)
tau.forest

grf::average_treatment_effect(tau.forest, target.sample = "all")
