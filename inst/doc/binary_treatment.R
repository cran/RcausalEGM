## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(fig.width=7, fig.height=4) 
set.seed(123)

## ----setup--------------------------------------------------------------------
library(RcausalEGM)

## ----check_pkg----------------------------------------------------------------
if (!(reticulate::py_module_available('CausalEGM'))){
  cat("## Please install the CausalEGM package using the function: install_causalegm()")
  knitr::knit_exit()
}

## ----simulation---------------------------------------------------------------
n <- 500
p <- 20
v <- matrix(rnorm(n * p), n, p)
x <- rbinom(n, 1, 0.3 + 0.2 * (v[, 1] > 0))
y <- pmax(v[, 1], 0) * x + v[, 2] + pmin(v[, 3], 0) + rnorm(n)

## ----look---------------------------------------------------------------------
oldpar <- par(mfrow=c(1,3))
slices <- c(sum(x==1), sum(x==0))
lbls <- c(paste("T group:",round(sum(x==1)*100/length(x), 2), "%", sep=""), paste("C group:",round(sum(x==0)*100/length(x), 2), "%", sep=""))
pie(slices, labels = lbls, main="Treatment Variables")
hist(y, breaks=12, col="red",xlab="y values")
boxplot(v[,1:5],main="First five covariates", xlab="Covariate index", ylab="v values")
par(oldpar)

## ----train--------------------------------------------------------------------
#help(causalegm)
model <- causalegm(x=x,y=y,v=v,n_iter=2000)

## ----extract------------------------------------------------------------------
ATE <- mean(model$causal_pre)
paste("The average treatment effect (ATE):", round(ATE, 3))
ITE <- model$causal_pre
boxplot(ITE, main="ITE distribution", ylab="Values")

## ----cf-----------------------------------------------------------------------
x_cf <- 1-x
y_cf <- get_est(model, v, x_cf)
boxplot(y_cf, main="Counterfactual outcome", ylab="Values")

## ----cate---------------------------------------------------------------------
n_test <- 100
v_test <- matrix(rnorm(n_test * p), n_test, p)
CATE <- get_est(model, v_test)
boxplot(CATE, main="CATE", ylab="Values")


