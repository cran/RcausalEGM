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
n <- 1000
p <- 200
v <- matrix(rexp(n * p), n, p)
rate <- v[,1] + v[,2]
scale = 1/rate
x = rexp(n=n, rate=rate)
y = rnorm(n=n, mean = x + (v[,1] + v[,3])*exp(-x * (v[,1] + v[,3])), sd=1)

## ----look---------------------------------------------------------------------
oldpar <-par(mfrow=c(1,3))
hist(x, breaks="FD", xlim=c(0,7), col="blue",xlab="x values")
hist(y, breaks="FD", xlim=c(0,7), col="red",xlab="y values")
boxplot(v[,1:5],main="First five covariates", xlab="Covariate index", ylab="v values")
par(oldpar)

## ----train--------------------------------------------------------------------
#help(causalegm)
model <- causalegm(x=x,y=y,v=v,
                   n_iter=2000, 
                   binary_treatment = FALSE, 
                   use_v_gan = FALSE, 
                   x_min=0, 
                   x_max=3)

## ----ace----------------------------------------------------------------------

ACE <- model$causal_pre
boxplot(ACE, main="ACE distribution", ylab="Values")

## ----adrf---------------------------------------------------------------------
grid_val = seq(0, 3, length.out=200)
true_effect = grid_val + 2 * (1+grid_val)**(-3)
plot(true_effect, col=2, xlab="Treatment", ylab="Average Dose Response")
lines(ACE, col=3)
legend(x = "topleft",          # Position
       legend = c("True ADRF", "Estimated ADRF"),  
       lty = c(1, 1),           # Line types
       col = c(2, 3),            # Line colors
       lwd = 2)  

