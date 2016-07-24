### Code skeleton for Series 8, task 2

## read in data
data(ozone, package = "gss")

###################################################
### TASK a)
###################################################

ozone$logupo3 <- log(ozone$upo3)
d.ozone <- subset(ozone, select=-upo3)
pairs(???, pch = ".",gap = 0.1)

## delete outlier
out <- ???
d.ozone.e <- d.ozone[-out,]



###################################################
### TASK b)
###################################################

## package for formula
require(sfsmisc)

## Linear models
## fit 1 (polynomial of degree 1)
form1 <- ???
fit1 <- ???



## fits of degree 2 to 5
form2 <- wrapFormula(???, ???, wrapString="poly(*,degree=2)")
fit2 <- ??? 
...


## GAM
require(mgcv)
gamForm <- wrapFormula(???,???)
g1 <- gam(???)


###################################################
### TASK c)
###################################################

## plot the fits

## for the linear models




## for the additive model

require(sfsmisc)
mult.fig(nr.plots = 9, main="gam(gamForm, data = d.ozone.e)")
plot(..., shade = TRUE)


###################################################
### TASK d)
###################################################


## Mallows Cp function

Cp <- function(object,sigma){
  res<-residuals(object)
  n <- length(res)
  p <- n-object$df.residual
  SSE <- sum(res^2)
  SSE/sigma^2-n+2*p
}

## set sigma (use estimated sigma from fit5)
sigma <- ???

## Calculate Mallows's Cp statistic for all 6 models
