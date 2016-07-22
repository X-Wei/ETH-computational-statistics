### Code skeleton for Series 5, task 2

## Read in dataset
diabetes <-
  read.table("http://stat.ethz.ch/Teaching/Datasets/diabetes2.dat",
             header = TRUE)
reg <- diabetes[, c("Age", "C.Peptide")]
names(reg) <-     c("x",   "y")

## Sort values
reg <- reg[sort.list(reg$x), ]

###################################################
### TASK a)
###################################################

### Utility function for LOO cross-validation:

##' Calculates the LOO CV score for given data and regression prediction function
##'
##' @param reg.data: regression data; data.frame with columns 'x', 'y'
##' @param reg.fcn:  regr.prediction function; arguments:
##'                    reg.x: regression x-values
##'                    reg.y: regression y-values
##'                    x:     x-value(s) of evaluation point(s)
##'                  value: prediction at point(s) x
##' @return LOOCV score
loocv <- function(reg.data, reg.fcn)
{
  ## Help function to calculate leave-one-out regression values
  loo.reg.value <- function(i, reg.data, reg.fcn)
    return(reg.fcn(???, ???, ???))

  ## Calculate LOO regression values using the help function above
  n <- nrow(reg.data)
  loo.values <- sapply(???, loo.reg.value, reg.data, reg.fcn)

  ## Calculate and return MSE
  return(???)
}


### Regression prediction function for NW kernel:
reg.fcn.nw <- function(reg.x, reg.y, x)
  ksmooth(???, ???, x.point = ???, kernel = "normal", bandwidth = ???)$y
  
### Calculation of LOO CV-score for NW kernel estimator:
(cv.nw <- loocv(reg, reg.fcn.nw))

### Hat matrix "S.nw":
n <- nrow(reg)
Id <- diag(n)
S.nw <- matrix(0, n, n)
for (j in 1:n)
  S.nw[, j] <- reg.fcn.nw(???, ???, ???)
  
### Degrees of freedom (cf. Formula (3.6) in the lecture notes:
(df.nw <- sum(diag(S.nw)))


###################################################
### TASKS b) to e)
###################################################

### Proceed similarly as in task a); you can reuse the utility function
### loocv from task a)



