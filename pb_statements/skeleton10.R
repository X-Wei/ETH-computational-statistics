### Code skeleton for Series 10

## a)

## load the data:
data <- read.table("http://stat.ethz.ch/Teaching/Datasets/NDK/vehicle.dat",
                   header = TRUE)


## train a full grown tree:
require(rpart)
set.seed(10)
tree <- rpart(???, data = ???,
              control = rpart.control(cp = 0.0, minsplit = 30))


## nice package for plotting trees:
require(rpart.plot)
prp(tree, extra=1, type=1, 
    box.col=c('pink', 'palegreen3', 
              'lightsteelblue 2','lightgoldenrod 1')[tree$frame$yval])



## b)
plotcp(tree)
printcp(tree)


## c)
#Compute the misclassification rate.


## d)
misclass.sample <- function(data, ind.training, ind.test)
{
  tree <- rpart(???)
  
  ## choose optimal cp according to 1-std-error rule:
  cp <- tree$cptable
  min.ind <- which.min(cp[,"xerror"])
  min.lim <- cp[min.ind, "xerror"] + cp[min.ind, "xstd"]
  cp.opt <- cp[(cp[,"xerror"] < min.lim),"CP"][1]

  ## prune the tree:
  tree.sample <- prune.rpart(tree, cp=cp.opt)
  
  ## return missclassifcation error:
  mean(data$Class[ind.test] != ???)
}


## Bootstrap error:

B <- 1000
n <- nrow(data)
boot.err <- function(data, ind) {
  misclass.sample(data, ind, 1:nrow(data))
}

boot.samples <- replicate(???, boot.err(data, sample(???)))
errboot <- ???(boot.samples)

## CV-error:

cv.err <- function(data, ind){
  ???
}

cv.samples <- sapply(???, ???, data = data)
errcv <- mean(???)



## e)
oobs.sample <- function(data, ind){
  ???
}

obs.samples <- ???
erroobs <- ???



