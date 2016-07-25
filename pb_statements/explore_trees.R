## Script to explore properties of decision trees in 2D
## Author : Sylvain Robert
## FS 2014, 16.05.2014
## FS 2016, 20.05.2016 updated by Sylvain

## Content:

## 0. load the data
## 1. training
## 2. pruning
## 3. variance of trees
## 4. bagging




###################################################################
## 0. load the data ###############################################

.pardefault <- par(no.readonly = T) ## to reset to default later on

## load the data:
banana <- read.table(file='banana.dat', sep=',')
## sample a training set
set.seed(11)
banana <- banana[sample.int(nrow(banana), 200),]

## format:
colnames(banana) <- c('X1', 'X2', 'Y')
banana[,'Y'] <- as.factor(banana[,'Y'])
y.colors <- c('pink', 'palegreen3')[banana$Y]
y.symbol <- c(2,3)[banana$Y]
## plot:
xlims <- c(-3,3); ylims <- c(-2.5,2.5)
plot(X2~X1, col=y.colors, data=banana, 
     pch=y.symbol,
     xlim=xlims, ylim=ylims)


###################################################################
## 1. training ####################################################

## overfit a tree:
## parameter cost-complexity=cp = 0.0 means that we do not penalize 
## the complexity of the tree at all.
## minsplit=10 means that we don't split leaves with less than 10
## points (try to set minsplit=1 and see how big the tree get).
require(rpart)
tree <- rpart(Y ~ ., data = banana,
              control = rpart.control(cp = 0.0, 
                                      minsplit = 10))

## nice plot:
require(rpart.plot)
prp(tree, extra=1, type=1, 
    box.col=c('pink', 'palegreen3')[tree$frame$yval])

## Interpretation:
## go down the tree
## X2 >= 0.59: left yes, right no
## node: -1 = majority class of the node
##      112 = number of points from class -1
##       88 = number of points from class +1


## function to plot 2D partioning:
## hackish solution similar to the use of contours for LDA/QDA:
## predict on a grid, plot with image()
tree.2d <- function(tree, data=banana, ...){
  x1 <- seq(xlims[1], xlims[2], length.out=200)
  x2 <- seq(ylims[1],ylims[2], length.out=200)
  newdat <- expand.grid(X1=x1, X2=x2)
  y.pred <- predict(tree, newdata=newdat, type='class')
  
  z <- matrix(as.numeric(y.pred), nrow=length(x1), ncol=length(x2))
  image(x1, x2, z, col=c('pink', 'palegreen3'),xlab=NA, ylab=NA,...)
  
  ## plot the data overlayed:
  y.colors <- c('pink', 'palegreen3')[banana$Y]
  y.symbol <- c(2,3)[banana$Y]
  y.symbol.full <- c(16,17)[banana$Y]
  
  points(X2~X1, data=data, col=y.colors, pch=y.symbol.full)
  points(X2~X1, data=data, pch=y.symbol)
}


## partioning for the overfitted tree:
tree.2d(tree,axes=F, main='non-pruned tree')








###################################################################
## 2. PRUNING:  ###################################################

## explore trees of decreasing complexity (==increasing cp)
complexities <- sort(unique(tree$frame$complexity))

for (i in seq_along(complexities)){
  complexity <- complexities[i]
  ## plot tree with branches that are pruned shaded
  cols <- ifelse(tree$frame$complexity > complexity, 1, "darkgray")
  par(mfrow=c(2,1), mar=c(.5,0,1,0))
  prp(tree, col=cols, branch.col=cols, split.col=cols,
      main=paste('CP =', round(complexity,3)),
      box.col=c('pink', 'palegreen3')[tree$frame$yval])
  p.tree <- prune.rpart(tree, cp=complexity)
  tree.2d(p.tree, axes=F)
  devAskNewPage(TRUE)
}
devAskNewPage(FALSE)



## Select optimal tree:
par(.pardefault)

### 1 std-error rule:
plotcp(tree)
abline(v=7, col='red')
## choose tree size = 8 ==> 7 splits 
## if not clear enough check more precisely with the table
printcp(tree)
## optimal Cp:
opt.cp <- tree$cptable[7, 'CP']
## check: 1. find min xerror
##        2. add xstd to min xerror => xlim
##        3. find smallest CP where xerror < xlim

## Prune the tree:
opt.tree <- prune.rpart(tree, cp=opt.cp)
## plot it:
prp(opt.tree, extra=1, type=1, 
    box.col=c('pink', 'palegreen3')[tree$frame$yval])

tree.2d(opt.tree, axes=F)







###################################################################
## 3. Large variance of trees #####################################

## load the full dataset
banana.full <- read.table(file='banana.dat', sep=',')
colnames(banana.full) <- c('X1', 'X2', 'Y')
banana.full[,'Y'] <- as.factor(banana.full[,'Y'])
y.full.colors <- c('pink', 'palegreen3')[banana.full$Y]
y.full.symbol <- c(2,3)[banana.full$Y]


## Repeat this as many times as you want:
## Observe how the trees are really different: High variance!
## if you increase n, it changes less (of course)
n <- 100 #800
resample <- 'yes'
par(.pardefault)
while(! resample %in% c('n','no')){  
  ## sample the data:
  banana <- banana.full[sample.int(nrow(banana.full), n),]
  
  ## fit a tree:
  tree <- rpart(Y ~ ., data = banana,
                control = rpart.control(cp = 0.0, minsplit = 10))
  
  ## automatic 1 std-error rule:
  cptable <- tree$cptable
  min.ind <- which.min(cptable[,"xerror"])
  min.lim <- cptable[min.ind, "xerror"] + cptable[min.ind, "xstd"]
  cp.opt <- cptable[(cptable[,"xerror"] < min.lim),"CP"][1]
  
  ## pruning
  opt.tree <- prune.rpart(tree, cp=cp.opt)
  
  ## plotting
  par(mfrow=c(2,1), mar=c(0.5,0,1,0))
  prp(opt.tree, extra=1, type=1, 
      main=paste('CP =', round(cp.opt,3)),
      box.col=c('pink', 'palegreen3')[tree$frame$yval])
  tree.2d(opt.tree, axes=F, xlim=c(-3,3), ylim=c(-3,3))
  
  
  print('Do you want to continue: Y/n')
  resample <- readline()
}





###################################################################
## 4. BAGGING of trees: ###########################################

## Trees have a lot of variance, as we just saw.
## A natural idea would be to take the average decision
## over many trees that we train on different bootstrapped sample.
## Pushing this idea a tiny bit further would lead to random Forest, but
## this will be for another time!

## function to train one tree on a subsample of data
## determined by ind.training
## return predicted probabilities for each classes on a test set:
one.tree <- function(data, ind.training, x.test=newdat){
  
  tree <- rpart(Y ~ ., data = data[ind.training, ],
                control = rpart.control(cp=0,minsplit = 1))
  ## choose optimal cp according to 1-std-error rule:
  min.ind <- which.min(tree$cptable[,"xerror"])
  min.lim <- tree$cptable[min.ind, "xerror"] + tree$cptable[min.ind, "xstd"]
  cp.opt <- tree$cptable[(tree$cptable[,"xerror"] < min.lim),"CP"][1]
  
  tree.sample <- prune.rpart(tree, cp=cp.opt)
  y <- predict(tree.sample, newdata=x.test, type='prob')
}



## grid:
x1 <- seq(-2.5,2.5, length.out=100)
x2 <- seq(-2.5,2.5, length.out=100)
newdat <- expand.grid(X1=x1, X2=x2)

## chose a particular dataset:
banana <- banana.full[sample.int(nrow(banana.full), 200),]
## format:
colnames(banana) <- c('X1', 'X2', 'Y')
banana[,'Y'] <- as.factor(banana[,'Y'])


## Train B trees on B different bootstrap samples:
## try different B
B <- 100 # 10, 100, 500, 1000
n <- 200
many.tree <- replicate(B, one.tree(banana, sample.int(nrow(banana),n, replace = TRUE)))

## average over the B repeatition:
mean.tree <- apply(many.tree, c(1,2), mean)

## plot majority class:
## Observe how the partioning becomes more and more 'curvy' and
## can follow better the 'banana' shape of the data!
par(.pardefault)
y.pred <- ifelse(mean.tree[,1] > 0.5, 1, 2)
z <- matrix(as.numeric(y.pred), nrow=length(x1), ncol=length(x2))
image(x1, x2, z, col=c('pink', 'palegreen3'), main=paste('Bag of trees of size = ', B))


## plot probability (intensity of color = probability of this class)
rbPal <- colorRampPalette(c('palegreen3','pink'))
y.prob <- rbPal(10)[as.numeric(cut(mean.tree[,1],breaks = 10))]
z.prob <- matrix(as.numeric(mean.tree[,1]), nrow=length(x1), ncol=length(x2))
image(x1, x2, z.prob, col=rbPal(20), main=paste('Bag of trees of size = ', B))

# overlay with plot of the original data:
y.colors <- c('pink', 'palegreen3')[banana$Y]
y.symbol <- c(2,3)[banana$Y]
y.symbol.full <- c(16,17)[banana$Y]
points(X2~X1, data=banana, col=y.colors, pch=y.symbol.full)
points(X2~X1, data=banana, pch=y.symbol)
## more errors in zone of lower probability...
## low probability == more uncertainty




