#require(tree)
#library(ISLR)
#data(package="ISLR")
#carseats<-Carseats
#High = ifelse(carseats$Sales<=8, "No", "Yes")
#carseats = data.frame(carseats, High)
#tree.carseats = tree(High~.-Sales, data=carseats)
#train=sample(1:nrow(carseats), 250)
#tree.pred = predict(tree.carseats, carseats[-train,])
#tree.carseats$where <- NULL
#tree.carseats$call <- NULL
#tree.carseats$y <- NULL
#tree.carseats$weights <- NULL


countries <- read.csv('/Users/pantelispanka/Desktop/every-day/datasets/gdp-countries.csv')
tit <- read.csv('/Users/pantelispanka/Desktop/every-day/datasets/train.csv')

library(MASS)
data(package="MASS")
boston<-Boston
require(randomForest)
train = sample(1:nrow(boston), 300)
rf.boston = randomForest(medv~., data = boston, subset = train)
predict(rf.boston, data = boston)

rf.boston$y <- NULL
rf.boston$mse <- NULL
rf.boston$rsq <- NULL
rf.boston$oob.times <- NULL
rf.boston$importance <- NULL
rf.boston$proximity <- NULL
rf.boston$coefs <- NULL
rf.boston$inbag <- NULL
rf.boston$localImportance <- NULL
rf.boston$importanceSD <- NULL


gmb.mod <- gbm(GDP~LFG+NEQ+GAP, data=countries)

gmb.mod$train.error <- NULL
gmb.mod$valid.error <- NULL
gmb.mod$fit <- NULL


