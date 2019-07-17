library(caret)
library(RANN)
library(skimr)

method = 'pcaNNet'
N=1000
y = rnorm(N,100,1)
x1 = 2*y+rnorm(N,2,2)
x2 = y/2+rnorm(N,0,10)
train = data.frame(y = y[1:(0.9*N)], x1 = x1[1:(0.9*N)], x2 = x2[1:(0.9*N)])
test =data.frame(y = y[(0.9*N+1):N], x1 = x1[(0.9*N+1):N], x2 = x2[(0.9*N+1):N])

model_mars2 = train(y ~ ., data=train, method=method)

test_model2= model_mars2


test_model2$modelInfo = NULL #maybe it has to be kept
test_model2$trainingData = NULL
test_model2$control = NULL 
test_model2$finalModel$glm.list =NULL
if (method !='dwdRadial' ){
  # Prediction of dwdRadial is not implemented without x 
 test_model2$finalModel$x =NULL
}
test_model2$finalModel$bx =NULL
test_model2$finalModel$y =NULL #rpart
test_model2$finalModel$call =NULL #glmboost, rpart
test_model2$finalModel$residuals =NULL
test_model2$finalModel$fitted.values =NULL
test_model2$finalModel$data=NULL #naive_bayes
test_model2$finalModel$fit =NULL #naive_bayes
test_model2$finalModel$effects =NULL #bayesglm
test_model2$finalModel$linear.predictors =NULL #bayesglm
test_model2$finalModel$qr$qr =NULL #bayesglm
test_model2$finalModel$weights =NULL #bayesglm
test_model2$finalModel$model =NULL #bayesglm
test_model2$finalModel$prior.weights =NULL #bayesglm
test_model2$finalModel$baselearner =NULL #glmboost
test_model2$finalModel$basemodel =NULL #glmboost
test_model2$finalModel$rownames =NULL #glmboost
test_model2$finalModel$`(weights)` =NULL #glmboost
test_model2$finalModel$ustart =NULL #glmboost
test_model2$finalModel$where =NULL #rpart 



if (method=='brnn'){
 test_model2$finalModel$y =NULL #brnn 
 test_model2$finalModel$x_normalized =NULL #brnn 
} else if (method=='bridge' || method=='blassoAveraged'){
 test_model2$finalModel$X =NULL #bridge 
} else if (method=='xgbDART' || method=='xgbLinear' || method=='xgbTree'){
 test_model2$modelInfo = NULL 
 test_model2$trainingData = NULL
 test_model2$control = NULL
 test_model2$results = NULL 
 test_model2$finalModel$call =NULL 
 test_model2$finalModel$raw =NULL 
 test_model2$finalModel$callbacks =NULL
} else if (method=='FIR.DM' || method=='GFS.FR.MOGUL'|| method=='GFS.THRIFT'||  method=='HYFIS'){
 test_model2$modelInfo = NULL 
 test_model2$control = NULL
 test_model2$results = NULL 
 test_model2$times = NULL 
 test_model2$trainingData = NULL 
  #test_model2$finalModel$ =NULL
} else if(method == 'bam'||method == 'gam'){
 test_model2$modelInfo = NULL 
 test_model2$control = NULL
 test_model2$results = NULL 
 test_model2$times = NULL 
 test_model2$trainingData = NULL 
 test_model2$finalModel$G =NULL
 test_model2$finalModel$call =NULL
 test_model2$finalModel$y =NULL
 test_model2$finalModel$qrx =NULL
 test_model2$finalModel$model =NULL
 test_model2$finalModel$residuals =NULL
 test_model2$finalModel$linear.predictors =NULL
 test_model2$finalModel$fitted.values =NULL
} else if(method=="glm" || method == 'glmStepAIC'){
  test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL 
  test_model2$finalModel$R =NULL
  test_model2$finalModel$data =NULL
  test_model2$finalModel$model =NULL
  test_model2$finalModel$qr$qr =NULL
  test_model2$finalModel$y =NULL
  test_model2$finalModel$linear.predictors =NULL
  test_model2$finalModel$fitted.values =NULL
  test_model2$finalModel$residuals =NULL
  test_model2$finalModel$prior.weights = NULL
  test_model2$finalModel$weights =NULL
  test_model2$finalModel$effects =NULL
} else if(method=="glmnet_h2o"|| method=="gbm_h2o" ){
  test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL
} else if(method=="glmnet"){
  #test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL
  test_model2$finalModel$call =NULL
} else if(method=="knn"){
  test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL
} else if(method=="svmLinear3"){
  test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL
} else if(method=="lmStepAIC"){
  test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL
  test_model2$finalModel$effects =NULL
  test_model2$finalModel$residuals =NULL
  test_model2$finalModel$qr$qr =NULL
  test_model2$finalModel$model =NULL
  test_model2$finalModel$fitted.values =NULL
} else if(method=="avNNet"){
  test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL
} else if(method=="M5Rules"||method=='M5'){
  test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL
  test_model2$finalModel$call = NULL
  test_model2$finalModel$predictions =NULL
} else if(method=="mlpWeightDecay"|| method == 'mlpWeightDecayML'|| method == 'mlpML'){
  test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL
  test_model2$finalModel$fitted.values = NULL
} else if(method=="mlpKerasDropout"|| method=='mlpKerasDecay'){
  test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL
} else if(method=="glm.nb"){
  test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL
  test_model2$finalModel$qr$qr= NULL
  test_model2$finalModel$residuals= NULL
  test_model2$finalModel$fitted.values= NULL
  test_model2$finalModel$R= NULL
  test_model2$finalModel$model= NULL
  test_model2$finalModel$linear.predictors= NULL
  test_model2$finalModel$y= NULL
  test_model2$finalModel$weights= NULL
  test_model2$finalModel$prior.weights= NULL
} else if(method=="nnet"){
  test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL
  test_model2$finalModel$residuals= NULL
  test_model2$finalModel$fitted.values= NULL
} else if(method=="pcaNNet"){
  test_model2$modelInfo = NULL 
  test_model2$control = NULL
  test_model2$results = NULL 
  test_model2$times = NULL 
  test_model2$trainingData = NULL
  test_model2$finalModel$model$residuals= NULL
  test_model2$finalModel$model$fitted.values= NULL
} 



fitted <- predict(test_model2, newdata = test)
sum(test$y - fitted)
remove(fitted)
object.size(test_model2)

sort(sapply(test_model2, object.size))
sort(sapply(test_model2$finalModel, object.size))

