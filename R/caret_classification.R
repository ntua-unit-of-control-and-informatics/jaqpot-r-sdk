###library(caret)
#library(RANN)
#library(skimr)

#paste(names(getModelInfo()), collapse=',  ')
#method = 'pcaNNet'

#orange <- read.csv(
#  'https://raw.githubusercontent.com/selva86/datasets/master/orange_juice_withmissing.csv')
#big = F
#if(big){
#trainData = rbind(orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange
#                  ,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,
#                  orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange
#                  ,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,orange,
#                  orange,orange,orange,orange,orange,orange,orange,orange)
#} else{
#  trainData = orange
#}
##sapply(trainData, class)
#x = trainData[, 2:18]
#y = trainData$Purchase#
#
#skimmed <- skimr::skim_to_wide(trainData)
#skimmed[, c(1:5, 9:11, 13, 15:16)]#
##
#
#preProcess_missingdata_model <-caret::preProcess(orange, method='knnImpute')
#trainData <- predict(preProcess_missingdata_model, newdata = trainData)
#anyNA(trainData)

#dummies_model <- caret::dummyVars(Purchase ~ ., data=trainData)
#trainData_mat <- predict(dummies_model, newdata = trainData)
#trainData <- data.frame(trainData_mat)#
#
#preProcess_range_model <- caret::preProcess(trainData, method='range')
#trainData <- predict(preProcess_range_model, newdata = trainData)
#trainData$Purchase <- y


#if(big){
#  train = trainData[1:87000,]
#  test = trainData[87001:87700,]
#} else {
#  train = trainData[1:1000,]
#  test = trainData[1001:1070,]
#}#

#
#model_mars = train(Purchase ~ ., data=train, method = method)
#test_model = model_mars


#test_model$modelInfo = NULL #maybe it has to be kept
#test_model$trainingData = NULL
#test_model$control = NULL
#test_model$finalModel$glm.list =NULL
#if (method !='dwdRadial' ){
  # Prediction of dwdRadial is not implemented without x
#test_model$finalModel$x =NULL
#}
#test_model$finalModel$bx =NULL
#test_model$finalModel$y =NULL #rpart
#test_model$finalModel$call =NULL #glmboost, rpart
#test_model$finalModel$residuals =NULL
#test_model$finalModel$fitted.values =NULL
#test_model$finalModel$data=NULL #naive_bayes
#test_model$finalModel$fit =NULL #naive_bayes
#test_model$finalModel$effects =NULL #bayesglm
#test_model$finalModel$linear.predictors =NULL #bayesglm
#test_model$finalModel$qr$qr =NULL #bayesglm
#test_model$finalModel$weights =NULL #bayesglm
#test_model$finalModel$model =NULL #bayesglm
#test_model$finalModel$prior.weights =NULL #bayesglm
#test_model$finalModel$baselearner =NULL #glmboost
#test_model$finalModel$basemodel =NULL #glmboost
#test_model$finalModel$rownames =NULL #glmboost
#test_model$finalModel$`(weights)` =NULL #glmboost
#test_model$finalModel$ustart =NULL #glmboost
#test_model$finalModel$where =NULL #rpart


#if (method =='fda'){
#  test_model$modelInfo = NULL #maybe it has to be kept
#  test_model$trainingData = NULL
#  test_model$control = NULL
#  test_model$finalModel$fit$bx =NULL
#  test_model$finalModel$fit$residuals = NULL
#  test_model$finalModel$fit$leverages = NULL
#} else if (method =='FRBCS.CHI' || method =='FH.GBML'|| method =='SLAVE'||method == 'FRBCS.W'){
#  test_model$modelInfo = NULL #maybe it has to be kept
#  test_model$trainingData = NULL
#  test_model$control = NULL
#  test_model$finalModel$rule.data.num =NULL
#}  else if (method=='gam' ||method=='bam'){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#  test_model$finalModel$call =NULL
#  test_model$finalModel$y =NULL
#  test_model$finalModel$qrx =NULL
#  test_model$finalModel$model =NULL
#  test_model$finalModel$residuals =NULL
#  test_model$finalModel$linear.predictors =NULL
#  test_model$finalModel$fitted.values =NULL
#  test_model$finalModel$dw.drho =NULL
#  test_model$finalModel$db.drho =NULL
#  test_model$finalModel$rV =NULL
#  test_model$finalModel$R =NULL
#  test_model$finalModel$Vp =NULL
#  test_model$finalModel$Ve =NULL
#} else if(method=="glm" ||method=="glmStepAIC"){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#  test_model$finalModel$R =NULL
#  test_model$finalModel$data =NULL
#  test_model$finalModel$model =NULL
#  test_model$finalModel$qr$qr =NULL
#  test_model$finalModel$y =NULL
#  test_model$finalModel$linear.predictors =NULL
#  test_model$finalModel$fitted.values =NULL
#  test_model$finalModel$residuals =NULL
#  test_model$finalModel$prior.weights = NULL
#  test_model$finalModel$weights =NULL
#  test_model$finalModel$effects =NULL
#} else if(method=="gpls"){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#} else if(method=="glmnet_h2o" || method == 'gbm_h2o'){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#} else if(method=="glmnet"){
#  #test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#  test_model$finalModel$call =NULL
#} else if(method=="svmLinearWeights2" || method== 'svmLinear3'){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#  test_model$finalModel$call =NULL
#} else if(method=="lssvmRadial"){
#    test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#  test_model$finalModel@fitted =NULL
#  test_model$finalModel@ymatrix =NULL
#} else if(method=="lda" || method=="lda2"){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#} else if(method=="stepLDA"){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#} else if(method=="dwdLinear"){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
# test_model$finalModel$x =NULL
#} else if(method=="svmLinearWeights"){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#  test_model$finalModel$decision.values =NULL
#  test_model$finalModel$fitted = NULL
#} else if(method=="LMT"){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#  test_model$finalModel$call =NULL
#} else if(method=="avNNet"){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#} else if(method=="mlpWeightDecay"|| method == 'mlpWeightDecayML'|| method == 'mlpML'){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#  test_model$finalModel$fitted.values = NULL
#}else if(method=="mlpKerasDropout"|| method == 'mlpKerasDropoutCost'|| method == 'mlpKerasDecay'
#         || method =='mlpKerasDecayCost'){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#}else if(method=="gcvEarth"){
  #  test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#  test_model$finalModel$call = NULL
#  test_model$finalModel$x = NULL
#  test_model$finalModel$bx =  NULL
#  test_model$finalModel$y = NULL
#  test_model$finalModel$residuals = NULL
#} else if(method=="nb"){
#  #test_model$modelInfo = NULL
#  test_model$control = NULL
#  test_model$results = NULL
#  test_model$times = NULL
#  test_model$trainingData = NULL
#  test_model$finalModel$call = NULL
#  test_model$finalModel$x = NULL
#} else if(method=="nnet"){
#  test_model$modelInfo = NULL
#  test_model$control = NULL
#  #test_model$results = NULL
#  #test_model$times = NULL
#  #test_model$trainingData = NULL
#  #test_model$finalModel$residuals = NULL
#  #test_model$finalModel$fitted.values = NULL
#  #} else if(method=="pcaNNet"){
#  #test_model$modelInfo = NULL
#  #test_model$control = NULL
#  #test_model$results = NULL
#  #test_model$times = NULL
#  #test_model$trainingData = NULL
#  #test_model$finalModel$model$residuals = NULL
  #  test_model$finalModel$model$fitted.values = NULL
  #}




  #fitted <- predict(test_model, newdata = test)
#100*(length(test$Purchase)-sum(test$Purchase== fitted))/length(test$Purchase)
#remove(fitted)
#object.size(test_model)

#sort(sapply(test_model, object.size))
#sort(sapply(test_model$finalModel, object.size))

#
