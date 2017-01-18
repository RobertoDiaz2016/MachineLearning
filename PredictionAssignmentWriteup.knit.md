---
title: "Prediction Assignment Writeup"
author: "Roberto Diaz"
date: "January 17, 2017"
output: 
  html_document:
    pandoc_args: [
    "-M2GB","+RTS","-K64m","-RTS"
    ]
    md_document:
      keep_md: true
---

<style type="text/css">

body, td {
   font-size: 14px;
}
code.r{
  font-size: 12px;
}
pre {
  font-size: 12px
}
</style>




##Goal
Predict classe response variables. classe variable is a factor, grouping of excersizes, and the predictor exercise variables(metrics) are sensor readings, belt, forearm, arm and dumbbell.

training data has five classes A,B,C,D,E, with A being the correct execution and the other four with varying incorrect executions. Classification Models will be used on the response variable, classe, and predictors for each of the four sensor metric groups. Adaptive Cross validation resampling in caret train executions and comparison of model accuracy, rpart, bag, gbm and rf, will be used to get optimal predictions running in parallel.

adaptive_cv trainControl parameters were set for all trained models. Folds=5, min=5, repeats=3, for a reasonable average, alpha =.05 for a best model, and complete=true, recompute single cases.

1-Accuracy will be used for the expected out of sample error.

Classification reasoning: classe was in the form of a factor and as opposed to a numeric value for regression.
Adaptive Cross Validation reasoning: adaptive_cv resampling drops poor models and saves time over running repeatedcv resampling.

testingCases$classe variable was added with a random sample.

testingCases returned a very low accuracy, possibly due to only twenty samples provided.

## Data

Trainng data taken from: <https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>.

Testing cases data taken from: <https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>.



```r
training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",na.strings = c("NA","#DIV/0!"),stringsAsFactors = TRUE)
```

Variables, user_name, cvtd_timestamp, new_window and classe are factor variables with labels.

```r
str(training$user_name)
str(training$cvtd_timestamp)
str(training$new_window)
str(training$classe)
```


Rows had `NA`, blank and `#DIV/0!` values, which were replaced with NA values. 

```r
no.na.sums <- which(apply(training,2,function(x) sum(is.na(x)) == 0 ))
```


Locate sensor variables for subsetting.

```r
belt <- names(no.na.sums[grep("_belt",names(no.na.sums))])

forearm <- names(no.na.sums[grep("_forearm",names(no.na.sums))])

arm <- names(no.na.sums[grep("_arm",names(no.na.sums))])

dumbbell <- names(no.na.sums[grep("_dumbbell",names(no.na.sums))])
```



```r
library(doParallel)
library(caret)
trainingsub <- subset(training[no.na.sums],select=c("classe",belt,forearm,arm,dumbbell))

inTrain <- createDataPartition(y=trainingsub$classe,
p=0.75, list=FALSE)
training <- trainingsub[inTrain,]
testing <- trainingsub[-inTrain,]
```



## ModelLibs

```r
library(caret)
library(gbm)
library(ipred)
library(compiler)

ctrl <- trainControl(method = "adaptive_cv",number = 5, repeats = 3,adaptive = list(min = 5, alpha = 0.05, method = "gls", complete = TRUE),classProbs = TRUE,search = "grid",savePredictions = TRUE)
```



## RPART Model

```r
set.seed(2017)

# Set up to do parallel processing
# Registrer a parallel backend
cl8 <- makeCluster(8)
registerDoParallel(cl8)
# Number of worker processes
getDoParWorkers()

fitRPART <- train(classe~.,data =training,method="rpart",trControl=ctrl,tuneLength=5)
# fitRPART

# stop cluster worker processes
stopCluster(cl8)
```



## Boosting Model

```r
set.seed(2017)

# Set up to do parallel processing
# Registrer a parallel backend
cl8 <- makeCluster(8)
registerDoParallel(cl8)
# Number of worker processes
getDoParWorkers()

fitGBM <- train(classe~.,data = training,method="gbm",trControl=ctrl,verbose=FALSE,tuneLength=5)

# stop cluster worker processes
stopCluster(cl8)
# fitGBM
```



## Bagging Model

```r
set.seed(2017)

# Set up to do parallel processing
# Registrer a parallel backend
cl8 <- makeCluster(8)
registerDoParallel(cl8)
# Number of worker processes
getDoParWorkers()

bagGrid <- expand.grid(parameter=(1:5)*nrow(training))


fitBAG <- train(classe~.,data = training,method="treebag",trControl=ctrl,tuneGrid=bagGrid)

# stop cluster worker processes
stopCluster(cl8)
# fitBAG
```

## Random Forest Model

```r
set.seed(2017)

# Set up to do parallel processing
# Registrer a parallel backend
cl8 <- makeCluster(8)
registerDoParallel(cl8)
# Number of worker processes
getDoParWorkers()

library(randomForest)
fitRF <- train(classe~.,data = training,method="rf",trControl=ctrl,tuneLength=5)

# stop cluster worker processes
stopCluster(cl8)
# fitRF
```

## Models Adaptive Cross Validation Run Times

```r
RPARTtimes <- fitRPART$times$everything[1:3]
names(RPARTtimes) <- c("user","sys","elapsed")
RPARTtimes

GBMtimes <- fitGBM$times$everything[1:3]
names(GBMtimes) <- c("user","sys","elapsed")
GBMtimes

BAGtimes <- fitBAG$times$everything[1:3]
names(BAGtimes) <- c("user","sys","elapsed")
BAGtimes

RFtimes <- fitRF$times$everything[1:3]
names(RFtimes) <- c("user","sys","elapsed")
RFtimes
```

## Models Adaptive Cross Validation Run Time Summary

```r
t(data.frame(RPARTtimes=RPARTtimes,GBMtimes=GBMtimes,BAGtimes=BAGtimes,RFtimes=RFtimes))
```

```
##              user  sys elapsed
## RPARTtimes   9.90 0.94   45.59
## GBMtimes   125.87 4.10 1260.46
## BAGtimes    40.40 4.78  674.02
## RFtimes     79.23 3.61 1382.80
```


## Models Adaptive Cross Validation Max Accuracy

```r
data.frame(RPARTAccr=max(fitRPART$results$Accuracy),GBMAccr=max(fitGBM$results$Accuracy),BAGAccr=max(fitBAG$results$Accuracy),RFAccr=max(fitRF$results$Accuracy))
```

```
##   RPARTAccr   GBMAccr   BAGAccr    RFAccr
## 1 0.5220154 0.9858675 0.9854593 0.9937263
```

## Models Adaptive Cross Validation ROC Plots

```r
par(mfrow=c(2,2))

library(pROC)
# need ,savePredictions = TRUE in cntrl
rocA <- roc(response = fitRPART$pred$obs, predictor = fitRPART$pred$A, positive = 'A',algorithm=2)
rocB <- roc(response = fitRPART$pred$obs, predictor = fitRPART$pred$B, positive = 'B',algorithm=2)
rocC <- roc(response = fitRPART$pred$obs, predictor = fitRPART$pred$C, positive = 'C',algorithm=2)
rocD <- roc(response = fitRPART$pred$obs, predictor = fitRPART$pred$D, positive = 'D',algorithm=2)
rocE <- roc(response = fitRPART$pred$obs, predictor = fitRPART$pred$E, positive = 'E',algorithm=2)

plot(rocA, col = "black",main = "ROC curves - rpart model- Classes ABCDEF",print.auc=TRUE,print.auc.y=.5)
plot(rocB, add = TRUE, col = "red",print.auc=TRUE,print.auc.y=.4)
plot(rocC, add = TRUE, col = "green",print.auc=TRUE,print.auc.y=.3)
plot(rocD, add = TRUE, col = "blue",print.auc=TRUE,print.auc.y=.2)
plot(rocE, add = TRUE, col = "purple",print.auc=TRUE,print.auc.y=.1)

legend("right", legend = c("A", "B", "C","D","E"), bty = "n", cex = 1, lty = 1, col = c("black", "red", "green","blue","purple"))


library(pROC)
# need ,savePredictions = TRUE in cntrl
rocA <- roc(response = fitGBM$pred$obs, predictor = fitGBM$pred$A, positive = 'A',algorithm=2)
rocB <- roc(response = fitGBM$pred$obs, predictor = fitGBM$pred$B, positive = 'B',algorithm=2)
rocC <- roc(response = fitGBM$pred$obs, predictor = fitGBM$pred$C, positive = 'C',algorithm=2)
rocD <- roc(response = fitGBM$pred$obs, predictor = fitGBM$pred$D, positive = 'D',algorithm=2)
rocE <- roc(response = fitGBM$pred$obs, predictor = fitGBM$pred$E, positive = 'E',algorithm=2)

plot(rocA, col = "black",main = "ROC curves - gbm model- Classes ABCDEF",print.auc=TRUE,print.auc.y=.5)
plot(rocB, add = TRUE, col = "red",print.auc=TRUE,print.auc.y=.4)
plot(rocC, add = TRUE, col = "green",print.auc=TRUE,print.auc.y=.3)
plot(rocD, add = TRUE, col = "blue",print.auc=TRUE,print.auc.y=.2)
plot(rocE, add = TRUE, col = "purple",print.auc=TRUE,print.auc.y=.1)

legend("right", legend = c("A", "B", "C","D","E"), bty = "n", cex = 1, lty = 1, col = c("black", "red", "green","blue","purple"))


library(pROC)
# need ,savePredictions = TRUE in cntrl
rocA <- roc(response = fitBAG$pred$obs, predictor = fitBAG$pred$A, positive = 'A',algorithm=2)
rocB <- roc(response = fitBAG$pred$obs, predictor = fitBAG$pred$B, positive = 'B',algorithm=2)
rocC <- roc(response = fitBAG$pred$obs, predictor = fitBAG$pred$C, positive = 'C',algorithm=2)
rocD <- roc(response = fitBAG$pred$obs, predictor = fitBAG$pred$D, positive = 'D',algorithm=2)
rocE <- roc(response = fitBAG$pred$obs, predictor = fitBAG$pred$E, positive = 'E',algorithm=2)

plot(rocA, col = "black",main = "ROC curves - bag model- Classes ABCDEF",print.auc=TRUE,print.auc.y=.5)
plot(rocB, add = TRUE, col = "red",print.auc=TRUE,print.auc.y=.4)
plot(rocC, add = TRUE, col = "green",print.auc=TRUE,print.auc.y=.3)
plot(rocD, add = TRUE, col = "blue",print.auc=TRUE,print.auc.y=.2)
plot(rocE, add = TRUE, col = "purple",print.auc=TRUE,print.auc.y=.1)

legend("right", legend = c("A", "B", "C","D","E"), bty = "n", cex = 1, lty = 1, col = c("black", "red", "green","blue","purple"))


library(pROC)
# need ,savePredictions = TRUE in cntrl
rocA <- roc(response = fitRF$pred$obs, predictor = fitRF$pred$A, positive = 'A',algorithm=2)
rocB <- roc(response = fitRF$pred$obs, predictor = fitRF$pred$B, positive = 'B',algorithm=2)
rocC <- roc(response = fitRF$pred$obs, predictor = fitRF$pred$C, positive = 'C',algorithm=2)
rocD <- roc(response = fitRF$pred$obs, predictor = fitRF$pred$D, positive = 'D',algorithm=2)
rocE <- roc(response = fitRF$pred$obs, predictor = fitRF$pred$E, positive = 'E',algorithm=2)

plot(rocA, col = "black",main = "ROC curves - rf model- Classes ABCDEF",print.auc=TRUE,print.auc.y=.5)
plot(rocB, add = TRUE, col = "red",print.auc=TRUE,print.auc.y=.4)
plot(rocC, add = TRUE, col = "green",print.auc=TRUE,print.auc.y=.3)
plot(rocD, add = TRUE, col = "blue",print.auc=TRUE,print.auc.y=.2)
plot(rocE, add = TRUE, col = "purple",print.auc=TRUE,print.auc.y=.1)

legend("right", legend = c("A", "B", "C","D","E"), bty = "n", cex = 1, lty = 1, col = c("black", "red", "green","blue","purple"))
```

<img src="PredictionAssignmentWriteup_files/figure-html/PLOTS-1.png" width="768" style="display: block; margin: auto;" />



## Predict Models

```r
predRPART <- predict(fitRPART,newdata = testing)
predRPART
class(predRPART)


predGBM <- predict(fitGBM,newdata = testing)
predGBM
class(predGBM)


predBAG <- predict(fitBAG,newdata = testing)
predBAG
class(predBAG)


predRF <- predict(fitRF,newdata = testing)
predRF
class(predRF)
```


## Confusion Matrices

```r
library(caret)

confusPredRPART <- confusionMatrix(predRPART,testing$classe)
confusPredRPART

confusPredGBM <- confusionMatrix(predGBM,testing$classe)
confusPredGBM

confusPredBAG <- confusionMatrix(predBAG,testing$classe)
confusPredBAG

confusPredRF <- confusionMatrix(predRF,testing$classe)
confusPredRF
```


## Expected Out of Sample Error , 1-Accuracy, EOOSE

```r
# expected out of sample error , 1-Accuracy, EOOSE
confusPredRPARTErrs <- c(round(confusPredRPART$overall[1],4),1-round(confusPredRPART$overall[1],4),confusPredRPART$byClass[,"Balanced Accuracy"])
names(confusPredRPARTErrs) <-c("Accuracy","EOOSE","BalAccr A","BalAccr B","BalAccr C","BalAccr D","BalAccr E")
confusPredRPARTErrs


confusPredGBMErrs <- c(round(confusPredGBM$overall[1],4),1-round(confusPredGBM$overall[1],4),confusPredGBM$byClass[,"Balanced Accuracy"])
names(confusPredGBMErrs) <-c("Accuracy","EOOSE","BalAccr A","BalAccr B","BalAccr C","BalAccr D","BalAccr E")
confusPredGBMErrs


confusPredBAGErrs <- c(round(confusPredBAG$overall[1],4),1-round(confusPredBAG$overall[1],4),confusPredBAG$byClass[,"Balanced Accuracy"])
names(confusPredBAGErrs) <-c("Accuracy","EOOSE","BalAccr A","BalAccr B","BalAccr C","BalAccr D","BalAccr E")
confusPredBAGErrs


confusPredRFErrs <- c(round(confusPredRF$overall[1],4),1-round(confusPredRF$overall[1],4),confusPredRF$byClass[,"Balanced Accuracy"])
names(confusPredRFErrs) <-c("Accuracy","EOOSE","BalAccr A","BalAccr B","BalAccr C","BalAccr D","BalAccr E")
confusPredRFErrs
```


## Expected Out of Sample Error Summary

```r
t(data.frame(confusPredRPARTErrs=confusPredRPARTErrs,confusPredGBMErrs=confusPredGBMErrs,confusPredBAGErrs=confusPredBAGErrs,confusPredRFErrs=confusPredRFErrs))
```

```
##                     Accuracy  EOOSE BalAccr A BalAccr B BalAccr C
## confusPredRPARTErrs   0.5493 0.4507 0.7790874 0.6409948 0.6788314
## confusPredGBMErrs     0.9900 0.0100 0.9973529 0.9913384 0.9926958
## confusPredBAGErrs     0.9847 0.0153 0.9954271 0.9887252 0.9900770
## confusPredRFErrs      0.9941 0.0059 0.9984282 0.9960591 0.9961463
##                     BalAccr D BalAccr E
## confusPredRPARTErrs 0.6404205 0.7323945
## confusPredGBMErrs   0.9893302 0.9961154
## confusPredBAGErrs   0.9795140 0.9944506
## confusPredRFErrs    0.9908057 0.9983352
```


## Model Max Accuracy and AUC for the Random Forest model show the the best accuracy and model fit. Random Forest model will be used to predict testing cases.



```r
testingCases <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",na.strings = c("NA","#DIV/0!"),stringsAsFactors = TRUE)
```


```r
# Add response column for testing predictor values with sample.
set.seed(2018)
# sample values
testingCases$classe <- factor(x=sample("1":"5",20,replace=TRUE),labels=c("A","B","C","D","E"))
```



Rows had `NA`, blank and `#DIV/0!` values, which were replaced with NA values. 

```r
no.na.sums <- which(apply(testingCases,2,function(x) sum(is.na(x)) == 0 ))
```


Locate sensor variables for subsetting.

```r
belt <- names(no.na.sums[grep("_belt",names(no.na.sums))])

forearm <- names(no.na.sums[grep("_forearm",names(no.na.sums))])

arm <- names(no.na.sums[grep("_arm",names(no.na.sums))])

dumbbell <- names(no.na.sums[grep("_dumbbell",names(no.na.sums))])
```




```r
testingCasessub <- subset(testingCases[no.na.sums],select=c("classe",belt,forearm,arm,dumbbell))
```



## Predict Testing Cases RF Model

```r
predRF <- predict(fitRF,newdata = testingCasessub)
predRF
class(predRF)
```


## Confusion Matrices

```r
library(caret)

confusPredRF <- confusionMatrix(predRF,testingCasessub$classe)
confusPredRF
```


## Expected Out of Sample Error , 1-Accuracy, EOOSE

```r
# expected out of sample error , 1-Accuracy, EOOSE

confusPredRFErrs <- c(round(confusPredRF$overall[1],4),1-round(confusPredRF$overall[1],4),confusPredRF$byClass[,"Balanced Accuracy"])
names(confusPredRFErrs) <-c("Accuracy","EOOSE","BalAccr A","BalAccr B","BalAccr C","BalAccr D","BalAccr E")
confusPredRFErrs
```


## Expected Out of Sample Error Summary

```r
t(data.frame(confusPredRFErrs=confusPredRFErrs))
```

```
##                  Accuracy EOOSE BalAccr A BalAccr B BalAccr C BalAccr D
## confusPredRFErrs     0.25  0.75 0.4901961    0.5625   0.46875       0.6
##                  BalAccr E
## confusPredRFErrs    0.5625
```

## Citation
References to data and specifics taken from:

Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.

[Documento](http://groupware.les.inf.puc-rio.br/public/papers/2013.Velloso.QAR-WLE.pdf)

Collaborators:

- Wallace Ugulino
- Eduardo Velloso
- Hugo Fuks




