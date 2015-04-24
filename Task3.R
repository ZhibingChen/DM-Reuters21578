setwd("/Users/apple/Documents/CS909/Exercise10/treetag")
reuters.combined.features <- read.csv("~/Documents/CS909/Exercise10/treetag/task2.reuters.combined.features.csv", header=T, sep=",")
c(nrow(reuters.combined.features ),  ncol(reuters.combined.features))
# [1] 9176  123
reuters.lda.features <- read.csv("~/Documents/CS909/Exercise10/treetag/task2.reuters.lda.features.csv", header=T, sep=",")
c(nrow(reuters.lda.features ),  ncol(reuters.lda.features))
#[1] 9176  56

reuters.combined.train <-  reuters.combined.features[which(reuters.combined.features$purpose == "train"), -c(1:11)]
c(nrow(reuters.combined.train), ncol(reuters.combined.train))
#[1] 6623  112                                
reuters.combined.test <- reuters.combined.features[which(reuters.combined.features$purpose == "test"), -c(1:11)]
c(nrow(reuters.combined.test), ncol(reuters.combined.test))
#[1] 2553  112
reuters.lda.train <-  reuters.lda.features[which(reuters.lda.features$purpose == "train"), -c(1:11)]
c(nrow(reuters.lda.train), ncol(reuters.lda.train))
#[1] 6623   45
reuters.lda.test <- reuters.lda.features[which(reuters.lda.features$purpose == "test"), -c(1:11)]
c(nrow(reuters.lda.test), ncol(reuters.lda.test))
# [1] 2553   45

reuters.combined.train$class <- as.factor(reuters.combined.train$class)
reuters.combined.test$class <- as.factor(reuters.combined.test$class)
reuters.lda.train$class <- as.factor(reuters.lda.train$class)
reuters.lda.test$class <- as.factor(reuters.lda.test$class)

# 10-fold cross validation
## RandomForest
reuters.combined.10fold <- rbind(reuters.combined.train,reuters.combined.test)
c(nrow(reuters.combined.10fold), ncol(reuters.combined.10fold))
#[1] 9176  112
reuters.lda.10fold <- rbind(reuters.lda.train,reuters.lda.test)
c(nrow(reuters.lda.10fold), ncol(reuters.lda.10fold))
#[1] 9176   45

#linear [1] 0.7447447 scale=F [1] 0.7607608
#sigmoid [1] 0.7067067        [1] 0.5555556
#polynomial [1] 0.7497497     [1] 0.5955956
#radial basis [1] 0.7687688   [1] 0.7627628
#default [1] 0.7687688 
#ten fold cross validation
overalldf <- function(df){
  tr <- c(sum(df[,8])/100, sum(df[,9])/100, sum(df[,10])/100, sum(df[,11])/100)
  return(rt)
}

combined.svm.linear.result <- crossValidation(reuters.combined.10fold,"svm.linear")#
# overalldf(combined.svm.linear.result)

combined.svm.sigmoid.result <- crossValidation(reuters.combined.10fold,"svm.sigmoid")#

combined.svm.polynomial.result <- crossValidation(reuters.combined.10fold,"svm.polynomial")

combined.svm.radial.result <- crossValidation(reuters.combined.10fold,"svm.radial")#

lda.svm.linear.result <- crossValidation(reuters.lda.10fold,"svm.linear")#

lda.svm.sigmoid.result <- crossValidation(reuters.lda.10fold,"svm.sigmoid")#

lda.polynomial.result <- crossValidation(reuters.lda.10fold,"svm.polynomial")


lda.svm.radial.result <- crossValidation(reuters.lda.10fold,"svm.radial")#


combined.nb.result <- crossValidation(reuters.combined.10fold,"naivebayes")#

lda.nb.result <- crossValidation(reuters.lda.10fold,"naivebayes")#

combined.rf.result <- crossValidation(reuters.combined.10fold,"randomforest")

lda.rf.result <- crossValidation(reuters.lda.10fold,"randomforest")
# microRecall macroRecall microPrecision macroPrecision
df<-lda.rf.result 
c(sum(df[,8])/100, sum(df[,9])/100, sum(df[,10])/100, sum(df[,11])/100)

#train model and prediction
library("randomForest")
combined.rf.model <- randomForest(class ~., data = reuters.combined.train, nodesize = 2)
predict <- predict(combined.rf.model, reuters.combined.test[,-ncol(reuters.combined.test)])
table <- table(observed = reuters.combined.test$class, predicted = predict)
combined.rf.model.result <- measure(table)
0.8159029
measure(combined.rf.model$confusion[,-11])
0.7424128 

lda.rf.model <- randomForest(class ~., data = reuters.lda.train, nodesize = 5)
predict <- predict(lda.rf.model, reuters.lda.test[,-ncol(reuters.lda.test)])
table <- table(observed = reuters.lda.test$class, predicted = predict)
lda.rf.model.result <- measure(table)
0.6948688 
measure(lda.rf.model$confusion[,-11])
0.7079873 

library("e1071")
combined.nb.model <- naiveBayes(class~., data = reuters.combined.train)
predict <- predict(combined.nb.model , reuters.combined.test[,-ncol(reuters.combined.test)])
table <- table(observed = reuters.combined.test$class, predicted = predict )
combined.nb.model.result <- measure(table)
# 0.6948688
# measure(lda.rf.model$confusion[,-11])

lda.nb.model <- naiveBayes(class~., data = reuters.lda.train)
predict <- predict(lda.nb.model , reuters.lda.test[,-ncol(reuters.lda.test)])
table <- table(observed = reuters.lda.test$class, predicted = predict )
lda.nb.model.result <- measure(table)
# 0.6200548 

library("e1071")
combined.svm.linear.model <- svm(class ~., data = reuters.combined.train, kernel = "linear")
predict <- predict(combined.svm.linear.model, reuters.combined.test[,-ncol(reuters.combined.test)])
table <- table(observed = reuters.combined.test$class, predicted = predict )
combined.svm.linear.model.result <- measure(table)
# 0.7978848 

lda.svm.linear.model <- svm(class ~., data = reuters.lda.train, kernel = "linear")
predict <- predict(lda.svm.linear.model, reuters.lda.test[,-ncol(reuters.lda.test)])
table <- table(observed = reuters.lda.test$class, predicted = predict )
lda.svm.linear.model.result <- measure(table)
# 0.7215041 

library("e1071")
combined.svm.poly.model <- svm(class ~., data = reuters.combined.train, kernel = "polynomial")
predict <- predict(combined.svm.poly.model, reuters.combined.test[,-ncol(reuters.combined.test)])
table <- table(observed = reuters.combined.test$class, predicted = predict )
combined.svm.poly.model.result <- measure(table)
# 0.7896592

lda.svm.poly.model <- svm(class ~., data = reuters.lda.train, kernel = "polynomial")
predict <- predict(lda.svm.poly.model, reuters.lda.test[,-ncol(reuters.lda.test)])
table <- table(observed = reuters.lda.test$class, predicted = predict )
lda.svm.poly.model.result <- measure(table)
# 0.7215041 

library("e1071")
combined.svm.sigmoid.model <- svm(class ~., data = reuters.combined.train, kernel = "sigmoid")
predict <- predict(combined.svm.sigmoid.model, reuters.combined.test[,-ncol(reuters.combined.test)])
table <- table(observed = reuters.combined.test$class, predicted = predict )
combined.svm.sigmoid.model.result <- measure(table)
# 0.7818253

lda.svm.sigmoid.model <- svm(class ~., data = reuters.lda.train, kernel = "sigmoid")
predict <- predict(lda.svm.sigmoid.model, reuters.lda.test[,-ncol(reuters.lda.test)])
table <- table(observed = reuters.lda.test$class, predicted = predict )
lda.svm.sigmoid.model.result <- measure(table)
# 0.7215041

library("e1071")
combined.svm.radial.model <- svm(class ~., data = reuters.combined.train, kernel = "radial")
predict <- predict(combined.svm.radial.model, reuters.combined.test[,-ncol(reuters.combined.test)])
table <- table(observed = reuters.combined.test$class, predicted = predict )
combined.svm.radial.model.result <- measure(table)
# 0.8076772 

lda.svm.radial.model <- svm(class ~., data = reuters.lda.train, kernel = "radial")
predict <- predict(lda.svm.radial.model, reuters.lda.test[,-ncol(reuters.lda.test)])
table <- table(observed = reuters.lda.test$class, predicted = predict )
lda.svm.radial.model.result <- measure(table)
# 0.7215041  

