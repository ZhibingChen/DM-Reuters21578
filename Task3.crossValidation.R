crossValidation<-function(dataframe, classifier){
  width=nrow(dataframe)/10
  width  
  result <- c()
  # 10 fold iteration
  for(i in 1:10){
    #     i=6
    #split the dataset into 10 folds
    k=(width*(i-1)+1):(width*i)
    length(k)
# split into train set and test set
    test <- dataframe[k,]
    c(nrow(text), ncol(test))
    train <- dataframe[-k,]
    c(nrow(train), ncol(train))
    
    switch(tolower(classifier),
           randomforest = {
             library("randomForest")
             model <- randomForest(class ~., data = train, nodesize = 5)
             predict <- predict(model, test[,-ncol(test)])
             table <- table(observed = test$class, predicted = predict)
           },
           naivebayes = {
             library("e1071")
             model <- naiveBayes(class~., data = train)
             predict <- predict(model , test[,-ncol(test)])
             table <- table(observed = test$class, predicted = predict )
           },
           svm.linear = {
             library("e1071")
             model <- svm(class ~., data = train, kernel = "linear")
             predict <- predict(model, test[,-ncol(test)])
             table <- table(observed = test$class, predicted = predict )
           },
           svm.polynomial = {
             library("e1071")
             model <- svm(class ~., data = train, kernel = "polynomial")
             predict <- predict(model, test[,-ncol(test)])
             table <- table(observed = test$class, predicted = predict )
           },
           svm.sigmoid = {
             library("e1071")
             model <- svm(class ~., data = train, kernel = "sigmoid")
             predict <- predict(model, test[,-ncol(test)])
             table <- table(observed = test$class, predicted = predict )
           },
           svm.radial = {
             library("e1071")
             model <- svm(class ~., data = train, kernel = "radial")
             predict <- predict(model, test[,-ncol(test)])
             table <- table(observed = test$class, predicted = predict )
           }
    )
    m <- measure(table)  
result <- rbind(result, m)


  }
result <- as.data.frame(result)
  return(result)
}
