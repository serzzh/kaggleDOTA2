url<-paste0('features.csv')
features<-read.csv(url)
d<-features
d[is.na(d)] <- 0

require(caret)
set.seed(99)
inTrain <- createDataPartition(y=d$radiant_win, p=0.9, list=FALSE)
training <- d[inTrain,]
testing <- d[-inTrain,]
dim(training); dim(testing)

train.feat<-training[,-c(1,2,105:109)]
test.feat<-testing[,-c(1,2,105:109)]

rfFit <- train(train.feat, training$radiant_win, method="rf",trControl = trainControl(method = 'cv', number = 4))

pred <- predict(rfFit, newdata=test.feat)
RSS<-sum((pred-testing$radiant_win)^2)
postResample(pred, testing$radiant_win)
