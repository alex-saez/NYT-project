library(topicmodels)

setwd("/Users/Alex/Dropbox/NYT project/")
load("./data/database.RData")
load("./data/topic_models/topicmodel_dtm2_100_Gibbs.RData")

data = database[grepl('e',database$content),] # only articles with content
data = data[1:topicmodel@Dim[1],]
remove(database)

X = topicmodel@gamma
y = as.factor(data$most_emailed != 0)

n = nrow(data)
ind = sample(n)
train_ind = ind[1:round(0.8*n)]

# SVM-----------------------------------------------------------------------------
library(e1071)

tuneind = seq(1, nrow(data), by = 13)
obj = tune.svm(X[tuneind,], y[tuneind], gamma = 10^(-2:2), cost = 10^(-2:2))

SVM = svm(X[train_ind,], y[train_ind], cost=0.01, gamma=0.01, type="C-classification", kernel="radial") 

yhat = predict(SVM, X[-train_ind,])
sum(yhat==y[-train_ind])/length(y[-train_ind])

# AdaBoost-----------------------------------------------------------------------------
library("rpart")
library("adabag")

D = as.data.frame(X)
D = data.frame(y=y,D)
D_train = D[train_ind,]
D_test = D[-train_ind,]

cnt = rpart.control(maxdepth=1, cp=-1, minsplit=0)
ada = boosting(y ~ ., data = D_train, boos=TRUE, coeflearn="Freund", mfinal=100, control=cnt)

# compute testing accuracy
y_hat = predict(ada, newdata= D_test)
y_hat = y_hat$class
sum(y_hat==as.character(D_test$y))/length(y_hat)

imp = sort(ada$importance, decreasing = TRUE)
data.frame(imp[1:20])




