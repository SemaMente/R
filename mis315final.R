library(MASS)
library(tidyverse)
library(tree)
library(PerformanceAnalytics)
library(corrplot)
library(glmnet)
library(ISLR)
library(randomForest)
getwd()

list.files()

videoGames <- read.csv("FinalDataset.csv" , sep = ',', header = TRUE, stringsAsFactors = F, dec = '.')
names(videoGames)
#importladığın csv dosyasının adını videogames olarak değiştirdim
#iki defa importladım


#a) Analyze your predictors/independent variables using techniques shown in the class.
#The distribution of a variable will depend on whether the variable is categorical or continuous.

#get information about dataset(mean, median,etc)
summary(videoGames)
#It is continuous variables

#ggplot(data = videoGames) +
 # geom_point(mapping = aes(x = Year, y = Platform, color = 'red' ))

#ggplot(data = videoGames, mapping = aes(x = Year, y = Platform)) +
#geom_boxplot(mapping = aes(group = cut_width(Year, 0.1)))

ggplot(data = videoGames) +
  geom_bar(mapping = aes(x = Platform)) 

ggplot(data = videoGames) +
  geom_bar(mapping = aes(x = Year)) 

ggplot(data = videoGames) +
  geom_count(mapping = aes(x = Year, y = Platform))

#b) Take the first 1000 observation as the training set and use the rest as the test set.
 videoGames <- videoGames[1:1803,] 
dataset <- videoGames[,1:9]
#dataset2 <- datasest[,4:8] do not take first 4 variables

train <- dataset [1:1000,]#take the first 1000 dataset obs
test <- dataset[1001: nrow(dataset),]#residual dataset obs
names(dataset)

#c) Fit a multiple regression model to predict “Global_Sales”. Use only the training set to fit the regression model
lm.fit <- lm(formula = Global_Sales ~.  , data = train)#lineer model
confint(lm.fit)
coef(lm.fit)

#d Analyze your estimated regression models. Comment on coefficients, adjusted R square and F statistic of the model.
summary(lm.fit) #1:27 yorumlicaksın

#e  Predict “Global_Sales” in the test set using the regression model obtained in (c). Calculatethe root mean square error of the test set (RMSE)
testpredict <- predict(lm.fit, newdata=test )
summary(testpredict)
#MSE
MSE <- (1/nrow(test))*sum((test$Global_Sales-testpredict)^2)
MSE #mean square error
RMSE <- sqrt(MSE)
RMSE #root mean square error
RMSE1 <- log(RMSE)

#f Fit a Ridge model and a Lasso model to predict “Global_Sales”. Use only the training set
#to fit these regression models. Determine the lambda parameter using cross-validation.

#Ridge
set.seed(1)
train.mat <- model.matrix(Global_Sales ~ ., data = train)
test.mat <- model.matrix(Global_Sales ~ ., data = test)

library(glmnet)
grid <- 10 ^ seq(10, -2, length = 100)
fit.ridge <- glmnet(train.mat, train$Global_Sales, alpha = 0, lambda = grid)
summary(fit.ridge)
cv.ridge <- cv.glmnet(train.mat, train$Global_Sales, alpha = 0, lambda = grid)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge
plot(cv.ridge)
log(bestlam.ridge)
#comment compare

#Lasso #alpha number change
set.seed(1)
grid <- 10 ^ seq(10, -2, length = 100)
fit.lasso <- glmnet(train.mat, train$Global_Sales, alpha = 1, lambda = grid)
cv.lasso <- cv.glmnet(train.mat, train$Global_Sales, alpha = 1, lambda = grid)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso
plot(cv.lasso)
log(bestlam.lasso)


#g) Analyze your Lasso Model. Compare your Lasso Model with the multiple regression model estimated in (c).
#burda çıkan değerlerle linear regression değerlerle kıyasla
predict(fit.lasso, s = bestlam.lasso, type = "coefficients") #these are the values selected by lasso model

plot(cv.lasso$glmnet.fit, "lambda", label=TRUE)


#h) Predict “Global_Sales” in the test set using the Ridge model and the Lasso model obtained in (f). Calculate RMSEs of these models only using the test set.

pred.ridge <- predict(fit.ridge, s = bestlam.ridge, newx = test.mat)
MSE2 <- mean((pred.ridge - test$Global_Sales)^2)
RMSE2 <- log(sqrt(MSE2)) #root mean sqre error
RMSE2

pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
MSE3 <- mean((pred.lasso - test$Global_Sales)^2)
RMSE3 <- log(sqrt(MSE3))
RMSE3
#ridge perform better than the losso

#i) Fit a regression tree to predict “Global_Sales”. Use only the training set to fit the regression model. Determine the number of terminal nodes using cross-validation.
library(tree)
set.seed(1)
tree.videoGames <- tree(Global_Sales~., data = train)
summary(tree.videoGames)
plot(tree.videoGames)
text(tree.videoGames)

cv.videoGames <- cv.tree(tree.videoGames)
plot(cv.videoGames$size, cv.videoGames$dev, type='b')
#optimal cross val
prune.videoGames <- prune.tree(tree.videoGames,best=8)
plot(prune.videoGames)
text(prune.videoGames,pretty=0)

#j) Predict “Global_Sales” in the test set using the regression tree model obtained in (i). Calculate the RMSE of the regression tree only using the test set.
yhat <- predict(prune.videoGames,newdata=test)
#boston.test =videoGames[-train.mat,"Global_Sales"]
boston.test <- test[,"Global_Sales"]
MSE4 <- mean((yhat-boston.test)^2)
RMSE4 <- log(sqrt(MSE4))
RMSE4

#k
library(randomForest)
set.seed(1)

cvvideoGames <- train[sample(nrow(train)),]
folds <- cut(seq(1,nrow(cvvideoGames)),breaks=5,labels=FALSE)

total_mse <- rep(NA,9)
for (i in 1:8) {
  mse <- rep(NA,5)
  #5-fold cross validation
  for (t in 1:5){
    set.seed(1)
    cv_test_index <- which(folds==t,arr.ind=TRUE)
    cv_train <- train[-cv_test_index,]
    cv_test <- train[cv_test_index,]
    rf.players <- randomForest(Global_Sales~., data=cv_train, mtry= i, 
                               ntree=500, importance=TRUE, na.action = na.omit)
    pred <- predict(rf.players,newdata=cv_test)
    mse[t] <- (1/nrow(cv_test))*sum((pred-cv_test$Global_Sales)^2)
  }
  total_mse[i] <- mean(mse)
}

min_mtry <- which.min(total_mse)
min_mtry

#l According to the random forest obtained in (k), which variables are import? Comment. 

rf.videoGames <- randomForest(Global_Sales~., data=train, mtry=5, 
                           ntree=500, importance=TRUE, na.action = na.omit)
importance(rf.videoGames)
varImpPlot(rf.videoGames, type=1) #most important criticscore less important year and platform
varImpPlot(rf.videoGames)

#m Predict “Global_Sales” in the test set using the random forest model obtained in (k). Calculate the RMSE of the random forest only using the test set.
set.seed(1)
rf.videoGames <- randomForest(Global_Sales~., data=train, mtry=5, 
                           ntree=500, importance=TRUE, na.action = na.omit)
yhat.rf <- predict(rf.videoGames,newdata=test)

MSE5 <- mean((yhat.rf-test$Global_Sales)^2)
RMSE5 <- log(sqrt(MSE5))
RMSE5
#n Compare RMSEs obtained in (e), (h), (j) and (m).
c(RMSE1,RMSE2,RMSE3,RMSE4,RMSE5)
#en düşüğü best 
