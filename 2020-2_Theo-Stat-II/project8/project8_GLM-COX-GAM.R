setwd("C:/users/jiwon/desktop/theostat/hw/hw8")

library(tidyverse)
library(GGally)
library(ggplot2)
library(DMwR)
library(randomForest)
library(ROCR)
library(ROSE)
library(caret)
library(e1071)
library(xgboost)
library(Matrix)
library(DiagrammeR)
library(MASS)
library(corrr)

data = read_csv("data_new.csv") 
train <- data[data$ID<=3168,]
test <- data[data$ID>3168,]

delete_id = c(169, 574,747, 1257, 1327, 2643, 2698, 2876, 2892,3745, 3782, 4098, 4299, 4575, 4627, 4952, 5610, 5642, 5926, 5946, 6029, 6204)
index_train <- c(169, 574,747, 1257, 1327, 2643, 2698, 2876, 2892)
index_test <- c(3745, 3782, 4098, 4299, 4575, 4627, 4952, 5610, 5642, 5926, 5946, 6029, 6204)

res_train = data.frame(id=index_train, prob=1, delta=1)
res_test = data.frame(ID=index_test, rf_test=1, svm_test=1, xgb_test=1)

train_mat = data.matrix(train)
train_mat %>% dim()
train_mat[,35] = as.factor(train$x41)
train_mat[,36] = as.factor(train$x42)

test_mat = data.matrix(test)
test_mat[,35] = as.factor(test$x41)
test_mat[,36] = as.factor(test$x42)

var = names(data)[2:39]
lift <- function(predhat) {
  result <- data.frame(predhat=predhat,id=train$ID,delta=train$delta) %>% 
    rbind(data.frame(predhat=1,id=delete_id[which(delete_id<=3168)],delta=1)) %>% arrange(-predhat) %>%
    mutate(deltahat = ifelse(rank(-predhat)<=317,1,0),
           interval = rep(c(1,2,3,4,5,6,7,8,9,10),c(rep(317,8),rep(316,2)))) %>% 
    arrange(id)
  percent <- result %>% group_by(interval) %>% summarise(per = sum(delta==1)/length(delta)) 
  ratio = 108/3168
  list(result=result,percent=percent,lift=percent$per/ratio)
}

##### rf ####

rf <- randomForest(factor(delta)~., data=train_mat[,var], mtry= floor(sqrt(length(var))),
                   importance=T)
prob_rf <- predict(rf, train_mat[,var[1:37]], type='prob')[,2]
res_rf = data.frame(id = train$ID, prob = prob_rf, delta = train$delta)
res_rf = res_rf[order(res_rf$prob,decreasing=T),]
class = rep(0,dim(res_svm)[1])
class[1:round(dim(res_svm)[1]*0.1)]=1
res_rf$class =class
acc_rf = mean(res_rf$delta==res_rf$class)
table(res_rf$delta,class)

rf_test = tibble(ID = test$ID,prob = predict(rf, test_mat[,var[1:37]], type='prob')[,2])%>% arrange(-prob)
delta = rep(0,dim(test)[1]); delta[1:round(dim(test)[1]*0.1)]=1
rf_test$delta = delta 
rf_test = rf_test %>% arrange(ID)


####### svm #######
svm <- svm(factor(delta)~., data=train[,var], probability=T)
prob_svm <- attr(predict(svm, train[,var[1:37]], probability=T), 'probabilities')[,2]

res_svm = data.frame(id = train$ID, prob = prob_svm, delta = train$delta)
res_svm = res_svm[order(res_svm$prob,decreasing=T),]
res_svm$class =class
acc_svm = mean(res_svm$delta==res_svm$class)

table(res_svm$delta, class) #confusion matrix


svm_test = tibble(ID = test$ID, 
                  prob = attr(predict(svm, test[,var[1:37]], probability=T),'probabilities')[,2])%>% 
  arrange(-prob)

svm_test$delta = delta 
svm_test = svm_test %>% arrange(ID)

####### XGB #######
### 변수 리스트

dtrain = xgb.DMatrix(data=train_mat[,2:38],label =train_mat[,39])
bst <- xgboost(data = dtrain, max.depth = 32, 
                      eta = 1, nthread = 2, nrounds = 3, objective = "binary:logistic")
prob_xgb = predict(bst, train_mat[,2:38])
res_xgb = data.frame(id = train$ID, prob = prob_xgb, delta = train$delta)
res_xgb = res_xgb[order(res_xgb$prob,decreasing=T),]

res_xgb$class= class
acc_xgb = mean(res_xgb$delta == res_xgb$class)

xgb_test = tibble(ID = test$ID,
                  prob = predict(bst, test_mat[,2:38]))%>% 
  arrange(-prob)

xgb_test$delta = delta 
xgb_test = xgb_test %>% arrange(ID)



test = rf_test %>% left_join(svm_test,by="ID") %>% left_join(xgb_test, by = "ID")
test = test %>%   dplyr::select(ID, delta.x, delta.y, delta) %>% rename(rf_test=delta.x,
                                                                 svm_test = delta.y,
                                                                 xgb_test = delta)
test = rbind(res_test,test) %>% arrange(ID)



res_train <- data.frame(index_train, rep(1, length(index_train)), rep(1, length(index_train)))
res_test <- data.frame(index_test, rep(1, length(index_test))); colnames(res_test) <- c('ID', 'prob')

## LDA ##
lda <- lda(formula=as.factor(delta)~., data=train[,-c(1,40)])
prob_lda <- predict(lda, train[,-c(1,40)], type='prob')$posterior[,2]

res_lda <- data.frame(train$ID, prob_lda, train$delta)
colnames(res_lda) <- c('ID', 'prob_lda', 'delta')
colnames(res_train) <- c('ID', 'prob_lda', 'delta')
colnames(res_test) <- c('ID', 'prob_lda', 'delta')
res_lda <- rbind(res_lda, res_train)
dim(res_lda)

class_lda <- rep(0, dim(res_lda)[1])
class_lda[order(res_lda$prob_lda, decreasing=T)[1:round(dim(res_lda)[1]*0.1)]]=1
table(class_lda)
correct_lda <- (class_lda==res_lda$delta)
acc_lda <- mean(correct_lda)
acc_lda
confusionMatrix(as.factor(res_lda$delta), as.factor(class_lda))


## KNN ##
knn <- knn3(as.factor(delta)~., data=train[,-c(1,40)], k=5)
prob_knn <- predict(knn, train[,-c(1,40)], type='prob')[,2]
table(predict(knn, train[,-c(1,40)], type='class'))

res_knn <- data.frame(train$ID, prob_knn, train$delta)
colnames(res_knn) <- c('ID', 'prob_knn', 'delta')
colnames(res_train) <- c('ID', 'prob_knn', 'delta')
colnames(res_test) <- c('ID', 'prob_knn', 'delta')
res_knn <- rbind(res_knn, res_train)
dim(res_knn)

class_knn <- rep(0, dim(res_knn)[1])
class_knn[order(res_knn$prob_knn, decreasing=T)[1:round(dim(res_knn)[1]*0.1)]]=1
table(class_knn)
correct_knn <- (class_knn==res_knn$delta)
acc_knn <- mean(correct_knn)
acc_knn
confusionMatrix(as.factor(res_knn$delta), as.factor(class_knn))


data.frame(knn = acc_knn, lda = acc_lda, rf = acc_rf, svm = acc_svm, xgb =acc_xgb,
           row.names = "accuracy")

lift_rf = lift(prob_rf)
lift_svm = lift(prob_svm)
lift_xgb = lift(prob_xgb)


lift_lda = lift(prob_lda)
lift_knn = lift(prob_knn)

lift_chart = lift_rf$percent %>% left_join(lift_svm$percent, by="interval") %>%
  left_join(lift_xgb$percent, by = "interval") %>% rename(RF = per.x,
                                                          SVM = per.y,
                                                          XGB = per) %>%
  left_join(lift_lda$percent, by = "interval") %>%
  left_join(lift_knn$percent, by = "interval") %>% rename(LDA = per.x,
                                                          KNN = per.y)
lift_chart %>% ggplot() + 
  geom_line(aes(factor(interval),RF,color="RF"),size=1,group=1) + 
  geom_point(aes(factor(interval),RF,color="RF"),size=3) +
  geom_line(aes(factor(interval),SVM,color="SVM"),size=1,group=1) + 
  geom_point(aes(factor(interval),SVM,color="SVM"),size=3) +
  geom_line(aes(factor(interval),XGB,color="XGB"),size=1,group=1) + 
  geom_point(aes(factor(interval),XGB,color="XGB"),size=3) +
  geom_line(aes(factor(interval),LDA,color="LDA"),size=1,group=1) + 
  geom_point(aes(factor(interval),LDA,color="LDA"),size=3) +
  geom_line(aes(factor(interval),KNN,color="KNN"),size=1,group=1) + 
  geom_point(aes(factor(interval),KNN,color="KNN"),size=3) + theme_bw() +
  xlab("Interval") + ylab("Percentage") 
