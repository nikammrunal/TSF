#diff betn binary logistic and decision tree
#binary log can work on only 2 variable whereas decision tree can have multiple variables
#decision trees are easy to build
#3 steps in CHAID
#1:preparing predictors   2:splitting data:identify variables which can significantly split the data into specific classes
#3:merging data:some variables can be merged but some variables cannot be meged so they should be kept splitted only
#when we want to know which model is better,then for comparion,we always use RMSE
#model which has least RMSE is better


#regression tree
#dep variable is continuous
#indep variable can be cont or categorical
#algo used->func-> rpart

#annova is used in decision tree
#it uses f test stat
#aov is used for data comparison
#there is a trade off betn f value nd p value......f<-variance


data(iris)
head(iris)
summary(iris)

#divide the data into training and testing(80-20)
index<-sample(nrow(iris),nrow(iris)*0.80)
train_iris<-iris[index,]
test_iris<-iris[-index,]


install.packages("rpart")
library(rpart)

treemodel<-rpart(Sepal.Length ~.,data = train_iris)
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(treemodel)


test_iris$pred_sepal<-predict(treemodel,test_iris,type = "class")#doubt
head(test_iris)
test_iris$res_tree<-test_iris$Sepal.Length - test_iris$pred_sepal
rmsetree<-sqrt(mean(test_iris$res_tree^2))
rmsetree

head(test_iris)
head(test_iris,1)

treemodel<-rpart(Species~.,data = train_iris)


lm_iris<-lm(Sepal.Length~.,data = train_iris)
test_iris$pred_sepal_lm<-predict(lm_iris,test_iris)
head(test_iris)

test_iris$res_lm<-test_iris$Sepal.Length - test_iris$pred_sepal_lm
rmselm<-sqrt(mean(test_iris$res_lm^2))
rmselm
rmsetree
head(test_iris)

