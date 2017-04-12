#
#The program you are about to use is one of my learning files as an R-Student
# Please enjoy 
#
#
# Decision tree
library(party)
library(rpart)

set.seed(101)
# Spliting the data into train and test set
ind=sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData=iris[ind==1,]
testData=iris[ind==2,]
myFormula=Species~Sepal.Length + Sepal.Width +Petal.Length + Petal.Width
#Decision tree implementation
iris_ctree=ctree(myFormula, data=trainData)
iris_ctree
# To print the results
table(predict(iris_ctree), trainData$Species)
# To make a simple plot of the decision tree
plot(iris_ctree,type="simple")
# Testting the Algorithm.
testPred=predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)
# A slight look at Recursive Partitioning and Regression Trees with pruning
iris_rpart= rpart(myFormula, data = trainData,control = rpart.control(minsplit = 10))
print(iris_rpart)
plot(iris_rpart)
text(iris_rpart, use.n=T)
opt=which.min(iris_rpart$cptable[,"xerror"])
cp=iris_rpart$cptable[opt, "CP"]
iris_prune= prune(iris_rpart, cp = cp)
print(iris_prune)
plot(iris_prune)
text(iris_prune, use.n=T)
DEXiris_pred=predict(iris_prune, newdata=testData)
xlim= range(DEXiris_pred)
plot(DEXiris_pred~Species, data=testData, xlab="Observed",
     ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)

