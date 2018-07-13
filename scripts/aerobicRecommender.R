library(randomForest)
library(caret)

## loading data
alleles <- read.csv("../data/aerobic.csv",stringsAsFactors = FALSE)

## formatting data to standardize language and select only alleles data
alleles$level <- sapply(alleles$level,
                        function(x) ifelse(x=="high","alto",x))
alleles$level <- sapply(alleles$level,
                        function(x) ifelse(x=="low","bajo",x))
alleles$level <- sapply(alleles$level,
                        function(x) ifelse(grepl("mediate",x),"intermedio",x))
alleles$level <- as.factor(alleles$level)
## removing non alleles data
alleles <- alleles[,!grepl("effect",colnames(alleles))]
## removing client id first column
alleles <- alleles[,-1]
head(alleles)

## performing a random forest machine learning algorithm
train <- createDataPartition(alleles$level,p=0.9,list = FALSE)
forest <- randomForest(level~.,data = alleles.m[train,])
forest.pred <- predict(forest,newdata = alleles.m[-train,])

## visualizing accuracy results
confusionMatrix(alleles$level[-train],forest.pred)

forest$importance
