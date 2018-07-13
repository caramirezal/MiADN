library(caret)

power <- read.csv("../data/power.csv")

alleles <- ! grepl("effect",names(power))
alleles <- power[,alleles]

## getting the rate desired output
#output <- alleles$rate

## selecting alleles column 3-etc
alleles <- alleles[,colnames(alleles)!="clientId"]
head(alleles)

## trainning the model
train <- createDataPartition(alleles$rate,p=0.9,list = FALSE)

model <- lm(rate~.,alleles[train,])

prediction <- predict(model,newdata = alleles[-train,])

plot(alleles$rate[-train],prediction,pch=22)
abline(0,1,col="red")
