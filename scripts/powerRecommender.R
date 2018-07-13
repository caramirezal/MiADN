## Construction of a linear regression model for the prediction of
## power over endurance values based on genotypic information

## Loading data for model construction
power <- read.csv("../data/power.csv")
## selecting only columns containing snps 
alleles <- ! grepl("effect",names(power))
alleles <- power[,alleles]

## selecting alleles column 3-etc, containing snps and
## power/endurances output values(=rate)
alleles <- alleles[,colnames(alleles)!="clientId"]
head(alleles)

## vector to store predictions 
predictions <- numeric(nrow(alleles))

## Perform a Leave-One-Out Cross-Validation (LOOC) 
for (i in 1:nrow(alleles)){
        ## construction of the model on n-1 observations
        ## n the number of total observations
        model <- lm(rate~.,alleles[-i,])
        
        ## getting the prediction
        predictions[i] <- predict(model,newdata = alleles[i,])
}

## mean squared error
mse <- sqrt(mean((predictions-alleles$rate)^2))

## model fit
plot(alleles$rate,predictions,
     col="steelblue",pch=20,
     xlab = "P/E (DNAfit )",ylab = "P/E (Linear Regression)",
     font=2,font.lab=2,cex.lab=1.2,cex=1.2)
abline(0,1,col="red",lwd=3)

## Deviation from true values
hist(predictions-alleles$rate,breaks = 1000,
     main = "",xlab = "Deviation from true values",
     font.lab=2,cex.lab=1.2,cex=1.2)

coef(model)

## run example
newSample <- sample(1:nrow(alleles),1)
model <- lm(rate~.,alleles[-newSample,])
predict(model,newdata = alleles[newSample,])
alleles$rate[newSample]
