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

## Plotting summary results
library(ggplot2)

results <- data.frame("dnafit"=alleles$rate,
                      "predictions"=predictions)
theme_set(theme_light())
g <- ggplot(data = results,aes(x=dnafit,y=predictions)) + geom_point(colour="steelblue")
g <- g + geom_smooth(method = "lm",formula = y~x,colour="red")
g <- g + labs(x="P/E (DNAfit)",y="P/E (Linear regression)")
g <- g + theme(text = element_text(size=26,face="bold"))
plot(g)


## run example
newSample <- sample(1:nrow(alleles),1)
model <- lm(rate~.,alleles[-newSample,])
unname(predict(model,newdata = alleles[newSample,]));alleles$rate[newSample]
