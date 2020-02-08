#import data

setwd('C:/Users/dell/Desktop/iti/ITI_DATAMINING/Course Project')
prices <- read.csv('House_Predict.csv', sep = ',')

colnames(prices)

#heat map ot find the correlation between salesprice and all other variables

prices.numeric <- prices[,sapply(prices, is.numeric)] #to remove any non numeric columns
colnames(prices.numeric)
prices.numeric <- na.omit(prices.numeric) #to remove rows with null values
price.cor <- cor(prices.numeric, method = "pearson")
heatmap(price.cor)

#scatter plots to view highly correlated and low correlated variables

scatter.smooth(x = prices$SalePrice, y = prices$OverallQual) #high correlation
scatter.smooth(x = prices$SalePrice, y = prices$OverallCond) #low correlation

#histogram

hist(prices$SalePrice)

#splitting data

index <- sample(1:nrow(prices), 0.8*nrow(prices))

traindata <- prices[index, ]
testdata <- prices[-index, ]

#training and testing the model using linear regression
#the variables with the highest correlation are selected
modeltrain <- lm(SalePrice ~ OverallQual + GarageCars + GarageArea + X1stFlrSF + TotalBsmtSF + GrLivArea, data = prices)
modeltest <- predict(modeltrain, testdata)

summary(modeltest)
MSE <- mean((testdata$SalePrice - modeltest) ^2)
print(MSE)

write.csv(modeltest, "linear_results.csv")

#neural networks
library(neuralnet)

nn <- neuralnet(SalePrice ~ OverallQual + GarageCars + GarageArea + X1stFlrSF + TotalBsmtSF + GrLivArea, data = traindata, hidden = 3, act.fct = "logistic")
plot(nn)

test.df <- data.frame(testdata$OverallQual, testdata$GarageCars, testdata$GarageArea, testdata$X1stFlrSF, testdata$TotalBsmtSF, testdata$GrLivArea)

predict <- compute(nn, test.df)
predict$net.result

write.csv(predict$net.result, "nn_results.csv")