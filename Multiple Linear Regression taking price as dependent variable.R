library("parallel")
library("xgboost")
library("caret")
library("doParallel")
library("Metrics")
library("vip")
library("iml")
library("pdp")
library("ggplot2")



#Divide the dataset into training dataset and test dataset
numberOfTrainingSamples <- round(nrow(kc_house_truc) * 0.7)

train = kc_house_truc[1:numberOfTrainingSamples,]
test_data = kc_house_truc[-(1:numberOfTrainingSamples),2:12]
test_label = kc_house_truc[-(1:numberOfTrainingSamples),1]


#Fit Multiple Liear Regression
M0<-lm(price ~., data=train)
summary(M0)

#Use the fitted model for the prediction on the test data set
pred <- predict(M0, newdata = test_data)

#Error Check
#MSE
MSE = mean((test_label - pred)^2)
cat('The mean square error of the test data is ', round(MSE,3),'\n')

#RMSE
residuals = test_label - pred
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

#R_Square
test_label_mean = mean(test_label)
# Calculate total sum of squares
tss =  sum((test_label - test_label_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')
