library("parallel")
library("xgboost")
library("caret")
library("doParallel")
library("Metrics")
library("vip")
library("iml")
library("pdp")
library("ggplot2")

#Add price_per_sqft_living to the variables
kc_house_truc$price_per_sqft_living <- with(kc_house_truc, price/sqft_living)

#Number of rows of the data
n=nrow(kc_house_truc)

#Divide the dataset into training dataset and test dataset
numberOfTrainingSamples <- round(nrow(kc_house_truc) * 0.7)

#Create a new Dataframe
newdf <- data.frame(bedrooms=kc_house_truc$bedrooms[1:n], 
                    bathrooms=kc_house_truc$bathrooms[1:n],
                    sqft_living=kc_house_truc$sqft_living[1:n],
                    sqft_lot=kc_house_truc$sqft_lot[1:n],
                    floors= kc_house_truc$floors[1:n],
                    condition=kc_house_truc$condition[1:n],
                    grade=kc_house_truc$grade[1:n],
                    sqft_above=kc_house_truc$sqft_above[1:n],
                    sqft_basement=kc_house_truc$sqft_basement[1:n],
                    yr_built= kc_house_truc$yr_built[1:n],
                    zipcode= kc_house_truc$zipcode[1:n],
                    price_per_sqft_living=kc_house_truc$price_per_sqft_living[1:n]
                    )


train = newdf[1:numberOfTrainingSamples,]
test = newdf[-(1:numberOfTrainingSamples),]
test_data = newdf[-(1:numberOfTrainingSamples),1:11]
test_label = newdf[-(1:numberOfTrainingSamples),12]

#Fit Multiple Liear Regression
M1<-lm(price_per_sqft_living ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+condition+grade+sqft_above+sqft_basement+yr_built+
         zipcode, data=train)
summary(M1)

#Use the fitted model for the prediction on the test data set
predition <- predict(M1, newdata = test_data)

#Error Check
#MSE
MSE = mean((test_label - predition)^2)
cat('The mean square error of the test data is ', round(MSE,3),'\n')

#RMSE
residuals = test_label - prediction
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
