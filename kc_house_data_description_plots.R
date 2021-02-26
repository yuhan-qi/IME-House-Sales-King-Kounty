#package
library(xgboost)
library(ggplot2)
library(gridExtra)
library(grid)
library(corrplot)

# This file contains visulizations presented in the "Data Description" section of the final report.

house=read.csv("kc_house_data.csv",header=TRUE,sep=",")
price_per_sqft <- house$price / house$sqft_living
house$price_per_sqft_living = price_per_sqft

# boxplot for each variable
par(mfrow = c(3, 5))
boxplot(house$price, main = "price")
boxplot(house$price_per_sqft_living, main = "price per sqft_living")
boxplot(house$bedrooms, main = "bedrooms")
boxplot(house$bathrooms, main = "bathrooms")
boxplot(house$sqft_living, main = "sqft_living")
boxplot(house$sqft_lot, main = "sqft_lot")
boxplot(house$floors, main = "floors")
boxplot(house$condition, main = "condition")
boxplot(house$grade, main = "grade")
boxplot(house$sqft_above, main = "sqft_above")
boxplot(house$sqft_basement, main = "sqft_basement")
boxplot(house$yr_built, main = "yr_built")
boxplot(house$zipcode, main = "zip code")

# Scatter plot taking price as response variable
sc_plots1 = list()
sc_plots1$sc1 <- ggplot(house, aes(x=bedrooms, y=price)) + geom_point(size = 1)
sc_plots1$sc2 <- ggplot(house, aes(x=bathrooms, y=price)) + geom_point(size = 1)
sc_plots1$sc3 <- ggplot(house, aes(x=sqft_living, y=price)) + geom_point(size = 1)
sc_plots1$sc4 <- ggplot(house, aes(x=sqft_lot, y=price)) + geom_point(size = 1)
sc_plots1$sc5 <- ggplot(house, aes(x=floors, y=price)) + geom_point(size = 1)
sc_plots1$sc6 <- ggplot(house, aes(x=condition, y=price)) + geom_point(size = 1)
sc_plots1$sc7 <- ggplot(house, aes(x=grade, y=price)) + geom_point(size = 1)
sc_plots1$sc8 <- ggplot(house, aes(x=sqft_above, y=price)) + geom_point(size = 1)
sc_plots1$sc9 <- ggplot(house, aes(x=sqft_basement, y=price)) + geom_point(size = 1)
sc_plots1$sc10 <- ggplot(house, aes(x=yr_built, y=price)) + geom_point(size = 1)
sc_plots1$sc11 <- ggplot(house, aes(x=zipcode, y=price)) + geom_point(size = 1)
grid.arrange(sc_plots1$sc1, sc_plots1$sc2, sc_plots1$sc3, sc_plots1$sc4, 
             sc_plots1$sc5, sc_plots1$sc6, sc_plots1$sc7, sc_plots1$sc8, 
             sc_plots1$sc9, sc_plots1$sc10, sc_plots1$sc11,
             ncol = 4)

# Scatter plot taking price per sqft_living as response variable
sc_plots = list()
sc_plots$sc1 <- ggplot(house, aes(x=bedrooms, y=price_per_sqft_living)) + geom_point(size = 1)
sc_plots$sc2 <- ggplot(house, aes(x=bathrooms, y=price_per_sqft_living)) + geom_point(size = 1)
sc_plots$sc3 <- ggplot(house, aes(x=sqft_living, y=price_per_sqft_living)) + geom_point(size = 1)
sc_plots$sc4 <- ggplot(house, aes(x=sqft_lot, y=price_per_sqft_living)) + geom_point(size = 1)
sc_plots$sc5 <- ggplot(house, aes(x=floors, y=price_per_sqft_living)) + geom_point(size = 1)
sc_plots$sc6 <- ggplot(house, aes(x=condition, y=price_per_sqft_living)) + geom_point(size = 1)
sc_plots$sc7 <- ggplot(house, aes(x=grade, y=price_per_sqft_living)) + geom_point(size = 1)
sc_plots$sc8 <- ggplot(house, aes(x=sqft_above, y=price_per_sqft_living)) + geom_point(size = 1)
sc_plots$sc9 <- ggplot(house, aes(x=sqft_basement, y=price_per_sqft_living)) + geom_point(size = 1)
sc_plots$sc10 <- ggplot(house, aes(x=yr_built, y=price_per_sqft_living)) + geom_point(size = 1)
sc_plots$sc11 <- ggplot(house, aes(x=zipcode, y=price_per_sqft_living)) + geom_point(size = 1)
grid.arrange(sc_plots$sc1, sc_plots$sc2, sc_plots$sc3, sc_plots$sc4, 
             sc_plots$sc5, sc_plots$sc6, sc_plots$sc7, sc_plots$sc8, 
             sc_plots$sc9, sc_plots$sc10, sc_plots$sc11,
             ncol = 4)

# Correlation matrix for indepdent variables
truc <- data.frame(house$bedrooms, house$bathrooms, house$sqft_living, house$sqft_lot, house$floors, house$condition, house$grade, house$sqft_above, house$sqft_basement, house$yr_built, house$zipcode)
M<-cor(truc)
head(round(M,2))
corrplot(M, method="number", type="lower", tl.col="black", tl.srt=45)
