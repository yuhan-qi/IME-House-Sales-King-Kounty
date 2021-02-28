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

train_data = kc_house_truc[1:numberOfTrainingSamples,2:12]
train_label = kc_house_truc[1:numberOfTrainingSamples,1]
test_data = kc_house_truc[-(1:numberOfTrainingSamples),2:12]
test_label = kc_house_truc[-(1:numberOfTrainingSamples),1]

train_y = as.numeric(train_label)
train_x = data.matrix(train_data)
dtrain <- xgb.DMatrix(data = train_x, label= train_y)

test_y = as.numeric(test_label)
test_x = data.matrix(test_data)
dtest <- xgb.DMatrix(data = test_x, label= test_y)

#Create Grid for Grid Search
searchGridSubCol <- expand.grid(
  subsample = c(0.5, 1), 
  colsample_bytree = c(0.5, 1),
  max_depth = c(3, 5,10),
  min_child = seq(1), 
  eta = c(0.01, 0.1)
)

#Set initial ntrees to 1000 to tune other hyperparameters
ntrees = 1000

#Hyperparameter Tuning
system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = ntrees, nfold = 5, showsd = TRUE, 
                             metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                             "objective" = "reg:linear", "max.depth" = currentDepth, "eta" = currentEta,                               
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    rmse <- tail(xvalidationScores$test_rmse_mean, 1)
    trmse <- tail(xvalidationScores$train_rmse_mean,1)
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))}))

output <- as.data.frame(t(rmseErrorsHyperparameters))
varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild")
names(output) <- varnames
head(output)


#determine nrounds by cross-validation
params <- list(booster = "gbtree", objective = "reg:linear", 
               eta=0.01, max_depth=3, colsample_bytree=1.0, subsample=1)

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 10000, nfold = 5, 
                 showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)


#Fit the model with the tuned hyperparameters
bst_model <- xgboost(data = dtrain, # the data  
                     nrounds = 5050,
                     eta = 0.01,
                     max_depth = 3,
                     colsample_bytree = 1.0,
                     subsample = 1.0,
                     objective = "reg:linear")



# generate predictions for our held-out testing data
pred <- predict(bst_model, dtest)

#Error Check
#MSE
MSE= mean((test_y - pred)^2)
cat('The mean square error of the test data is ', round(MSE,3),'\n')

#RMSE
residuals = test_y - pred
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

#R_squared
test_y_mean = mean(test_y)
# Calculate total sum of squares
tss =  sum((test_y - test_y_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')


#Variable Importance Plot
vip(bst_model)

#Partial Dependence Plot & Individual Conditional Expectation 
#Hypothesis 1
PDP_1_price_non_centered <- partial(bst_model, pred.var = "grade", ice = TRUE, center = FALSE, 
              plot = TRUE, rug = TRUE,  plot.engine = "ggplot2",
              train = train_data)
PDP_1_price_centered <- partial(bst_model, pred.var = "grade", ice = TRUE, center = TRUE, 
              plot = TRUE, rug = TRUE,  plot.engine = "ggplot2",
              train = train_data)


#Hypothesis 2
PDP_2_non_centered <- partial(bst_model, pred.var = "yr_built", ice = TRUE, center = FALSE, 
                              plot = TRUE, rug = TRUE,  plot.engine = "ggplot2",
                              train = train_data)
PDP_2_centered <- partial(bst_model, pred.var = "yr_built", ice = TRUE, center = TRUE, 
                          plot = TRUE, rug = TRUE,  plot.engine = "ggplot2",
                          train = train_data)

#Hypothesis 3
PDP_3_non_centered <- partial(bst_model, pred.var = "condition", ice = TRUE, center = FALSE, 
                              plot = TRUE, rug = TRUE,  plot.engine = "ggplot2",
                              train = train_data)
PDP_3_centered <- partial(bst_model, pred.var = "condition", ice = TRUE, center = TRUE, 
                          plot = TRUE, rug = TRUE,  plot.engine = "ggplot2",
                          train = train_data)

#Hypothesis 4
PDP_4_price_non_centered <- partial(bst_model, pred.var = "floors", ice = TRUE, center = FALSE, 
                                    plot = TRUE, rug = TRUE,  plot.engine = "ggplot2",
                                    train = train_data)
PDP_4_price_centered <- partial(bst_model, pred.var = "floors", ice = TRUE, center = TRUE, 
                                plot = TRUE, rug = TRUE,  plot.engine = "ggplot2",
                                train = train_data)

#Hypothesis 5
PDP_5_price_non_centered <- partial(bst_model, pred.var = "sqft_lot", ice = TRUE, center = FALSE, 
                                    plot = TRUE, rug = TRUE,  plot.engine = "ggplot2",
                                    train = train_data)
PDP_5_price_centered <- partial(bst_model, pred.var = "sqft_lot", ice = TRUE, center = TRUE, 
                                plot = TRUE, rug = TRUE,  plot.engine = "ggplot2",
                                train = train_data)


#Interactions
p1 <- partial(bst_model, pred.var = c("grade", "condition"), plot = TRUE, chull = TRUE, plot.engine = "ggplot2", train = train_data)
p2 <- partial(bst_model, pred.var = c("grade", "sqft_lot"), plot = TRUE, chull = TRUE, plot.engine = "ggplot2", train = train_data)
p3 <- partial(bst_model, pred.var = c("grade", "floors"), plot = TRUE, chull = TRUE, plot.engine = "ggplot2", train = train_data)
p4 <- partial(bst_model, pred.var = c("grade", "yr_built"), plot = TRUE, chull = TRUE, plot.engine = "ggplot2", train = train_data)
p5 <- partial(bst_model, pred.var = c("condition", "sqft_lot"), plot = TRUE, chull = TRUE, plot.engine = "ggplot2", train = train_data)
p6 <- partial(bst_model, pred.var = c("condition", "floors"), plot = TRUE, chull = TRUE, plot.engine = "ggplot2", train = train_data)
p7 <- partial(bst_model, pred.var = c("condition", "yr_built"), plot = TRUE, chull = TRUE, plot.engine = "ggplot2", train = train_data)
p8 <- partial(bst_model, pred.var = c("yr_built", "floors"), plot = TRUE, chull = TRUE, plot.engine = "ggplot2", train = train_data)
p9 <- partial(bst_model, pred.var = c("yr_built", "sqft_lot"), plot = TRUE, chull = TRUE, plot.engine = "ggplot2", train = train_data)
p10 <- partial(bst_model, pred.var = c("floors", "sqft_lot"), plot = TRUE, chull = TRUE, plot.engine = "ggplot2", train = train_data)


grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,ncol = 2)