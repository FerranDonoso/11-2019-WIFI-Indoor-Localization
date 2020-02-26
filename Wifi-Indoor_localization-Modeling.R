#### Usefull functions for error ploting
pabserror_split <- function(pred, real, building, z) {
  AELAT <- abs(pred - real)
  errors <- data.frame(abs = AELAT, b = building)
  dens <- density(AELAT, na.rm = TRUE)
  df <- data.frame(x = dens$x, y = dens$y)
  probs <- c(0.75)
  quantiles <- quantile(AELAT, prob = probs)
  df$quant <- factor(findInterval(df$x, quantiles))
  p1 <- ggplot(df, aes(x, y)) + geom_line(size = 1.5) + geom_ribbon(aes(ymin = 0, ymax = y, fill = quant)) +
    scale_fill_brewer(guide = "none") + 
    scale_x_continuous(breaks = seq(0, 120, 20)) +
    labs(x = "Absolute Error", y = "Density",
         title = paste("Density of global error in", z))
  p2 <- ggplot(errors,aes(abs, fill = b)) + geom_density(aes(y = ..density..)) +
    facet_wrap( ~b) + xlab("Absolute error") + 
    ggtitle("Densities of absolute error by building") + 
    labs(x = "Absolute Error", y = "Density", fill = "Building")
  p.both <- arrangeGrob(p1, p2)
  grid::grid.draw(p.both)
}

#### building prediction ####
metrics_b <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(metrics_b) <- c("Accuracy", "Kappa", "Model", "Set")

set.seed(123)

#### Using a method that uses all the WAPS
WAPs <- grep("WAP", names(trainingData), value = T)

## Train set:
Building_Model_ranger <- ranger::ranger(BUILDINGID ~., data = trainingData[, c(WAPs, "BUILDINGID")])

Building_Model_ranger_cm_train <- confusionMatrix(trainingData$BUILDINGID, Building_Model_ranger$predictions) 

## Validation set:
Building_Model_ranger_val_pred <- predict(Building_Model_ranger, validationData[, c(WAPs)])

Building_Model_ranger_cm_val <- confusionMatrix(validationData$BUILDINGID, Building_Model_ranger_val_pred[[1]]) 

metrics_b[nrow(metrics_b) + 1, ] <- c(Building_Model_ranger_cm_train$overall[1:2], "AllWaps", "Training")
metrics_b[nrow(metrics_b) + 1, ] <- c(Building_Model_ranger_cm_val$overall[1:2], "AllWaps", "Validation")

#### Building-floor prediction ####

BF_Model_ranger <- ranger::ranger(BUILDINGID_FLOOR ~., data = trainingData[, c(WAPs, "BUILDINGID_FLOOR")]) 

BF_Model_ranger_cm_train <- confusionMatrix(trainingData$BUILDINGID_FLOOR, BF_Model_ranger$predictions)

BF_Model_ranger_pred_val <- predict(BF_Model_ranger, validationData[, c(WAPs)])

BF_Model_ranger_cm_val <- confusionMatrix(validationData$BUILDINGID_FLOOR, BF_Model_ranger_pred_val[[1]])

### Model to predict the Building based on the id's of the three WAPs for which
### the highest signal is captured.

predictors_train <- grep("TopW", colnames(trainingData))
predictors_train <- c(predictors_train, which(colnames(trainingData) == "BUILDINGID"))
predictors_validation   <- grep("TopW", colnames(validationData))
predictors_validation   <- c(predictors_validation, which(colnames(validationData) == "BUILDINGID"))

Building_ModelTop3_ranger <- ranger::ranger(BUILDINGID ~., data = trainingData[, c(predictors_train)])

###
Building_ModelTop3_ranger_cm_train <- confusionMatrix(trainingData$BUILDINGID, Building_ModelTop3_ranger$predictions)

Building_ModelTop3_ranger_val_pred <- predict(Building_ModelTop3_ranger, validationData[, c(predictors_validation)])

###
Building_ModelTop3_ranger_cm_val   <- confusionMatrix(validationData$BUILDINGID, Building_ModelTop3_ranger_val_pred$predictions)

metrics_b[nrow(metrics_b) + 1, ] <- c(Building_ModelTop3_ranger_cm_train$overall[1:2], "Top3Waps", "Training")
metrics_b[nrow(metrics_b) + 1, ] <- c(Building_ModelTop3_ranger_cm_val$overall[1:2], "Top3Waps", "Validation")

metrics_b[, c(1, 2)] <- mapply(as.numeric,metrics_b[, c(1, 2)])

#### Plotting error metrics for building
ggplot(metrics_b, aes(x = Set, y = Accuracy, fill = Set)) + geom_col() + facet_wrap( ~Model) +
  ggtitle("Accuracy of models for predicting building ID")
ggplot(metrics_b, aes(x = Set, y = Kappa, fill = Set)) + geom_col() + facet_wrap( ~Model) +
  ggtitle("Kappa of models for predicting building ID") 

####
set.seed(123) 

metrics_bf <- metrics_bf <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(metrics_bf) <- c("Accuracy", "Kappa", "Model", "Set")

#### Model to predict Building-floor with all the WAPs

BF_Model_allwaps <- ranger::ranger(BUILDINGID_FLOOR ~.,trainingData[, c(WAPs, "BUILDINGID_FLOOR")])

BF_Model_allwaps_cm_train <- confusionMatrix(trainingData$BUILDINGID_FLOOR, BF_Model_allwaps$predictions)

BF_Model_allwaps_pred_val <- predict(BF_Model_allwaps, validationData[, c(WAPs)])

BF_Model_allwaps_cm_val <- confusionMatrix(validationData$BUILDINGID_FLOOR, BF_Model_allwaps_pred_val$predictions)

metrics_bf[nrow(metrics_bf) + 1, ] <- c(BF_Model_allwaps_cm_train$overall[1:2], "AllWaps", "Training")
metrics_bf[nrow(metrics_bf) + 1, ] <- c(BF_Model_allwaps_cm_val$overall[1:2], "AllWaps", "Validation")

#### Model to predict the Building-floor based on the id's of the three WAPs for which
#### the highest signal is captured

BF_ModelTop3_ranger <- ranger::ranger(BUILDINGID_FLOOR ~., trainingData[, c("TopWap1", "TopWap2", "TopWap3",
                                                                            "BUILDINGID_FLOOR")])

BF_ModelTop3_ranger_cm_train <- confusionMatrix(trainingData$BUILDINGID_FLOOR, BF_ModelTop3_ranger$predictions)

BF_ModelTop3_ranger_pred_val <- predict(BF_ModelTop3_ranger, validationData[, c("TopWap1", "TopWap2", "TopWap3")])


BF_ModelTop3_ranger_cm_val   <- confusionMatrix(validationData$BUILDINGID_FLOOR, BF_ModelTop3_ranger_pred_val$predictions) 

metrics_bf[nrow(metrics_bf) + 1, ] <- c(BF_ModelTop3_ranger_cm_train$overall[1:2], "Top3Waps", "Training")
metrics_bf[nrow(metrics_bf) + 1, ] <- c(BF_ModelTop3_ranger_cm_val$overall[1:2], "Top3Waps", "Validation")

#### Plotting error metrics for building-floor    
metrics_bf[, c(1, 2)] <- mapply(as.numeric, metrics_bf[, c(1, 2)])

ggplot(metrics_bf, aes(x = Set, y = Accuracy, fill = Set)) + geom_col() + facet_wrap( ~Model) +
  ggtitle("Accuracy of models for predicting building-floor")
ggplot(metrics_bf, aes(x = Set, y = Kappa, fill = Set)) + geom_col() + facet_wrap( ~Model) +
  ggtitle("Kappa of models for predicting building-floor") 

#### latitude prediction ####
metrics_lat <- metrics_lat <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(metrics_lat) <- c("RMSE", "Rsquared", "MAE", "Model", "Set")

set.seed(123)

Latitude_Model <- ranger(LATITUDE ~., data = trainingData[, c(WAPs, "LATITUDE")])

### 
Latitude_Model_pred_train_metrics <- postResample(Latitude_Model$predictions, trainingData$LATITUDE)

Latitude_Model_pred_val <- predict(Latitude_Model, validationData[, c(WAPs)])

### 
Latitude_Model_pred_val_metrics <- postResample(Latitude_Model_pred_val$predictions, validationData$LATITUDE)

metrics_lat[nrow(metrics_lat) + 1, ] <- c(Latitude_Model_pred_train_metrics[1:3], "AllWaps", "Training")
metrics_lat[nrow(metrics_lat) + 1, ] <- c(Latitude_Model_pred_val_metrics[1:3], "AllWaps", "Validation")

pabserror_split(Latitude_Model_pred_val$predictions, validationData$LATITUDE,
                validationData$BUILDINGID, "LATITUDE")


##### longitude prediction ####
metrics_lon <- metrics_lon <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(metrics_lon) <- c("RMSE", "Rsquared", "MAE", "Model", "Set")

set.seed(123)

Longitude_Model <- ranger(LONGITUDE ~., data = trainingData[, c(WAPs, "LONGITUDE")])

###
Longitude_Model_metrics_train <- postResample(Longitude_Model$predictions, trainingData$LONGITUDE)

Longitude_Model_pred_val <- predict(Longitude_Model, validationData[, c(WAPs)])

Longitude_Model_metrics_val <- postResample(Longitude_Model_train_metrics_pred_val$predictions, validationData$LONGITUDE)

#lon errors
metrics_lon[nrow(metrics_lon) + 1, ] <- c(Longitude_Model_metrics_train[1:3], "AllWaps", "Training")
metrics_lon[nrow(metrics_lon) + 1, ] <- c(Longitude_Model_metrics_val[1:3], "AllWaps", "Validation")


pabserror_split(Longitude_Model_pred_val$predictions, validationData$LONGITUDE,
                validationData$BUILDINGID, "LONGITUDE")





