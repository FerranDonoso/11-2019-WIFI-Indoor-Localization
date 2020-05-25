# BUILDINGID prediction
# Model that uses all the selected WAPs
WAPs <- grep("WAP", names(trainingData), value = T)

# Train set
set.seed(123);Building_Model_allwaps <-
  ranger(BUILDINGID ~., data = trainingData[, c(WAPs, "BUILDINGID")])

Building_Model_allwaps_cm_train <-
  confusionMatrix(trainingData$BUILDINGID, Building_Model_allwaps$predictions) 

# Validation set
Building_Model_allwaps_val_pred <-
  predict(Building_Model_allwaps, validationData[, c(WAPs)])

Building_Model_allwaps_cm_val <-
  confusionMatrix(validationData$BUILDINGID, Building_Model_allwaps_val_pred[[1]]) 

# Model that uses the names of the top three WAPs in every observation
predictors_train <- grep("TopW", colnames(trainingData))
predictors_train <- c(predictors_train, which(colnames(trainingData) == "BUILDINGID"))
predictors_validation <- grep("TopW", colnames(validationData))
predictors_validation <- c(predictors_validation, which(colnames(validationData) == "BUILDINGID"))

# Train set
set.seed(123);Building_Model_Top3waps <-
  ranger(BUILDINGID ~., data = trainingData[, c(predictors_train)])

Building_Model_Top3waps_cm_train <-
  confusionMatrix(trainingData$BUILDINGID, Building_Model_Top3waps$predictions)

# Validation set
Building_Model_Top3waps_val_pred <-
  predict(Building_Model_Top3waps, validationData[, c(predictors_validation)])

Building_Model_Top3waps_cm_val <-
  confusionMatrix(validationData$BUILDINGID, Building_Model_Top3waps_val_pred$predictions)

# Error metrics table for BUILDINGID
metrics_b <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(metrics_b) <- c("Accuracy", "Kappa", "Model", "Set")

metrics_b[nrow(metrics_b) + 1, ] <- c(Building_Model_allwaps_cm_train$overall[1:2],
                                      "AllWaps", "Training")
metrics_b[nrow(metrics_b) + 1, ] <- c(Building_Model_allwaps_cm_val$overall[1:2],
                                      "AllWaps", "Validation")
metrics_b[nrow(metrics_b) + 1, ] <- c(Building_Model_Top3waps_cm_train$overall[1:2],
                                      "Top3Waps", "Training")
metrics_b[nrow(metrics_b) + 1, ] <- c(Building_Model_Top3waps_cm_val$overall[1:2],
                                      "Top3Waps", "Validation")

metrics_b[, c(1, 2)] <- mapply(as.numeric,metrics_b[, c(1, 2)])
metrics_b

# Plotting error metrics for BUILDINGID
ggplot(metrics_b, aes(x = Set, y = Accuracy, fill = Set)) +
  geom_col() +
  facet_wrap( ~Model) +
  ggtitle("Accuracy of models for predicting BUILDINGID")

ggplot(metrics_b, aes(x = Set, y = Kappa, fill = Set)) +
  geom_col() +
  facet_wrap( ~Model) +
  ggtitle("Kappa of models for predicting BUILDINGID") 

# Plotting the locations where the model makes mistakes
correctness <- function(x, y){
  return(ifelse(x == y, "Correct", "Wrong"))
}

validationData$Error_BPred <- mapply(correctness, validationData$BUILDINGID,
                                     Building_Model_Top3waps_val_pred$predictions)
validationData$Error_BPred <- as.factor(validationData$Error_BPred)

p3 <- plot_ly(validationData, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR,
              color = ~Error_BPred, colors = c("green", "red"),
              size = 1.2) %>% add_markers() %>%
  layout(scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")),
         title = "Location of the BUILDINGID errors")
p3

# BUILDINGID_FLOOR prediction
# Model that uses all the selected WAPs
# Train set
set.seed(123);BF_Model_allwaps <-
  ranger(BUILDINGID_FLOOR ~.,trainingData[, c(WAPs, "BUILDINGID_FLOOR")])

BF_Model_allwaps_cm_train <-
  confusionMatrix(trainingData$BUILDINGID_FLOOR, BF_Model_allwaps$predictions)

# Validation set
BF_Model_allwaps_pred_val <-
  predict(BF_Model_allwaps, validationData[, c(WAPs)])

BF_Model_allwaps_cm_val <-
  confusionMatrix(validationData$BUILDINGID_FLOOR, BF_Model_allwaps_pred_val$predictions)

# Model that uses the names of the top three WAPs in every observation
# Train set
set.seed(123);BF_Model_Top3waps <-
  ranger(BUILDINGID_FLOOR ~.,
         trainingData[, c("TopWap1", "TopWap2", "TopWap3", "BUILDINGID_FLOOR")])

BF_Model_Top3waps_cm_train <-
  confusionMatrix(trainingData$BUILDINGID_FLOOR, BF_Model_Top3waps$predictions)

# Validation set
BF_Model_Top3waps_pred_val <-
  predict(BF_Model_Top3waps, validationData[, c("TopWap1", "TopWap2", "TopWap3")])

BF_Model_Top3waps_cm_val <-
  confusionMatrix(validationData$BUILDINGID_FLOOR, BF_Model_Top3waps_pred_val$predictions) 

# Error metrics table for BUILDINGID_FLOOR
metrics_bf <- metrics_bf <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(metrics_bf) <- c("Accuracy", "Kappa", "Model", "Set")

metrics_bf[nrow(metrics_bf) + 1, ] <- c(BF_Model_allwaps_cm_train$overall[1:2],
                                        "AllWaps", "Training")
metrics_bf[nrow(metrics_bf) + 1, ] <- c(BF_Model_allwaps_cm_val$overall[1:2],
                                        "AllWaps", "Validation")
metrics_bf[nrow(metrics_bf) + 1, ] <- c(BF_Model_Top3waps_cm_train$overall[1:2],
                                        "Top3Waps", "Training")
metrics_bf[nrow(metrics_bf) + 1, ] <- c(BF_Model_Top3waps_cm_val$overall[1:2],
                                        "Top3Waps", "Validation")

metrics_bf[, c(1, 2)] <- mapply(as.numeric, metrics_bf[, c(1, 2)])
metrics_bf

# Plotting error metrics for BUILDINGID_FLOOR
ggplot(metrics_bf, aes(x = Set, y = Accuracy, fill = Set)) +
  geom_col() +
  facet_wrap( ~Model) +
  ggtitle("Accuracy of models for predicting BUILDINGID_FLOOR")

ggplot(metrics_bf, aes(x = Set, y = Kappa, fill = Set)) +
  geom_col() +
  facet_wrap( ~Model) +
  ggtitle("Kappa of models for predicting BUILDINGID_FLOOR")

# Plotting the locations where the model makes mistakes
validationData$Error_BFPred <- mapply(correctness, validationData$BUILDINGID_FLOOR,
                                      BF_Model_Top3waps_pred_val$predictions)
validationData$Error_BFPred <- as.factor(validationData$Error_BFPred)

p4 <- plot_ly(validationData, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR,
              color = ~Error_BFPred, colors = c("green", "red"),
              size = 1.2) %>% add_markers() %>%
  layout(scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")),
         title = "Location of the BUILDINGID_FLOOR errors")
p4

# Add BUILDINGID prediction to validation
validationData$BUILDINGID <- Building_Model_Top3waps_val_pred$predictions

# LATITUDE prediction
# Model that uses all the selected WAPs
# Train set
set.seed(123);Latitude_Model <-
  ranger(LATITUDE ~., data = trainingData[, c(WAPs, "LATITUDE", "BUILDINGID")])

Latitude_Model_pred_train_metrics <-
  postResample(Latitude_Model$predictions, trainingData$LATITUDE)

# Validation set
Latitude_Model_pred_val <-
  predict(Latitude_Model, validationData[, c(WAPs, "BUILDINGID")])

Latitude_Model_pred_val_metrics <-
  postResample(Latitude_Model_pred_val$predictions, validationData$LATITUDE)

# Error metrics table for LATITUDE
metrics_lat <- metrics_lat <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(metrics_lat) <- c("RMSE", "Rsquared", "MAE", "Model", "Set")

metrics_lat[nrow(metrics_lat) + 1, ] <- c(Latitude_Model_pred_train_metrics[1:3],
                                          "AllWaps", "Training")
metrics_lat[nrow(metrics_lat) + 1, ] <- c(Latitude_Model_pred_val_metrics[1:3],
                                          "AllWaps", "Validation")
metrics_lat

# Plotting LATITUDE error distribution
pabserror_split <- function(pred, real, building, z) {
  AELAT <- abs(pred - real)
  errors <- data.frame(abs = AELAT, b = building)
  dens <- density(AELAT, na.rm = TRUE)
  df <- data.frame(x = dens$x, y = dens$y)
  probs <- c(0.75)
  quantiles <- quantile(AELAT, prob = probs)
  df$quant <- factor(findInterval(df$x, quantiles))
  p1 <- ggplot(df, aes(x, y)) + geom_line(size = 1.5) +
    geom_ribbon(aes(ymin = 0, ymax = y, fill = quant)) +
    scale_fill_brewer(guide = "none") + 
    scale_x_continuous(breaks = seq(0, 120, 20)) +
    labs(x = "Absolute Error", y = "Density", title = paste("Density of global error in", z))
  p2 <- ggplot(errors, aes(abs, fill = b)) + geom_density(aes(y = ..density..)) +
    facet_wrap( ~b) + xlab("Absolute error") + 
    ggtitle("Densities of absolute error by BUILDINGID") + 
    labs(x = "Absolute Error", y = "Density", fill = "BUILDINGID")
  p.both <- arrangeGrob(p1, p2)
  grid::grid.draw(p.both)
}

pabserror_split(Latitude_Model_pred_val$predictions, validationData$LATITUDE,
                validationData$BUILDINGID, "LATITUDE")

# Add LATITUDE prediction to validation
validationData$LATITUDE <- Latitude_Model_pred_val$predictions

# LONGITUDE prediction
# Model that uses all the selected WAPs
# Train set
set.seed(123);Longitude_Model <-
  ranger(LONGITUDE ~., data = trainingData[, c(WAPs, "LONGITUDE", "LATITUDE", "BUILDINGID")])

Longitude_Model_metrics_train <-
  postResample(Longitude_Model$predictions, trainingData$LONGITUDE)

# Validation set
Longitude_Model_pred_val <-
  predict(Longitude_Model, validationData[, c(WAPs, "LATITUDE", "BUILDINGID")])

Longitude_Model_metrics_val <-
  postResample(Longitude_Model_pred_val$predictions, validationData$LONGITUDE)

# Error metrics table for LONGITUDE
metrics_lon <- metrics_lon <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(metrics_lon) <- c("RMSE", "Rsquared", "MAE", "Model", "Set")

metrics_lon[nrow(metrics_lon) + 1, ] <- c(Longitude_Model_metrics_train[1:3],
                                          "AllWaps", "Training")
metrics_lon[nrow(metrics_lon) + 1, ] <- c(Longitude_Model_metrics_val[1:3],
                                          "AllWaps", "Validation")
metrics_lon

# Plotting LONGITUDE error distribution
pabserror_split(Longitude_Model_pred_val$predictions, validationData$LONGITUDE,
                validationData$BUILDINGID, "LONGITUDE")
