# Load libraries
if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
} else {
  pacman::p_load(readr,dplyr,magrittr,lubridate,ggplot2,plotly,reshape2,tidyr, caret,stringr,mime,
                 ranger,e1071,gridExtra,tidyverse,svDialogs,rstudioapi)
}

# Load data
trainingData <- read_csv("C:/Users/FDL_4/OneDrive/Documentos/Ubiqum/Course/Module 4/Task 1/Ignacio/Wi-Fi-master/datasets/trainingData.csv")
validationData <- read_csv("C:/Users/FDL_4/OneDrive/Documentos/Ubiqum/Course/Module 4/Task 1/Ignacio/Wi-Fi-master/datasets/validationData.csv")

# NA
sum(is.na(trainingData))
sum(is.na(validationData))

# Pre processing ####
preproc <- function(df) {
  df$SPACEID <- NULL
  df$RELATIVEPOSITION <- NULL
  df$USERID <- NULL
  df$TIMESTAMP <- NULL
  cols <- c("FLOOR", "BUILDINGID")
  df[, cols] <- lapply(df[, cols], as.factor)
  if (sum(duplicated(df)) != 0) {
    df <- df[!duplicated(df), ]
  }
  WAP <- grep("WAP", names(df), value = T)
  replace <- function(x) ifelse(x == 100, NA, x)
  df[, c(WAP)] <- df[, c(WAP)] %>% mutate_all(replace)
  return(df)
}

trainingData   <- preproc(trainingData)
validationData <- preproc(validationData)

#lat lon plot
trainingData$Set <- "Train"
validationData$Set <- "Validation"
combinedData <- bind_rows(trainingData, validationData)
combinedData$Set <- as.factor(combinedData$Set)

p <- plot_ly(combinedData, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~Set, colors = c("cyan", "red"),
             size = 1.2) %>% add_markers() %>%
  layout(scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))
p

#look for observations with no signal or for waps that dont provide signal to any observation
drop_allna_rows <- function(df) {
  WAP <- grep("WAP", names(df), value = T)
  ind <- which(apply(df[, c(WAP)], 1, function(x) all(is.na(x))) == TRUE)
  if (length(ind) > 0){df <- df [-ind, ]}
  return(df)
}

drop_allna_cols <- function(df) {
  WAP <- grep("WAP", names(df), value = T)
  cols_to_drop <- which(apply(df[, c(WAP)], 2, function(x) all(is.na(x))) == TRUE)
  df <- df[, -cols_to_drop]
  return(df)
}

trainingData   <- drop_allna_rows(trainingData)
validationData <- drop_allna_rows(validationData)

trainingData   <- drop_allna_cols(trainingData)
validationData <- drop_allna_cols(validationData)

#### Finding out which columns corresponds to WAPS in each dataset
WAPs_train <- grep("WAP", names(trainingData), value = T)
WAPs_val   <- grep("WAP", names(validationData), value = T)

#### Getting a common set of WAPS in both datasets.
WAPs <- intersect(WAPs_train, WAPs_val)

rm(WAPs_train, WAPs_val)

#### Update datasets
cols_to_keep_train <- c(which(colnames(trainingData) %in% WAPs),
                        which(grepl("^WAP", colnames(trainingData)) == FALSE))

cols_to_keep_val  <- c(which(colnames(validationData) %in% WAPs),
                       which(grepl("^WAP", colnames(validationData)) == FALSE))

trainingData   <- trainingData[, cols_to_keep_train]
validationData <- validationData[, cols_to_keep_val]

combinedData <- bind_rows(trainingData, validationData)
combinedData$Set <- as.factor(combinedData$Set)

#density plot to compare train and validation
dat1 <- melt(combinedData[, c(WAPs, "Set")])

p0 <- ggplot(dat1, aes(x = value, fill = Set)) +
  geom_density(alpha = 0.2) +
  xlab("RSSI Signal value") +
  ggtitle("Densities of WAP signals for each dataset")

p0

# phones ####
# phones with signal 0 #phones 19 and 23 have 102 signals
rowsw0 <- which(trainingData[, c(WAPs)] == 0, arr.ind = TRUE)
Phone_test <- trainingData[rowsw0, ]
table(Phone_test$PHONEID)

# phones with signal stronger than -30 #phones 19 and 23 have have 600+ signals
rowsw30 <- which(trainingData[, c(WAPs)] > -30, arr.ind = TRUE)
Phone_test <- trainingData[rowsw30, ]
table(Phone_test$PHONEID)

# phones with signal weaker than -90 #phone 23 have 49.000+ signals, phone 13 have 14.000+ signals
rowsw90 <- which(trainingData[, c(WAPs)] < -90, arr.ind = TRUE)
Phone_test <- trainingData[rowsw90, ]
table(Phone_test$PHONEID)

trainingData$PHONEID <- NULL
validationData$PHONEID <- NULL
combinedData$PHONEID <- NULL

# relocate very good and very bad signals
replace_values <- function(df) {
  WAP <- grep("WAP", names(df), value = T)
  na_to_onehundred <- function(x) {
    ifelse(!is.na(x), ifelse(x < -90, -90, ifelse(x > -30, -30, x)), -100)
  }
  df[, c(WAP)] <- df[, c(WAP)] %>% mutate_all(na_to_onehundred)
  return(df)
}

trainingData     <- replace_values(trainingData)
validationData   <- replace_values(validationData)

# create BUILDINGID_FLOOR feature
build_floor <- function(df) {
  df <- unite(df, BUILDINGID_FLOOR, c("BUILDINGID", "FLOOR"), sep = "_", remove = FALSE)
  df$BUILDINGID_FLOOR <- as.factor(df$BUILDINGID_FLOOR)
  return(df)
}

trainingData   <- build_floor(trainingData)
validationData <- build_floor(validationData)

#### First step: Finding out for each observation, which is the WAP which gives
#### the strongest signal.
HighestWap_train <- names(trainingData)[apply(trainingData[, c(WAPs)], 1, which.max)]

#### Second step: For each observation we find out in which building_floor the 
#### observation was recorded.
Highest_loc_train <- data.frame("WAP" = HighestWap_train, "BFLocation" = trainingData$BUILDINGID_FLOOR)

#### Third step:We find out for each WAP what was the most frequent building-floor
#### in which the highest signal was recorded for it and we store if on a dataframe.
multiple <- c()
for ( i in unique(Highest_loc_train$WAP) ) {
  multiple <- rbind(multiple,
                    c(i, names(which.max(table(Highest_loc_train[which(Highest_loc_train$WAP == i), ]$BFLocation)))))
}

multiple <- as.data.frame(multiple)
colnames(multiple) <- c("WAP", "Most_Frequent_location")
multiple$Most_Frequent_location <- factor(multiple$Most_Frequent_location,
                                          levels = levels(trainingData$BUILDINGID_FLOOR))

#### Evaluating the model. In order to do that, we will append a new column 
#### to the Highest_loc_train dataframe with the prediction.
Highest_loc_train$Prediction <- sapply(Highest_loc_train$WAP, function(x) multiple[which(multiple$WAP == x), ]$Most_Frequent_location)

#### Getting the confussion Matrix of the model on the training set ####
BFPred_cm <- confusionMatrix(trainingData$BUILDINGID_FLOOR, Highest_loc_train$Prediction)
BFPred_cm

#understanding errors
correctness <- function(x, y){
  return(ifelse(x == y, "Green", "Red"))
}

trainingData$Error_BFPred <- mapply(correctness,Highest_loc_train$BFLocation,Highest_loc_train$Prediction)
trainingData$Error_BFPred <- as.factor(trainingData$Error_BFPred)

# Plotting the locations where this simple model makes the mistakes.
p1 <- plot_ly(trainingData, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~Error_BFPred, colors = c("green", "red"),
              size = 1.2) %>% add_markers() %>%
  layout(scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")),
         title = "Location of the Building_floor errors")
p1

#### In order to do this, we only need to get the first digit of the predicted
#### building-floor.

Highest_loc_train$BLocation   <- str_sub(Highest_loc_train$BFLocation, start = 1, end = 1)
Highest_loc_train$BPrediction <- str_sub(Highest_loc_train$Prediction, start = 1, end = 1)

Highest_loc_train$BLocation   <- as.factor(Highest_loc_train$BLocation)
Highest_loc_train$BPrediction <- as.factor(Highest_loc_train$BPrediction)

#### As it could be expected, this new modelhas better accuracy and kappa on the
#### training set.
#### Accuracy: 0.997, Kappa: 0.995

BPredic_cm <- confusionMatrix(Highest_loc_train$BLocation, Highest_loc_train$BPrediction)
BPredic_cm

#understanding errors
trainingData$Error_BPred <- mapply(correctness,Highest_loc_train$BLocation,Highest_loc_train$BPrediction)
trainingData$Error_BPred <- as.factor(trainingData$Error_BPred)

# Plotting the locations where this simple model makes the mistakes.
p2 <- plot_ly(trainingData, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~Error_BPred, colors = c("green", "red"),
              size = 1.2) %>% add_markers() %>%
  layout(scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")),
         title = "Location of the Building errors")
p2

#### Another interesting question is why this happens.
Highest_loc_train$BFCorrect <- Highest_loc_train$BFLocation == Highest_loc_train$Prediction
BadWAPs <- unique(Highest_loc_train[which(Highest_loc_train$BFCorrect == FALSE), ]$WAP)

trainingData <- trainingData[, !(colnames(trainingData) %in% BadWAPs), drop = FALSE]
validationData <- validationData[, !(colnames(validationData) %in% BadWAPs), drop = FALSE]

Highest_loc_train <- Highest_loc_train[which(Highest_loc_train$BFCorrect == TRUE), ]

WAPs_train <- grep("WAP", names(trainingData), value = T)
WAPs_val <- grep("WAP", names(validationData), value = T)

#### Getting a common set of WAPS in both datasets.
WAPs <- intersect(WAPs_train, WAPs_val)

#### Function to add to a dataframe the waps for which the highest signal has been
#### recorded in each observation.

topwaps <- function(df) {
  df$Top3Waps <- apply(df[, c(WAPs)], 1, function(x) 
    paste(names(head(sort(x, decreasing = TRUE), 3)), collapse = " "))
  df <- separate(df, Top3Waps, c("TopWap1", "TopWap2", "TopWap3"))
  df$TopWap1 <- factor(df$TopWap1, levels = WAPs)
  df$TopWap2 <- factor(df$TopWap2, levels = WAPs)
  df$TopWap3 <- factor(df$TopWap3, levels = WAPs)
  return(df)
}

trainingData$Top3Waps <- apply(trainingData[, c(WAPs)], 1, function(x) 
  paste(names(head(sort(x, decreasing = TRUE), 3)), collapse = " "))

trainingData <- separate(trainingData, Top3Waps, c("TopWap1", "TopWap2", "TopWap3"))

trainingData$TopWap1 <- factor(trainingData$TopWap1, levels = WAPs)
trainingData$TopWap2 <- factor(trainingData$TopWap2, levels = WAPs)
trainingData$TopWap3 <- factor(trainingData$TopWap3, levels = WAPs)

validationData$Top3Waps <- apply(validationData[, c(WAPs)], 1, function(x) 
  paste(names(head(sort(x, decreasing = TRUE), 3)), collapse = " "))

validationData <- separate(validationData, Top3Waps, c("TopWap1", "TopWap2", "TopWap3"))

validationData$TopWap1 <- factor(validationData$TopWap1, levels = WAPs)
validationData$TopWap2 <- factor(validationData$TopWap2, levels = WAPs)
validationData$TopWap3 <- factor(validationData$TopWap3, levels = WAPs)


#plot bad waps

###
#longitude then latitude
###
#longitude MAE: 3.9075783
#longitude range: 395.113
#MAE relative to range: 101.1145
395.113 / 3.9075783
#latitude MAE: 2.9017225
#latitude range: 271.0905
#MAE relative to range: 93.42399
271.0905 / 2.9017225

###
#latitude then longitude
###
#latitude MAE: 3.2175152
#latitude range: 271.0905
#MAE relative to range: 84.25461
271.0905 / 3.2175152
#longitude MAE: 3.4914148
#longitude range: 395.113
#MAE relative to range: 113.167
395.113 / 3.4914148

#lat first: lon MAE relative to range increased 12.0525 / lat MAE stays the same
101.1145 - 113.167

#lon first: lat MAE relative to range increased 9.16938 / lon MAE stays the same
84.25461 - 93.42399

#since range stays the same, and the MAE gets lower, the bigger the increase the better
