# Load libraries
if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
} else {
  pacman::p_load(readr, dplyr, magrittr, ggplot2, plotly, reshape2, tidyr, caret,
                 stringr, mime, ranger, gridExtra, tidyverse, svDialogs, rstudioapi)
}

# Load data
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

trainingData <- read_csv("trainingData.csv")
validationData <- read_csv("validationData.csv")

# NA
sum(is.na(trainingData))
sum(is.na(validationData))

# Pre-processing
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

# Compare training and validation observations
trainingData$Set <- "Train"
validationData$Set <- "Validation"
combinedData <- bind_rows(trainingData, validationData)
combinedData$Set <- as.factor(combinedData$Set)

p <- plot_ly(combinedData, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR,
             color = ~Set, colors = c("turquoise3", "tomato1"),
             size = 1.2) %>% add_markers() %>%
  layout(scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))
p

# Remove observations with no signal or for WAPs that dont provide signal to any observation
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

# List of WAPs that give signal to observations in both datasets
WAPs_train <- grep("WAP", names(trainingData), value = T)
WAPs_val   <- grep("WAP", names(validationData), value = T)

WAPs <- intersect(WAPs_train, WAPs_val)

rm(WAPs_train, WAPs_val)

# Update datasets
cols_to_keep_train <- c(which(colnames(trainingData) %in% WAPs),
                        which(grepl("^WAP", colnames(trainingData)) == FALSE))

cols_to_keep_val  <- c(which(colnames(validationData) %in% WAPs),
                       which(grepl("^WAP", colnames(validationData)) == FALSE))

trainingData   <- trainingData[, cols_to_keep_train]
validationData <- validationData[, cols_to_keep_val]

combinedData <- bind_rows(trainingData, validationData)
combinedData$Set <- as.factor(combinedData$Set)

# Density plot to compare train and validation
dat1 <- melt(combinedData[, c(WAPs, "Set")])

p0 <- ggplot(dat1, aes(x = value, fill = Set)) +
  geom_density(alpha = 0.2) +
  xlab("Signal value") +
  ggtitle("Densities of WAP signals for each dataset")

p0

# Phone exploration
# Phones with signal 0
rowsw0 <- which(trainingData[, c(WAPs)] == 0, arr.ind = TRUE)
Phone_test <- trainingData[rowsw0, ]
table(Phone_test$PHONEID)

# Phones with signal stronger than -30
rowsw30 <- which(trainingData[, c(WAPs)] > -30, arr.ind = TRUE)
Phone_test <- trainingData[rowsw30, ]
table(Phone_test$PHONEID)

# Phones with signal weaker than -90
rowsw90 <- which(trainingData[, c(WAPs)] < -90, arr.ind = TRUE)
Phone_test <- trainingData[rowsw90, ]
table(Phone_test$PHONEID)

trainingData$PHONEID <- NULL
validationData$PHONEID <- NULL
combinedData$PHONEID <- NULL

# Relocate very good and very bad signals
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

# Create BUILDINGID_FLOOR feature
build_floor <- function(df) {
  df <- unite(df, BUILDINGID_FLOOR, c("BUILDINGID", "FLOOR"),
              sep = "_", remove = FALSE)
  df$BUILDINGID_FLOOR <- as.factor(df$BUILDINGID_FLOOR)
  return(df)
}

trainingData   <- build_floor(trainingData)
validationData <- build_floor(validationData)

# Getting a list of WAPs that give strong signals to many BUILDINGID and BUILDINGID_FLOOR
# First step: Finding out for each observation, which is the WAP that gives
# the strongest signal
HighestWap_train <- names(trainingData)[apply(trainingData[, c(WAPs)], 1, which.max)]

# Second step: For each observation we find out in which BUILDINGID_FLOOR the 
# observation was recorded
Highest_loc_train <- data.frame("WAP" = HighestWap_train,
                                "BFLocation" = trainingData$BUILDINGID_FLOOR)

# Third step: We find out for each WAP what was the most frequent BUILDINGID_FLOOR
# in which the highest signal was recorded for it and we store it on a dataframe.
multiple <- c()
for ( i in unique(Highest_loc_train$WAP) ) {
  multiple <- rbind(multiple,
                    c(i, names(which.max
                               (table(Highest_loc_train
                                               [which(Highest_loc_train
                                                      $WAP == i), ]$BFLocation)))))
}

multiple <- as.data.frame(multiple)
colnames(multiple) <- c("WAP", "Most_Frequent_location")
multiple$Most_Frequent_location <- factor(multiple$Most_Frequent_location,
                                          levels = levels(trainingData$BUILDINGID_FLOOR))

# Taking that position as a prediction of the WAP location
Highest_loc_train$Prediction <-
  sapply(Highest_loc_train$WAP,
         function(x) multiple[which(multiple$WAP == x), ]$Most_Frequent_location)

# Do the same for BUILDINGID
# In order to do this we only need to get the first digit of the predicted BUILDINGID_FLOOR
Highest_loc_train$BLocation <- str_sub(Highest_loc_train$BFLocation, start = 1, end = 1)
Highest_loc_train$BPrediction <- str_sub(Highest_loc_train$Prediction, start = 1, end = 1)

Highest_loc_train$BLocation <- as.factor(Highest_loc_train$BLocation)
Highest_loc_train$BPrediction <- as.factor(Highest_loc_train$BPrediction)

# List of bad WAPs for BUILDINGID and BUILDINGID_FLOOR
Highest_loc_train$BFCorrect <- Highest_loc_train$BFLocation == Highest_loc_train$Prediction
BadWAPsBF <- unique(Highest_loc_train[which(Highest_loc_train$BFCorrect == FALSE), ]$WAP)

Highest_loc_train$BCorrect <- Highest_loc_train$BLocation == Highest_loc_train$BPrediction
BadWAPsB <- unique(Highest_loc_train[which(Highest_loc_train$BCorrect == FALSE), ]$WAP)

# Plot bad BUILDINGID WAPs
for (i in BadWAPsB) {
  x <- trainingData[[i]]
  x[x == -100] <- NA
  plot(trainingData$BUILDINGID_FLOOR, x, ylim = c(-100, -25), main = i,
       xlab = "BUILDINGID_FLOOR", ylab = "Signal value")
}

for (i in BadWAPsB) {
  x <- trainingData[[i]]
  x[x == -100] <- NA
  plot(density(x, na.rm = TRUE), main = i, xlab = "Signal value")
}

# Remove the bad BUILDINGID WAPs from both datasets
trainingData <- trainingData[, !(colnames(trainingData) %in% BadWAPsB), drop = FALSE]
validationData <- validationData[, !(colnames(validationData) %in% BadWAPsB), drop = FALSE]

WAPs_train <- grep("WAP", names(trainingData), value = T)
WAPs_val <- grep("WAP", names(validationData), value = T)

# Since we removed WAPs, we update the WAP list
WAPs <- intersect(WAPs_train, WAPs_val)

# Function that adds three new variables with the WAP names of the 3 strongest signals
# received in every observation
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

# Normalize
# Since we removed some WAPs we will check again for rows with no signals and remove them
drop_null_value_rows <- function(df) {
  df[df == -100] <- NA
  df <- drop_allna_rows(df)
  df[is.na(df)] <- -100
  return(df)
}

trainingData <- drop_null_value_rows(trainingData)
validationData <- drop_null_value_rows(validationData)

# Normalize the signals of every row between 0 and 1
normalize <- function(df) {
  data.frame(t(apply(df[WAPs], 1,
                     function(x) (x - min(x)) / (max(x) - min(x)))),
             df[, !colnames(df) %in% WAPs])
  return(df)
}

trainingData <- normalize(trainingData)
validationData <- normalize(validationData)
