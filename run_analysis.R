run_analysis <- function() {
  library(dplyr)
  library(reshape2)
  
  ## Reads features.
  X_train <- read.table("train/X_train.txt")
  X_test <- read.table("test/X_test.txt")
  ## Reads subjects.
  subject_train <- read.table("train/subject_train.txt")
  subject_test <- read.table("test/subject_test.txt")
  ## Read labels/activities.
  y_train <- read.table("train/y_train.txt")
  y_test <- read.table("test/y_test.txt")
  
  ## Merges the training and the test sets to create one data set.
  X <- rbind(X_train, X_test)
  subject <- rbind(subject_train, subject_test)
  y <- rbind(y_train, y_test)
  
  ## Reads names of measurements.
  features <- read.table("features.txt")
  
  ## Extracts only the measurements on the mean and standard deviation for each
  ## measurement.
  std_features <- grep("std\\(\\)", features$V2)
  mean_features <- grep("mean\\(\\)", features$V2)
  X <- X[, c(mean_features, std_features)]
  
  ## Reads activity references.
  activity_labels <- read.table("activity_labels.txt")
  ## Finds labels for activities.
  activity <-
    sapply(y$V1, function(i) {
      as.character(activity_labels[activity_labels$V1 == i, 2])
    })
  
  ## Combines features, subjects and activities.
  data <- cbind(X, subject, activity)
  
  ## Appropriately labels the data set with descriptive variable names.
  vars <- as.vector(features[c(mean_features, std_features), 2])
  vars <- gsub("\\(|\\)", "", vars)
  vars <- gsub("-", "_", vars)
  colnames(data) <- c(vars, "subject", "activity")
  
  ## Reshapes the data set with the combination of subject, activity as id.
  moltenData <- melt(data, id.vars = c("subject", "activity"))
  
  ## Casts the molten the data frame to calculate mean of each variable for
  ## each activity and each subject.
  castData <- dcast(moltenData, subject + activity ~ variable, mean)
  
  ## Creates a txt file with tidy data.
  write.table(castData, file = "tidy_data.txt", row.names = FALSE)
  
}