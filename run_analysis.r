# This R script gets and performs some cleaning on human activity data, built
# from recordings of subjects performing daily activities while carrying
# smartphone. The full description of the data set is available at:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# This script `run_analysis.R` performs following operations in sequence
# 0. downloads the data from [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/index.html). if needed  
# 1. merges the training and test data sets to create one consolidated data set  
# 2. replaces `activity` values in the dataset with descriptive activity names  
# 3. extracts only the measurements (features) on the mean and standard deviation for each measurement  
# 4. appropriately labels the columns with descriptive names  
# 5. creates a second, independent tidy dataset with an average of each variable for each each activity and each subject. In other words, same type of measurements for a particular subject and activity are averaged into one value and the tidy data set contains these mean values only. The processed tidy data set is also exported as csv file.  

library(plyr)

# 0. downloads the data from [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/index.html). if needed  
download_data = function() {
  if (!file.exists("data")) {
    dir.create("data")
  }
  if (!file.exists("data/UCI HAR Dataset")) {
    # downloading the data
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    datazip="data/UCI_HAR_data.zip"
    download.file(fileURL, destfile=datazip, method="curl")
    unzip(datazip, exdir="data")
  }
}

# 1. merges the training and test data sets to create one consolidated data set  
merge_data = function() {
  # Read data
  training.x <- read.table("data/UCI HAR Dataset/train/X_train.txt")
  training.y <- read.table("data/UCI HAR Dataset/train/y_train.txt")
  training.subject <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
  test.x <- read.table("data/UCI HAR Dataset/test/X_test.txt")
  test.y <- read.table("data/UCI HAR Dataset/test/y_test.txt")
  test.subject <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
  
  # Merge
  merged.x <- rbind(training.x, test.x)
  merged.y <- rbind(training.y, test.y)
  merged.subject <- rbind(training.subject, test.subject)
  # merge train and test datasets and return
  list(x=merged.x, y=merged.y, subject=merged.subject)
}

# 3. extracts only the measurements (features) on the mean and standard deviation for each measurement  
extract_meansd = function(dataframe) {
  features <- read.table("data/UCI HAR Dataset/features.txt")
  # Find the mean and standard deviation columns
  mean.col <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
  std.col <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))
  
  # Extract them from the data
  exdataframe <- dataframe[, (mean.col | std.col)]
  colnames(exdataframe) <- features[(mean.col | std.col), 2]
  exdataframe
}

# 4. appropriately labels the columns with descriptive names  
descriptive_activities = function(dataframe) {
  colnames(dataframe) <- "activity"
  dataframe$activity[dataframe$activity == 1] = "WALKING"
  dataframe$activity[dataframe$activity == 2] = "WALKING_UPSTAIRS"
  dataframe$activity[dataframe$activity == 3] = "WALKING_DOWNSTAIRS"
  dataframe$activity[dataframe$activity == 4] = "SITTING"
  dataframe$activity[dataframe$activity == 5] = "STANDING"
  dataframe$activity[dataframe$activity == 6] = "LAYING"
  dataframe
}

bind.data <- function(x, y, subjects) {
  # Combine mean-std values (x), activities (y) and subjects into one data frame.
  cbind(x, y, subjects)
}

create.tidy.dataset = function(dataframe) {
  # Given X values, y values and subjects, create an independent tidy dataset with the average of each variable for each activity and each subject.
  tidy <- ddply(dataframe, .(subject, activity), function(x) colMeans(x[,1:60]))
  tidy
}

clean_data = function() {
  # Download data
  download_data()
  # merge training and test datasets. merge.datasets function returns a list of three dataframes: X, y, and subject
  mergeddata <- merge_data()
  # Extract only the measurements of the mean and standard deviation for each measurement
  cx <- extract_meansd(mergeddata$x)
  # Name activities
  cy <- descriptive_activities(mergeddata$y)
  # Use descriptive column name for subjects
  colnames(mergeddata$subject) <- c("subject")
  # Combine data frames into one
  combineddata <- bind.data(cx, cy, mergeddata$subject)
  # Create tidy dataset
  # 5. creates a second, independent tidy dataset with an average of each variable for each each activity and each subject.
  tidydata <- create.tidy.dataset(combineddata)
  # Write tidy dataset as csv
  write.table(tidydata, "UCI_HAR_tidydata.csv", row.names=FALSE)
}

