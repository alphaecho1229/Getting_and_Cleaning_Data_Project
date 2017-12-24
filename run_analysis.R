run_analysis <- function() {
  
  library(dplyr)
  
  # Set up directory and file names
  cur_dir <- getwd()
  
  # Assumes current directory is ./Getting_and_Cleaning_Data_Project
  file_names <- c("/activity_labels.txt", "/features.txt", 
                  "/train/X_train.txt", "/train/y_train.txt", 
                  "/train/subject_train.txt", "/test/X_test.txt", 
                  "/test/y_test.txt", "/test/subject_test.txt")
  file_names <- paste(cur_dir, "/UCI HAR Dataset", file_names, sep = "")
  
  # Load activity labels
  activity_labels <- read.table(file_names[1])
  activities <- as.character(activity_labels$V2)
  
  # Load the features and ID the target features (mean and std)
  features_tbl <- read.table(file_names[2], header = FALSE)
  target_features <- grepl("mean[^F]|std", features_tbl$V2)
  
  # Clean up the feature names
  feature_names <- features_tbl[target_features, 2]
  feature_names <- gsub("^t", "Time.", feature_names)
  feature_names <- gsub("^f", "Frequency.", feature_names)
  feature_names <- gsub("(Body)+", "Body.", feature_names)
  feature_names <- gsub("Acc", "Acceleration.", feature_names)
  feature_names <- gsub("Gyro", "Gyroscope.", feature_names)
  feature_names <- gsub("Mag", "Magnitude.", feature_names)
  feature_names <- gsub("Gravity", "Gravity.", feature_names)
  feature_names <- gsub("Jerk", "Jerk.", feature_names)
  feature_names <- gsub("-mean\\(\\)(-)*", "Mean.", feature_names)
  feature_names <- gsub("-std\\(\\)(-)*", "Std.", feature_names)
  
  feature_names <- c("Subject.ID", feature_names, "Activity")
  
  # Load the training data sets and merge
  X_train <- read.table(file_names[3], header = FALSE)[, target_features]

  y_train <- read.table(file_names[4], header = FALSE)
  # Substitute the activity number for the activity description
  for(i in 1:6) {
    y_train$V1[y_train$V1 == i] <- activities[i]
  }

  subject_train <- read.table(file_names[5], header = FALSE)

  train_data <- cbind(subject_train, X_train, y_train)
  
  # Load the test data sets and merge
  X_test <- read.table(file_names[6], header = FALSE)[, target_features]

  y_test <- read.table(file_names[7], header = FALSE)
  for(i in 1:6) {
    y_test$V1[y_test$V1 == i] <- activities[i]
  }

  subject_test <- read.table(file_names[8], header = FALSE)

  test_data <- cbind(subject_test, X_test, y_test)
  
  # Merge the data sets and rename the features
  merged_data <- rbind(train_data, test_data)
  names(merged_data) <- feature_names
  
  # Reorder the data based on subject id
  merged_data <- arrange(merged_data, Subject.ID)
  
  merged_data <- group_by(merged_data, Subject.ID, Activity) %>%
    summarize_all("mean")
  
  write.table(merged_data, "Tidy_data.txt", row.names = FALSE)
  
  
}