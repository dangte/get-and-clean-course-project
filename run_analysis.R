# This program is used for Course Project Data Cleaning (from 1th Sep to 29th Sep, 2014)
# Run under Microsoft Windows, 
# If you use other system, please change a little bit forward-backward slash "\", "/"
# Data set download from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# Unzip data set to your working directory
# The folder path should be  "Your_working_directory/UCI Har Dataset/" 
# Copy this source file into Your_working_directory
# After source it by:  source('Your_working_directory/run_analysis.R')
# use: run_analysis("Your_working_directory") 
#=======================================================================================
# Step 1: a 10299 x 563 data frame after merge "Test" data set and "Train" data set 
# 563 columns: "Subject", "ActivityDescription", 561 features column
# 10299 rows: 2947 of Test; 7352 of Train data
#======================================================================================
# Step 5: write a 180 x 68 data frame to file "dataset_analysis.txt"
 
run_analysis <- function(workDirStr){
    setwd(workDirStr)
    library(plyr)
    all_features <- c() # array of features only (for step 5)
    features_mean_std <- data.frame() # array of mean() & std() (for step 5)
    sub_act_line <- c() # combine "Subject" and "ActivitiesDescription" in a line (for step 5)
    sub_act_array <- c() # vector of 180 pair "Subject" and "ActivitiesDescription" (for step 5)
    sub_act <- data.frame() # convert from vector to a 180 x 2 data frame (for step 5)
    result <- data.frame() # the final data frame for writing to file (for step 5)
    
    # Read data from file and combine "Subject-ActivityDescription-561 features" of test data set
    test_label <- read.table("./UCI Har Dataset/test/y_test.txt", sep  = "")
    test_data <- read.table("./UCI Har Dataset/test/X_test.txt", sep ="")
    test_subject <- read.table("./UCI Har Dataset/test/subject_test.txt", sep = "")
    test <- cbind(test_subject,test_label)
    test <- cbind(test, test_data)
    #Rename column 1, 2
    colnames(test)[1] <- "Subject"
    colnames(test)[2] <- "ActivityDescription"
    
    # Read data from file and combine "Subject-ActivityDescription-561 features" of train data set
    train_label <- read.table("./UCI Har Dataset/train/y_train.txt", sep  = "")
    train_data <- read.table("./UCI Har Dataset/train/X_train.txt", sep ="")
    train_subject <- read.table("./UCI Har Dataset/train/subject_train.txt", sep ="")
    train <- cbind(train_subject, train_label)
    train <- cbind(train,train_data)
    # Rename column 1, 2
    colnames(train)[1] <- "Subject"
    colnames(train)[2] <- "ActivityDescription"
    
    # Merge two data frame: "Test: 2947 x 563 data frame" and ""Train: 7352 x 563 data frame"
    all <- merge(train, test, all = TRUE)
    # End of step 1
    label <- read.table("./UCI Har Dataset/features.txt", sep = "")
    # Step 2: Extract only column with pattern "mean()" or "std() however the "meanFreq()" is also recognized
    # Save the name of all features for purpose of rename column
    label_group1 <- grep("mean()",label$V2)
    label_group2 <- grep("std()",label$V2)
    all_label <- c(label_group1, label_group2)
    label_group3 <- grep("meanFreq()",label$V2)
    # Exclude all column with meanFreq()"
    labels <- setdiff(all_label,label_group3)
    # Do the exact for 68 colum; 2 for "Subject" and "ActivityDescription"; 66 for measurement features
    all_extract <- all[c(1:2,labels+2)]
    #Step 3: Name the column 
    colnames(all_extract)[3:68] <- as.character(label$V2[labels])
    # Step 4: Replace the ActivityDescription the more meaning name for example change "1" to "WALKING"
    act <- all_extract$ActivityDescription
    char_act <- as.character(act)
    char_act <- gsub("1","WALKING",char_act)
    char_act <- gsub("2","WALKING_UPSTAIRS",char_act)
    char_act <- gsub("3","WALKING_DOWNSTAIRS",char_act)
    char_act <- gsub("4","SITTING",char_act)
    char_act <- gsub("5","STANDING",char_act)
    char_act <- gsub("6","LAYING",char_act)
    all_extract$ActivityDescription <- char_act
    # Step 5: Do the mean for each feature
    # We have 30 subject, each subject have 6 activities, in total 180 rows
    # 68 column: 2 for Subject and ActivityDescription; 66 for measurment features
    # Split the data frame to 180 sub data frames - each equivalent to 1 row in 180 rows
    all_splitter <- split(all_extract,list(all_extract$Subject,all_extract$ActivityDescription))
    split_counter <- length(all_splitter)
    # loop through each row and caculate the column mean for each
    for(i in 1:split_counter){
        all_features[[i]] <- all_splitter[[i]][,-c(1,2)]
        # calculate the column name of 66 measurement vector
        features_mean_std <- rbind(features_mean_std,colMeans(all_features[[i]]))
        # creat a 180 x 2 data frame for Subject and ActivityDescription
        sub_act_line <- c(unique(all_splitter[[i]]$Subject),unique(all_splitter[[i]]$ActivityDescription))
        sub_act_array <- c(sub_act_array,sub_act_line)
        v1 <- sub_act_array[seq_along(sub_act_array) %% 2 ==1]
        v2 <- sub_act_array[seq_along(sub_act_array) %% 2 ==0]
        sub_act <- data.frame(v1,v2)
        #join 2 data frame to become the result data frame
        result <- cbind(sub_act,features_mean_std)
    }
    # Rename the column
    colnames(result)[1:2] <- c("Subject", "ActivityDescription")
    colnames(result)[3:68] <- as.character(label$V2[labels])
    # Arrange data by Subject, each Subject has 6 activitiest from Subject 1 - Subject 30
    result <- arrange(result, as.numeric(as.character(result$Subject)))
    # Write to txt file, but I add more code for write to csv file for easier to reading the result
    write.table(result, "dataset_analysis.txt", row.names = FALSE)
    write.csv(result, "dataset_analysis.csv", row.names = FALSE)
}
# Thank for reading my whole code, contact me at dangdh@hou.edu.vn
