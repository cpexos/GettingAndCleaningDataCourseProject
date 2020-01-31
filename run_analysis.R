# set dplyr library
library(dplyr)

# Set my working directory
setwd("C:/Users/Costas/datasciencecoursera/Assignment_GettingAndCleaningData_Wk4")

# download file for assignment
dataset <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", dataset)
unzip(dataset)

# file is called "UCI HAR Dataset" when unzipped

# Part 1 consists of merging the training and the test sets to create one data set
# Start with reading test data
X_testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_testData <- read.table("./UCI HAR Dataset/test/Y_test.txt")
Sub_testData <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Then read train data
X_trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_trainData <- read.table("./UCI HAR Dataset/train/Y_train.txt")
Sub_trainData <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Read in the Features list
featuresData <- read.table("./UCI HAR Dataset/features.txt")

# Read in the Activity Labels
activitylabelsData <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Merge X train/test, Y train/test and Sub train/test Data together
X_data_merged <- rbind(X_testData, X_trainData)
Y_data_merged <- rbind(Y_testData, Y_trainData)
Sub_data_merged <- rbind(Sub_testData, Sub_trainData)

# Replace column names with descriptive text
colnames(X_data_merged) <- featuresData[,2]
colnames(Y_data_merged) <- "ActivityID"
colnames(Sub_data_merged) <- "SubjectID"
colnames(activitylabelsData) <- c("ActivityID", "ActivityType")

# Merge all data into one combined table
AllDataMerged <- cbind(Sub_data_merged, Y_data_merged, X_data_merged)

# Create table with list of all column names
ColumnSelect <- colnames(AllDataMerged)

# From ColumnSelect, specifically select columns for ActivityID, SubjectID and all columns containing mean and standard deviation
ColMean_StD <- (grepl("ActivityID", ColumnSelect) |
                        grepl("SubjectID", ColumnSelect) |
                        grepl("mean..", ColumnSelect) |
                        grepl("std...", ColumnSelect)
                )

# Create Final Table with all means and standard deviations
AllMean_StD_Data <- AllDataMerged[ , ColMean_StD == TRUE]

# Replace ActivityID with ActivityType labels in AllMean_StD_Data, remove AtivityID, sort by SubjectID and ActivityType
merge(AllMean_StD_Data, activitylabelsData, by = "ActivityID", all.x = TRUE) %>% 
        select(-ActivityID) %>% 
        select("SubjectID", "ActivityType", everything()) %>% 
        arrange(SubjectID, ActivityType) -> AllMean_StD_Data_Labelled


# Create and print a final tidy dataset that contains the average of each variable for each activity and each subject
AllMean_StD_Data_Labelled %>% 
        group_by(SubjectID, ActivityType) %>% 
        summarise_all(funs(mean)) %>% 
        as.data.frame() -> TidyDataSet

write.table(TidyDataSet, file = "./TidyDataSet.txt", row.names = FALSE, col.names = TRUE)