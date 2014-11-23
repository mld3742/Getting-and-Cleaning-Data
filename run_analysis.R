## run_analysis.R
## Data Science Specialization
## Course 3 Course Project
##
## Purpose:
## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

rm(list = ls())
gc()
library(data.table)

## Setting working directory, Evaluator please change the working directory 
## to test the code
setwd("/Users/Mark/Dropbox/Coursera/Data Science - Course 3/Week3/Getting-and-Cleaning-Data")

###==========Please do not chnage below==================================

## Part 4 - Giving appropriate variable name
## Assigning the name from the "feature.txt" file
## Reading feature data, which will be used to assign the col name to test and train data
feature <- data.table(read.table("./UCI HAR Dataset/features.txt",
                                 sep="", header = F, stringsAsFactors = F))
## Subsetting second column of feature data table, which will be used as column names
## for training and testing data
colnames <- feature[, V2]
## Removing pancutations with dot so as to make proper column name
colnames <- gsub("[[:punct:]]", ".", colnames)
## Removing multiple dots from the colnames
colnames <- gsub("...","",colnames,fixed = T)
colnames <- gsub("..","",colnames,fixed = T)
##==================================================================================
## Reading traning data set and assigning column names on the basis of feature data
train_data <- data.table(read.table("./UCI HAR Dataset/train/X_train.txt",
                                    sep = "", header = F, stringsAsFactors = F,
                                    col.names = colnames))

train_subject <- data.table(read.table("./UCI HAR Dataset/train/subject_train.txt",
                                       sep ="", header = F, stringsAsFactors = F,
                                       col.names = "subject"))
train_label <- data.table(read.table("./UCI HAR Dataset/train/y_train.txt",
                                     sep ="", header = F, stringsAsFactors = F,
                                     col.names = "label"))

## Combining the subject and label infomration with training data
train <- cbind(train_subject,train_label,train_data)
## Reading test data
test_data <- data.table(read.table("./UCI HAR Dataset/test/X_test.txt",
                                   sep = "", header = F, stringsAsFactors = F,
                                   col.names = colnames))

test_subject <- data.table(read.table("./UCI HAR Dataset/test/subject_test.txt",
                                      sep ="", header = F, stringsAsFactors = F,
                                      col.names = "subject"))
test_label <- data.table(read.table("./UCI HAR Dataset/test/y_test.txt",
                                    sep ="", header = F, stringsAsFactors = F,
                                    col.names = "label"))
## Combining the subject and label infomration with test data
test <- cbind(test_subject,test_label, test_data)
###================================================================================

## Part 1- Merging Test and Training Data 

merged_data <- rbind(train,test)
##=================================================================================

## part 2 - Extracting columns which have either mean or standard deviation value

mean_col      <- grep("mean",colnames) 
std_col       <- grep("std", colnames) 
mean_std_col  <- c(mean_col, std_col)
needed_col    <- c("subject","label",colnames[mean_std_col])
mean_std_data <- merged_data[, needed_col, with = F]
##================================================================================

## Part 3 - labeling with appropriate activity
## reading activity label

activity_label <- data.table(read.table("./UCI HAR Dataset/activity_labels.txt",
                                        sep ="", header = F, stringsAsFactors = F,
                                        col.names = c("label", "activity_name")))

labeled_data <- merge(mean_std_data, activity_label, by = "label", all.x = T)


###==================================================================================
## Part 5- Creates a second, independent tidy data set with the average of 
## each variable for each activity and each subject

all_mean_data <- merged_data[,lapply(.SD,mean), by = c("subject", "label"),
                             .SDcols = 3:563]
## Appropriately naming the tiday data set
colnames_2 <- colnames(all_mean_data)
colnames_2[3:563] <- paste(colnames_2[3:563],"mean",sep="_")
setnames(all_mean_data,colnames(all_mean_data) , colnames_2)

tidy_data <- copy(all_mean_data)
setkeyv(tidy_data, c("subject", "label"))
write.table(tidy_data,"./tidy_data.txt", sep =" ", row.names = F)