
## File and data loading

if (!file.exists("data")) {dir.create ("data")}


fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/UCI HAR Dataset.zip")

unzip("./data/UCI HAR Dataset.zip",)

list.files("./data")

##----------------------------------

library(dplyr)

setwd("UCI HAR Dataset")

# read.table : Reading the files in a table format
features<-read.table("features.txt",colClasses = c("character"))
activity_labels<-read.table("activity_labels.txt",col.names = c("label", "Activity"))

## Test data
subject_test<-read.table("test/subject_test.txt", sep = "")
x_test<-read.table("test/X_test.txt", sep = "")
y_test<-read.table("test/y_test.txt", sep = "")

## Train data
subject_train<-read.table("train/subject_train.txt", sep = "")
x_train<-read.table("train/x_train.txt", sep = "")
y_train<-read.table("train/y_train.txt", sep = "")

#-------------------------------------------------------------------------------------------------

## 1. Merges the training and the test sets to create one data set.

# Combine R Objects by Rows or Columns: cbind(), rbind()

# merging data
test_set<-cbind(cbind(x_test,subject_test),y_test)
train_set<-cbind(cbind(x_train,subject_train),y_train)
exp_data<-rbind(test_set,train_set)

# labeling columns
names(exp_data)<-rbind(rbind(features,c(562, "Subject")), c(563,"label"))[,2]

#----------------------------------------------------------------------------------------------------


## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

# Pattern Matching and Replacement: grepl

exp_mean_std<-exp_data[,grepl("mean|std|Subject|label",names(exp_data))]

#------------------------------------------------------------------------------------------------------
## 3. Uses descriptive activity names to name the activities in the data set

# merge activity labels with experimental data

exp_mean_std_Act<-merge(exp_mean_std,activity_labels, by="label")

#-------------------------------------------------------------------------------------------------------

## 4. Appropriately labels the data set with descriptive variable names. 

# gsub(): perform replacement of the first and all matches respectively

# Removing the "()", use of "\\"

names(exp_mean_std_Act)<-gsub('\\(|\\)',"",  names(exp_mean_std_Act), perl = TRUE)
names(exp_mean_std_Act)<-gsub('Acc'," Acceleration ",  names(exp_mean_std_Act), perl = TRUE)
names(exp_mean_std_Act)<-gsub('BodyBody'," Body ",  names(exp_mean_std_Act), perl = TRUE)
names(exp_mean_std_Act)<-gsub('Gyro'," Angular Velocity ",  names(exp_mean_std_Act), perl = TRUE)
names(exp_mean_std_Act)<-gsub('tBody'," Time Body ",  names(exp_mean_std_Act), perl = TRUE)
names(exp_mean_std_Act)<-gsub('tGravity'," Time Gravity ",  names(exp_mean_std_Act), perl = TRUE)
names(exp_mean_std_Act)<-gsub('fBody'," Frequency Body ",  names(exp_mean_std_Act), perl = TRUE)
names(exp_mean_std_Act)<-gsub('Mag'," Magnitude ",  names(exp_mean_std_Act), perl = TRUE)

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable
## for each activity and each subject.

TidyData<-aggregate(. ~Subject + Activity, exp_mean_std_Act, mean)
TidyData<-TidyData[order(TidyData$Subject,TidyData$Activity),]
write.table(TidyData, file = "TidyData.txt",row.name=FALSE)
