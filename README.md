# Getting-and-Cleaning-Data-Course-Project
## File and data loading

library(dplyr)

# Set working directory
setwd("~/Coursera/Project/UCI HAR Dataset/UCI HAR Dataset")

# read.table : Reading the files in a table format
features<-read.table("features.txt",colClasses = c("character"))
activity_labels<-read.table("activity_labels.txt",col.names = c("label", "Activity"))

## Test data
subject_test<-read.table("test/subject_test.txt", sep = "")
X_test<-read.table("test/X_test.txt", sep = "")
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

## 3. Uses descriptive activity names to name the activities in the data set

# Join two tbls together
# left_join(): return all rows from x, and all columns from x and y.
# Rows in x with no match in y will have NA values in the new columns. 
# If there are multiple matches between x and y, all combinations of the matches are returned

exp_mean_std_L<-left_join(exp_mean_std,activity_labels, by="label")

## 4. Appropriately labels the data set with descriptive variable names. 

# gsub(): perform replacement of the first and all matches respectively

# Removing the word "Body"
names(exp_mean_std_L)<-gsub("Body","",  names(exp_mean_std_L))

# Removing the "()", use of "\\"
names(exp_mean_std_L)<-gsub('\\(|\\)',"",  names(exp_mean_std_L), perl = TRUE)

#Acc = Acceleration
names(exp_mean_std_L)<-gsub("Acc","Acceleration",  names(exp_mean_std_L))

#Renaming Gyro by " Angular_Velocity "
names(exp_mean_std_L)<-gsub("Gyro"," Angular_Velocity ",  names(exp_mean_std_L))

#Renaming " Angular_Velocity Jerk"
names(exp_mean_std_L)<-gsub(" Angular_Velocity Jerk"," Angular_Acceleration ",  names(exp_mean_std_L))

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable
## for each activity and each subject.

TidyData<-aggregate(. ~Subject + Activity, exp_mean_std_L, mean)
TidyData<-TidyData[order(TidyData$Subject,TidyData$Activity),]
write.table(TidyData, file = "TidyData.txt",row.name=FALSE)

