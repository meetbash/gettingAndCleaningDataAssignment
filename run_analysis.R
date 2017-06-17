##################################################################
#       Getting and Cleaning Data Course Project                 #
##################################################################

## Load libraries
if (!require("dplyr")) 
        library(dplyr)

## 1. Merges the training and the test sets to create one data set.

# read data
features<-read.table("./UCI HAR Dataset/features.txt")
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")

subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")

subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")

# give column names
names(activity_labels)<-c("activityId","activityName")

names(subject_test)<-"subjectId"
names(X_test)<-features[,2]
names(y_test)<-"activityId"

names(subject_train)<-"subjectId"
names(X_train)<-features[,2]
names(y_train)<-"activityId"

# combine
train<-cbind(subject_train, y_train, X_train)
test<-cbind(subject_test, y_test, X_test)
combined<-rbind(train, test)

# remove duplicate column names
combined<-combined[, !duplicated(names(combined))]


## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

extracted_mean_std_data <- combined[,grep("subjectId|activityId|mean|std",names(combined))]


## 3. Uses descriptive activity names to name the activities in the data set

descriptive_activity_data <- merge(extracted_mean_std_data, activity_labels, by="activityId")

# reorder columns so that activity name is the 3rd column now
descriptive_activity_data<-select(descriptive_activity_data, subjectId, activityId, activityName, 3:81)


## 4. Appropriately labels the data set with descriptive variable names.

names(descriptive_activity_data)<-gsub("Acc", " Accelerator", names(descriptive_activity_data))
names(descriptive_activity_data)<-gsub("Mag", " Magnitude", names(descriptive_activity_data))
names(descriptive_activity_data)<-gsub("Gyro", " Gyroscope", names(descriptive_activity_data))
names(descriptive_activity_data)<-gsub("^t", "time", names(descriptive_activity_data))
names(descriptive_activity_data)<-gsub("^f", "frequency", names(descriptive_activity_data))
names(descriptive_activity_data)<-gsub("-", " ", names(descriptive_activity_data)) 
names(descriptive_activity_data)<-gsub("\\.", " ", names(descriptive_activity_data))
names(descriptive_activity_data)<-gsub("BodyBody", "Body", names(descriptive_activity_data))
names(descriptive_activity_data)<-gsub("^\\s+|\\s+$", "", names(descriptive_activity_data)) #remove leading & trailing spaces


## 5. From the data set in step 4, creates a second, 
##    independent tidy data set with the average of each variable for each activity and each subject.

tidydata <- tbl_df(descriptive_activity_data) %>% group_by(activityId, activityName, subjectId) %>% 
        summarise_all(funs(mean))

write.table(tidydata, file="./UCI HAR Dataset/tidy.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=TRUE)







