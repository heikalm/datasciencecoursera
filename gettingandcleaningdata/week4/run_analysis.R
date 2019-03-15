#load dplyr
library(dplyr)

#Step 1: Merges the training and the test sets to create one data set.
#merging X,Y,Subject for train and test datasets into mergeddata dataframe
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
mergeddata <- cbind(Subject, Y, X)

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
#Using dplyr SELECT subject and activity code, and only the variables containing "mean" and "std"
dat <- mergeddata %>% select(subject, code, contains("mean"), contains("std"))

#Step 3: Uses descriptive activity names to name the activities in the data set.
#replace codes with activity names from activities file
dat$code <- activities[dat$code, 2]

#Step 4: Appropriately labels the data set with descriptive variable names.
#rename variables with descriptions using gsub() 
names(dat)[2] = "Activity"
names(dat)<-gsub("Acc", "Accelerometer", names(dat))
names(dat)<-gsub("Gyro", "Gyroscope", names(dat))
names(dat)<-gsub("BodyBody", "Body", names(dat))
names(dat)<-gsub("Mag", "Magnitude", names(dat))
names(dat)<-gsub("^t", "Time", names(dat))
names(dat)<-gsub("^f", "Frequency", names(dat))
names(dat)<-gsub("tBody", "TimeBody", names(dat))
names(dat)<-gsub("-mean()", "Mean", names(dat), ignore.case = TRUE)
names(dat)<-gsub("-std()", "STD", names(dat), ignore.case = TRUE)
names(dat)<-gsub("-freq()", "Frequency", names(dat), ignore.case = TRUE)
names(dat)<-gsub("angle", "Angle", names(dat))
names(dat)<-gsub("gravity", "Gravity", names(dat))


#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# chaining group by subject and activity followed by summarize_all to get the average per subject and activity group
tidy <- dat %>%
        group_by(subject, activity) %>%
        summarize_all(list(~mean))
write.table(tidy, "tidydata.txt", row.name=FALSE)