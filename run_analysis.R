# assumes the data has been downloaded and unzipped into the "c:\\samsungdata" directory on the c: drive
# also assumes the "c:\\samsungdata" directory" has test and train subdirectories

# set the working directory
setwd("c:\\samsungdata")

# captures and labels the activities
act_labels <- read.table("activity_labels.txt")
names(act_labels) <- c("activity_nbr","activity")

# combines the train and test activities
y_train <- read.table("c:\\samsungdata\\train\\y_train.txt")
y_test <- read.table("c:\\samsungdata\\test\\y_test.txt")
y <- rbind(y_train,y_test)

# combines the activity labels with the detailed activities
y_label <- merge(y,act_labels,by.x="V1",by.y="activity_nbr")

# combines the train and test subjects
subject_train <- read.table("c:\\samsungdata\\train\\subject_train.txt")
subject_test <- read.table("c:\\samsungdata\\test\\subject_test.txt")
subjects <- rbind(subject_train,subject_test)

# combines the subjects with activities and activity labels
subject_acts <- cbind(subjects,y_label)

# combines the train and test measurements
x_train <- read.table("c:\\samsungdata\\train\\x_train.txt")
x_test <- read.table("c:\\samsungdata\\test\\x_test.txt")
x <- rbind(x_train,x_test)

# combines measurements with the subjects and activities
x2 <- cbind(subject_acts,x)

# a series of substitutions to make adjustments to field names
features <- read.table("features.txt")
features2 <- sub("BodyBody", "Body", features[,2])
features3 <- sub("BodyGyro", "Gyro", features2)
features4 <- sub("Jerk", "JerkSignal", features3)
features5 <- sub("Mag", "Magnitude", features4)
features6 <- sub("fBody", "xformBody", features5)
features7 <- sub("fGyro", "xformGyro", features6)
features8 <- sub("Acc", "Acceleration", features7)
features9 <- c("subjects","activity_nbr","activity",features8)

# applies adjusted field names to combined data set
names(x2) <- features9

# creates and ordered index of columns to be kept in the final data set
# cols 1:3 are the subject, activity nbr, & name, while the 2 grep statements capture field names with mean and standard deviation
varindex <- sort(c(1:3,grep('mean', names(x2), ignore.case = TRUE, value = FALSE),grep('std', names(x2), ignore.case = TRUE, value = FALSE)))

# initializes assembly of final data set
tidydat1 <- data.frame(x2[,1:3])
startcol <- 4
endcol <- 4
nextstart <- 5
i <- 4

# loop to check sequences of indices to be included in final table
while( i < (length(varindex) + 1)) { 
        if (varindex[i+1] - varindex[i] == 1) {
                endcol <- endcol + 1
                nextstart <- nextstart + 1
                i <- i + 1
        } else {
                tidydat1 <- cbind(tidydat1, x2[,varindex[startcol]:varindex[endcol]])
                startcol <- nextstart
                endcol <- nextstart
                nextstart <- nextstart + 1
                i <- i + 1
        }
}