##
# Getting and cleaning data - Course Project
# Paul Walker  12/20/2014
##
##

library(plyr);
# Helper functions
mapActivityNames <-function (name) {
    switch(name,
        '1' = 'WALKING',
        '2' = 'WALKING_UPSTAIRS',
        '3' = 'WALKING_DOWNSTAIRS',
        '4' = 'SITTING',
        '5' = 'STANDING',
        '6' = 'LAYING'
    );
}

##
# (1) Merge training and test sets to create one data set
# (4) Appropriately labels the data set with descriptive variable names
##
wd <- dirname(sys.frame(1)$ofile)  # script directory
features <- read.table(paste(wd,'/data/features.txt',sep=""))  #variable names
features <- sapply(features, function (x) {gsub('[()-,]', "", x); }); #remove weird characters from colnames

#test data
xtest <- read.table(paste(wd,'/data/test/X_test.txt',sep=""), col.names=features[,2]);
testSubjects <- read.table(paste(wd,'/data/test/subject_test.txt',sep=""), col.names=c("subject"));
testActivity <- read.table(paste(wd,'/data/test/y_test.txt',sep=""), col.names=c("activity"));

# update activity names
testActivity <- apply(testActivity, 1, addActivityNames);

test1 <- cbind(testSubjects, xtest); # add subject col
test2 <- cbind(testActivity, test1); # add activity col
colnames(test2)[1] <- 'activity'; # fix activity col name

# train data
xtrain <- read.table(paste(wd,'/data/train/X_train.txt',sep=""), col.names=features[,2]);
trainSubjects <- read.table(paste(wd,'/data/train/subject_train.txt',sep=""), col.names=c("subject"));
trainActivity <- read.table(paste(wd,'/data/train/y_train.txt',sep=""), col.names=c("activity"));

trainActivity <- apply(trainActivity, 1, addActivityNames);

train1 <- cbind(trainSubjects, xtrain); # add subject col
train2 <- cbind(trainActivity, train1); # add activity col
colnames(train2)[1] <- 'activity'; # fix activity col name

# entire raw dataset
complete <- rbind(test2, train2);

# (2) Extract only the measurements on the mean and standard deviation for each measurement
fields1 <- c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241, 253:254, 266:271, 294:296, 345:350, 373:375, 424:429, 452:454, 503:504, 513, 516:517, 526, 529, 539, 542:543, 552, 555:561)
fields2 <- c(1,2, sapply(fields1, function (x) {x+2})); # re-index fields to account for subject & activity fields which come first

# subset of raw dataset 
tidySet <- complete[,fields2];
byActivity <- aggregate(tidySet[,3:87], list(Activity=tidySet$activity), mean); # show avgs by activity
bySubject <- aggregate(tidySet[,3:87], list(Subject=tidySet$subject), mean); # show avgs by subject
byActivityBySubject <- aggregate(tidySet[,3:87], tidySet[,1:2], mean); # show avgs by subject by activity

# Creates a second, independent tidy data set with the average 
#     of each variable for each activity and each subject

# avg by activity
    
# avg by subject

